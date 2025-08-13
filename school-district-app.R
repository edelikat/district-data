library(shiny)
library(tidyverse)
library(tidycensus)
library(tigris)
library(tidygeocoder)
library(shinythemes)
library(sf)
library(purrr)

## ShinyThemes images https://rstudio.github.io/shinythemes/

ui <- navbarPage(
        "Map Legislative Districts with Schools",
        id = "main_nav",
        theme = shinytheme("sandstone"),
        
        # Tab 1: Choose State and Load District Shape Files
        tabPanel(
                title = "Step 1: Upload Shape Files",
                value = "step1",
                
                fluidPage(
                        titlePanel("👋 Welcome! This app will help you match each school in your state to it's legislative district."),
                        h4("📍 First, you will find and upload your State's Legislative District Shape Files."),
                        tags$ul(
                                tags$li("Click the Button Below to Go to the US Census Website")),
                        tags$a(href = "https://www.census.gov/cgi-bin/geo/shapefiles/index.php", class = "btn btn-primary", target = "_blank", "Open Census.Gov"),
                        tags$ul(
                                tags$li("On that page, choose census year: 2024 and layer type: State Legislative Districts")),
                        tags$img(src = "census_front.jpg", height = "250px"),
                        
                        fileInput("house_zip", "Upload House District ZIP", accept = ".zip"),
                        fileInput("senate_zip", "Upload Senate District ZIP", accept = ".zip"),
                        actionButton("go_to_step2", "Next →", class = "btn-success")
                )
                
        ),
        
        # Tab 2:Upload Schools
        tabPanel(
                title = "Step 2: Match Schools to Districts",
                value = "step2",
                
                fluidPage(
                        titlePanel("Match Schools to State Legislative Districts"),
                        
                        h4("📤 Upload Your School File"),
                        helpText("📢 Your CSV should include either:"),
                        tags$ul(
                                tags$li("A column named 'full_address' with complete address text"),
                                tags$li("OR separate columns: 'street_address', 'city', and 'state'")
                        ),
                        fileInput("school_file", "Upload CSV of Schools", accept = ".csv"),
                        uiOutput("address_validation_badge"),
                        
                        br(),
                        fluidRow(
                                column(6, actionButton("go_to_step1", "← Back", class = "btn-success"),
                                       actionButton("go_to_step3", "Next →", class = "btn-success"))
                        )
                )
        ),
        
        # Tab 3: GeoCode and Match Schools to Districts
        tabPanel(
                title = "Step 3: Geocode and Match Schools",
                value = "step3",
                
                fluidPage(
                        titlePanel("Geocode Schools and Match Them to State Legislative Districts"),
                        
                        
                        h4("💫 Click to Run Geocoding and Matching"),
                        
                        actionButton("run_match", "Run Matching", class = "btn-primary"),
                        br(), br(),
                        
                        h4("✅ Preview of Matched Schools (House + Senate)"),
                        tableOutput("matched_table"),
                        
                        fluidRow(
                                column(6, downloadButton("download_matched", "Download Matched Schools")),
                                column(6, downloadButton("download_unmatched", "Download Unmatched Schools"))
                        ),
                        
                        br(),
                        fluidRow(
                                column(6, actionButton("go_to_step2", "← Back", class = "btn-success"))
                        )
                )
        ))

server <- function(input, output, session) {
        rv <- reactiveValues()
        
        
        # Create unique subdirectory for House
        observeEvent(input$house_zip, {
                house_dir <- file.path(tempdir(), "house")
                dir.create(house_dir, showWarnings = FALSE)
                unzip(input$house_zip$datapath, exdir = house_dir)
                shp_file <- list.files(house_dir, pattern = "\\.shp$", full.names = TRUE)
                rv$house <- st_read(shp_file[1])
        })
        
        observeEvent(input$senate_zip, {
                senate_dir <- file.path(tempdir(), "senate")
                dir.create(senate_dir, showWarnings = FALSE)
                unzip(input$senate_zip$datapath, exdir = senate_dir)
                shp_file <- list.files(senate_dir, pattern = "\\.shp$", full.names = TRUE)
                rv$senate <- st_read(shp_file[1])
        })
        
        observeEvent(input$run_match, {
                req(input$school_file, rv$house, rv$senate)
                
                withProgress(message = "Matching schools to districts...", value = 0, {
                        
                        # Step 1: Read uploaded school file
                        incProgress(0.1, detail = "Reading school file")
                        schools <- read_csv(input$school_file$datapath, show_col_types = FALSE)
                        
                        # Step 2: Geocode
                        incProgress(0.2, detail = "Geocoding addresses")
                        if ("full_address" %in% names(schools)) {
                                geocoded_schools <- schools %>%
                                        geocode(address = full_address, method = "census", full_results = FALSE)
                        } else if (all(c("street_address", "city", "state") %in% names(schools))) {
                                geocoded_schools <- schools %>%
                                        geocode(
                                                street = street_address,
                                                city = city,
                                                state = state,
                                                method = "census",
                                                full_results = FALSE
                                        )
                        } else {
                                showNotification("❌ Your file must include either a 'full_address' column or separate 'street_address', 'city', and 'state' columns.", type = "error")
                                return(NULL)
                        }
                        
                        # Step 3: Separate matched and unmatched
                        incProgress(0.1, detail = "Filtering geocoded results")
                        matched_schools <- geocoded_schools %>% filter(!is.na(lat) & !is.na(long))
                        unmatched_schools <- geocoded_schools %>% filter(is.na(lat) | is.na(long))
                        
                        # Step 4: Convert to spatial points
                        incProgress(0.1, detail = "Converting to spatial points")
                        schools_sf <- st_as_sf(matched_schools, coords = c("long", "lat"), crs = 4326)  # correct WGS84 origin
                        
                        
                        
                        # Step 5: Match to districts
                        incProgress(0.2, detail = "Matching House districts")
                        # Step 1: Transform school points to match Senate CRS
                        schools_sf_senate <- st_transform(schools_sf, st_crs(rv$senate))
                        
                        # Step 2: Get index of which school falls within which district
                        within_matrix <- st_within(schools_sf_senate, rv$senate)
                        
                        # Step 3: Build a clean match table with one district per school
                        matched_senate <- tibble(
                                school_id = schools_sf_senate$school_id,
                                school_name = schools_sf_senate$school_name,
                                zipcode = schools_sf_senate$zipcode,
                                senate_district = map_chr(within_matrix, ~ if (length(.x) > 0) rv$senate$SLDUST[.x[1]] else NA)
                        )
                        
                        # Transform school points to match House CRS
                        schools_sf_house <- st_transform(schools_sf, st_crs(rv$house))
                        
                        # Get index of which school falls within which House district
                        within_matrix_house <- st_within(schools_sf_house, rv$house)
                        
                        # Build match table
                        matched_house <- tibble(
                                school_id = schools_sf_house$school_id,
                                school_name = schools_sf_house$school_name,
                                zipcode = schools_sf_house$zipcode,
                                house_district = map_chr(within_matrix_house, ~ if (length(.x) > 0) rv$house$SLDLST[.x[1]] else NA)
                        )
                        
                        # Step 6: Combine results
                        incProgress(0.1, detail = "Combining results")
                        matched_combined <- matched_senate %>%
                                left_join(matched_house, by = c("school_id", "school_name", "zipcode")) %>%
                                mutate(
                                        house_district = paste("House District", house_district),
                                        senate_district = paste("Senate District", senate_district)
                                )
                
                        
                        # Step 8: Setup download handlers
                        incProgress(0.05, detail = "Preparing downloads")
                        output$download_matched <- downloadHandler(
                                filename = function() { "schools_with_house_and_senate.csv" },
                                content = function(file) {
                                        write_csv(matched_combined, file)
                                }
                        )
                        
                        output$download_unmatched <- downloadHandler(
                                filename = function() { "unmatched_schools.csv" },
                                content = function(file) {
                                        write_csv(unmatched_schools, file)
                                }
                        )
        
                })  # closes withProgress
        })  # closes observeEvent(input$run_match)
        
        
        observeEvent(input$go_to_step1, {
                updateNavbarPage(session, "main_nav", selected = "step1")
        })
        observeEvent(input$go_to_step2, {
                updateNavbarPage(session, "main_nav", selected = "step2")
        })
        observeEvent(input$go_to_step3, {
                updateNavbarPage(session, "main_nav", selected = "step3")
        })
        
        
        output$matched_table <- renderTable({
                validate(
                        need(exists("matched_combined"), "⚠️ No matched data yet. Please run matching.")
                )
                head(matched_combined, 10)
        })
        output$column_preview <- renderPrint({
                req(input$school_file)
                df <- read_csv(input$school_file$datapath, show_col_types = FALSE)
                cols <- names(df)
                print(cols)
        })
        output$address_validation_badge <- renderUI({
                req(input$school_file)
                
                tryCatch({
                        df <- read_csv(input$school_file$datapath, show_col_types = FALSE)
                        cols <- names(df)
                        
                        if ("full_address" %in% cols) {
                                tags$span("✅ Full address column detected", class = "badge bg-success")
                        } else if (all(c("street_address", "city", "state") %in% cols)) {
                                tags$span("✅ Separate address components detected", class = "badge bg-success")
                        } else {
                                tags$span("⚠️ Missing required address columns", class = "badge bg-danger")
                        }
                }, error = function(e) {
                        tags$span("❌ Unable to read file", class = "badge bg-danger")
                })
        })
        
}  # closes server function

shinyApp(ui = ui, server = server)

