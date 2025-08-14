library(shiny)
library(tidyverse)
library(tidycensus)
library(tidygeocoder)
library(shinythemes)
library(sf)
library(purrr)

## ShinyThemes images https://rstudio.github.io/shinythemes/

ui <- navbarPage(
        "Map Legislative Districts with Schools",
        id = "main_nav",
        theme = shinytheme("sandstone"),
        
        # Tab 1
        tabPanel(
                title = "Step 1: Upload Shape Files",
                value = "step1",
                fluidPage(
                        titlePanel("👋 Welcome! This app will help you match each school in your state to its legislative district."),
                        h3("📍 First, you will find and upload your State's Legislative District Shape Files."),
                        h4("1. Click the Button Below to Go to the US Census Website"),
                        tags$a(href = "https://www.census.gov/cgi-bin/geo/shapefiles/index.php", 
                               class = "btn btn-primary", target = "_blank", "Open Census.Gov"),
                        h4("2. Choose census year: 2024 and layer type: State Legislative Districts"),
                        tags$img(src = "census_front.jpg", height = "400px"),
                        h4("3. Choose your state and download the .zip files for both chambers."),
                        h5("Note: Lower Chamber is the House, and Upper Chamber is the Senate."),
                        tags$img(src = "choosestate.jpg", height = "300px"),
                        h4("4. Upload Your zip files below, then click Next."),
                        fileInput("house_zip", "Upload House District ZIP", accept = ".zip"),
                        fileInput("senate_zip", "Upload Senate District ZIP", accept = ".zip"),
                        actionButton("go_to_step2", "Next →", class = "btn-success")
                )
        ),
        
        # Tab 2
        tabPanel(
                title = "Step 2: Upload Schools",
                value = "step2",
                fluidPage(
                        titlePanel("Upload Your School File"),
                        h4("🕵️ Be sure that your school file is a .csv file and is formated with one of the following combinations of columns:"),
                        tags$ul(
                                tags$li("'full_address', 'county', and 'zipcode'"),
                                tags$li("'street_address', 'city', 'state', 'county', and 'zipcode'")
                        ),
                        h4("⚠️️ Before uploading, rename or create data columns if necessary. If your file does not match either format, the code will fail to match appropriately."),
                        h4("📤 Upload Your School File"),
                        fileInput("school_file", "Upload CSV of Schools", accept = ".csv"),
                        uiOutput("address_validation_badge"),
                        h4("👍 If your file uploaded successfully, click Next."),
                        br(),
                        fluidRow(
                                column(6, actionButton("go_to_step1", "← Back", class = "btn-success"),
                                       actionButton("go_to_step3", "Next →", class = "btn-success"))
                        )
                )
        ),
        
        # Tab 3
        tabPanel(
                title = "Step 3: Geocode and Match Schools",
                value = "step3",
                fluidPage(
                        titlePanel("Geocode Schools and Match Them to State Legislative Districts"),
                        h4("Now that you have uploaded all three files, it is time to geocode your school addresses and match them to districts."),
                        h5("This is the longest step, and the one that is most prone to errors. If you have trouble, please reach out to emily@tnfamiliesforvaccines.org for help troubleshooting."),
                        h5("When the process is complete, you will have two files to download:"),
                        tags$ul(
                                tags$li("Matched Schools - schools_with_house_and_senate.csv - Includes all schools successfully matched with their House and Senate districts."),
                                tags$li("Unmatched Schools - unmatched_schools.csv - Includes all schools that were not able to be successfully matched with their districts.")
                        ),
                        h4("💫 Click to Run Geocoding and Matching"),
                        actionButton("run_match", "Run Matching", class = "btn-primary"),
                        br(), br(),
                        h4("✅ Preview of Matched Schools (House + Senate)"),
                        tableOutput("preview_matched_step3"),
                        fluidRow(
                                column(6, downloadButton("download_matched", "Download Matched Schools")),
                                column(6, downloadButton("download_unmatched", "Download Unmatched Schools"))
                        ),
                        br(),
                        fluidRow(
                                column(6, actionButton("go_to_step2", "← Back", class = "btn-success"),
                                       actionButton("go_to_step4", "Next →", class = "btn-success"))
                        )
                )
        ),
        
        # Tab 4
        tabPanel(
                title = "Step 4: Next Steps",
                value = "step4",
                fluidPage(
                        titlePanel("🎉 You did it! 🎉"),
                        h4("Are you ready to take the next step and match the schools with their vaccination rates and their districts?"),
                        h5("Prepare your school vaccination data:"),
                        tags$ul(
                                tags$li("Make sure that the school name column in your vaccination data file is labeled 'school_name'"),
                                tags$li("Make sure that your data file is in .csv format.")
                        ),
                        h4("📤 Upload Your School Vaccination Rates File"),
                        fileInput("rates_file", "Upload Vaccination Rates CSV"),
                        h5("After adding any of the unmatched schools that you were able to manually match with their districts, upload your schools_with_house_and_senate.csv."),   
                        h4("📤 Upload Your Matched Schools File"),
                        fileInput("schools_with_district", "Upload Matched Schools CSV"),
                        br(),
                        fluidRow(
                                column(6, actionButton("go_to_step3", "← Back", class = "btn-success"),
                                       actionButton("go_to_step5", "Next →", class = "btn-success"))
                        )
                )
        ),
        
        # Tab 5: Review & Download Merged Data
        tabPanel(
                title = "Step 5: Review & Download",
                value = "step5",
                fluidPage(
                        titlePanel("📊 Review Merged Data"),
                        h4("Merge your matched schools file with your vaccination rates file."),
                        h5("Click the button below to merge the two datasets. Then, you can download the matched file which will include all of the variables from your school vaccination rates file plus columns for the legislative districts. The unmatched file will include schools that were not found in the vaccination rates file."),
                        
                        actionButton("merge_button", "🔄 Merge Data", class = "btn-primary"),
                        br(), br(),
                        
                        fluidRow(
                                column(6,
                                       downloadButton("download_merged_step4", "⬇️ Download Merged Dataset"),
                                       br(), br(),
                                       tableOutput("preview_merged_step4")
                                ),
                                column(6,
                                       downloadButton("download_unmatched_rates", "⬇️ Download Unmatched Schools"),
                                       br(), br(),
                                       tableOutput("preview_unmatched_rates")
                                )
                        )
                )
        )
)  # closes navbarPage

server <- function(input, output, session) {
        
        # Store reactive objects
        rv <- reactiveValues()
        
        # --- Step 1: Read shapefiles for House ---
        observeEvent(input$house_zip, {
                house_dir <- file.path(tempdir(), "house")
                dir.create(house_dir, showWarnings = FALSE)
                unzip(input$house_zip$datapath, exdir = house_dir)
                shp_file <- list.files(house_dir, pattern = "\\.shp$", full.names = TRUE)
                req(length(shp_file) > 0)
                rv$house <- st_read(shp_file[1])
        })
        
        # --- Step 2: Read shapefiles for Senate ---
        observeEvent(input$senate_zip, {
                senate_dir <- file.path(tempdir(), "senate")
                dir.create(senate_dir, showWarnings = FALSE)
                unzip(input$senate_zip$datapath, exdir = senate_dir)
                shp_file <- list.files(senate_dir, pattern = "\\.shp$", full.names = TRUE)
                req(length(shp_file) > 0)
                rv$senate <- st_read(shp_file[1])
        })
        
        # --- Step 3: Run the matching process ---
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
                                        geocode(address = "full_address", method = "census", full_results = FALSE)
                        } else if (all(c("street_address", "city", "state") %in% names(schools))) {
                                geocoded_schools <- schools %>%
                                        geocode(
                                                street = "street_address",
                                                city   = "city",
                                                state  = "state",
                                                method = "census",
                                                full_results = FALSE
                                        )
                        }
                        else {
                                showNotification(
                                        "❌ Your file must include either a 'full_address' column or separate 'street_address', 'city', and 'state' columns.",
                                        type = "error"
                                )
                                return(NULL)
                        }
                        
                        # Step 3: Separate matched and unmatched
                        incProgress(0.1, detail = "Filtering geocoded results")
                        matched_schools   <- geocoded_schools %>% filter(!is.na(lat) & !is.na(long))
                        unmatched_schools <- geocoded_schools %>% filter(is.na(lat) | is.na(long))
                        
                        # Step 4: Convert to spatial points
                        incProgress(0.1, detail = "Converting to spatial points")
                        schools_sf <- st_as_sf(matched_schools, coords = c("long", "lat"), crs = 4326)
                        
                        # Ensure required ID columns exist
                        req(all(c("school_id", "school_name", "zipcode", "county") %in% names(schools_sf)))
                        
                        # Step 5: Match to Senate districts
                        incProgress(0.2, detail = "Matching Senate districts")
                        schools_sf_senate <- st_transform(schools_sf, st_crs(rv$senate))
                        within_matrix     <- st_within(schools_sf_senate, rv$senate)
                        matched_senate <- tibble(
                                school_id       = schools_sf_senate$school_id,
                                school_name     = schools_sf_senate$school_name,
                                zipcode         = schools_sf_senate$zipcode,
                                county          = schools_sf_senate$county,
                                senate_district = map_chr(within_matrix, ~ if (length(.x) > 0) rv$senate$SLDUST[.x[1]] else NA)
                        )
                        
                        # Step 6: Match to House districts
                        incProgress(0.1, detail = "Matching House districts")
                        schools_sf_house  <- st_transform(schools_sf, st_crs(rv$house))
                        within_matrix_h   <- st_within(schools_sf_house, rv$house)
                        matched_house <- tibble(
                                school_id     = schools_sf_house$school_id,
                                school_name   = schools_sf_house$school_name,
                                zipcode       = schools_sf_house$zipcode,
                                county          = schools_sf_house$county,
                                house_district = map_chr(within_matrix_h, ~ if (length(.x) > 0) rv$house$SLDLST[.x[1]] else NA)
                        )
                        
                        # Step 7: Combine results
                        incProgress(0.1, detail = "Combining results")
                        matched_combined <- matched_senate %>%
                                left_join(matched_house, by = c("school_id", "school_name", "zipcode", "county")) %>%
                                mutate(
                                        house_district  = paste("House District", house_district),
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
                        
                        output$preview_matched_step3 <- renderTable({
                                req(matched_combined)
                                head(matched_combined %>% select(school_name, county, house_district, senate_district), 10)
                        })
                })  # closes withProgress
        })  # closes observeEvent
        
        
        # --- Step navigation ---
        observeEvent(input$go_to_step1, { updateNavbarPage(session, "main_nav", selected = "step1") })
        observeEvent(input$go_to_step2, { updateNavbarPage(session, "main_nav", selected = "step2") })
        observeEvent(input$go_to_step3, { updateNavbarPage(session, "main_nav", selected = "step3") })
        observeEvent(input$go_to_step4, { updateNavbarPage(session, "main_nav", selected = "step4") })
        observeEvent(input$go_to_step5, { updateNavbarPage(session, "main_nav", selected = "step5") })
        
        
        # --- Address validation badge ---
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
        observeEvent(input$rates_file, {
                req(input$rates_file)
                rv$rates <- read_csv(input$rates_file$datapath, show_col_types = FALSE)
        })
        
        observeEvent(input$schools_with_district, {
                req(input$schools_with_district)
                rv$schools_with_district <- read_csv(input$schools_with_district$datapath, show_col_types = FALSE)
        })
        
        observeEvent(input$merge_button, {
                req(rv$rates, rv$schools_with_district)
                
                withProgress(message = "Merging vaccination rates with matched schools...", value = 0, {
                        
                        # Step 1: Check required columns
                        incProgress(0.1, detail = "Checking required columns")
                        if (!"school_name" %in% names(rv$rates) || !"school_name" %in% names(rv$schools_with_district)) {
                                showNotification("⚠️ Both files must have a 'school_name' column", type = "error")
                                return(NULL)
                        }
                        
                        # Step 2: Perform merge
                        incProgress(0.4, detail = "Merging datasets")
                        merged <- rv$schools_with_district %>%
                                inner_join(rv$rates, by = c("school_name", "county"))
                        
                        # Step 3: Identify unmatched schools
                        incProgress(0.2, detail = "Identifying unmatched schools")
                        unmatched_schools <- rv$schools_with_district %>%
                                anti_join(rv$rates, by = c("school_name", "county"))
                        
                        # Step 4: Store results
                        incProgress(0.1, detail = "Storing results")
                        rv$merged_step4 <- merged
                        rv$unmatched_rates <- unmatched_schools
                        
                        # Step 5: Setup downloads
                        incProgress(0.1, detail = "Preparing downloads")
                        output$download_merged_step4 <- downloadHandler(
                                filename = function() { "schools_with_districts_and_rates.csv" },
                                content = function(file) { write_csv(rv$merged_step4, file) }
                        )
                        
                        output$download_unmatched_rates <- downloadHandler(
                                filename = function() { "unmatched_rates_schools.csv" },
                                content = function(file) { write_csv(rv$unmatched_rates, file) }
                        )
                        
                        # Final step: Notify user
                        incProgress(0.1, detail = "Finalizing")
                        showNotification("✅ Merge complete!", type = "message")
                })
        })
        
        
        
} # closes server function

shinyApp(ui = ui, server = server)

