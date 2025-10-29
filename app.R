# -----------------------------------------------------------------------------
# |                      Researcher Database Shiny App                        |
# -----------------------------------------------------------------------------

# --- 1. Load Libraries ---
library(shiny)
library(shinyjs)
library(googlesheets4)
library(dplyr)
library(bslib)
library(stringr)

# --- 2. Authenticate and Load Data ---
service_account_json <- Sys.getenv("GCP_SERVICE_ACCOUNT_JSON")

if (nzchar(service_account_json)) {
  tmp <- tempfile(fileext = ".json")
  writeLines(service_account_json, tmp)
  gs4_auth(path = tmp)
} else {
  stop("No Google service account JSON found in environment variable")
}

sheet_url <- "https://docs.google.com/spreadsheets/d/1I-nZynbFuKnRhzMa9suqkVYhwGLlvURDNxrR_P6cf0g/edit?gid=908323980#gid=908323980"

# --- 3. Theming ---
# Adapted from the faculty profile example to use the same color scheme.
app_theme <- bs_theme(
  version = 5,
  bg = "#FFFFFF",        # white background
  fg = "#000000",        # default text color: black
  primary = "#7A0019",   # Maroon
  secondary = "#FFDE7A", # Gold
  base_font = font_google("Open Sans"),
  bootswatch = "zephyr"
)


load_data <- function() {
  # This function now includes the de-duplication logic from the example app.
  # It keeps only the most recent entry for each unique email address.
  df <- read_sheet(sheet_url) %>%
    rename(
      Timestamp = `Timestamp`,
      Email = `Email Address`,
      Name = `Name`,
      Title = `Title/Position`,
      Affiliation = `Primary Affiliation (Department/School/Center)`,
      Research_Interests = `Please briefly describe your research interests as they relate to mobile and/or wearable technology.`,
      Studies = `List 1-3 current or recent studies where you have used this mobile and wearable devices for data collection. Please provide a title or brief description for each.`,
      Populations = `What populations do you typically study using these devices?`,
      Devices = `What types of mobile or wearable devices have you used in your research? (Check all that apply)`,
      Software = `What supporting software systems and platforms have you used to obtain data or deliver mobile interventions? (Check all that apply)`,
      Storage = `Where do you currently store the data collected from these devices? (Check all that apply)`
    ) %>%
    select(Timestamp, Email, Name, Title, Affiliation, Research_Interests, Studies, Populations, Devices, Software, Storage) %>%
    mutate(across(everything(), as.character)) %>%
    # Convert timestamp and de-duplicate
    mutate(Timestamp = as.POSIXct(Timestamp)) %>%
    arrange(desc(Timestamp)) %>%
    distinct(Email, .keep_all = TRUE)
  
  return(df)
}

# Load the data
researcher_data <- load_data()

# This function now splits by commas, but ignores commas inside parentheses.
get_filter_choices <- function(column_data) {
  all_responses <- na.omit(unique(column_data))
  
  choices <- str_split(all_responses, ",(?![^()]*\\))") %>%
    unlist() %>%
    str_trim() %>%
    unique() %>%
    sort()
  
  return(choices[choices != ""])
}

device_choices <- get_filter_choices(researcher_data$Devices)
software_choices <- get_filter_choices(researcher_data$Software)
storage_choices <- get_filter_choices(researcher_data$Storage)
population_choices <- get_filter_choices(researcher_data$Populations)


# --- 5. Helper Functions ---
highlight_terms <- function(text, terms) {
  if (is.null(terms) || length(terms) == 0 || nchar(terms[1]) == 0 || is.na(text)) {
    return(text)
  }
  
  pattern <- paste0("\\b(", paste(str_escape(terms), collapse = "|"), ")\\b")
  
  highlighted_text <- str_replace_all(
    text,
    regex(pattern, ignore_case = TRUE),
    "<strong>\\1</strong>"
  )
  
  return(highlighted_text)
}

# --- MODIFICATION: Renamed to create_researcher_panel, returns an accordion_panel ---
create_researcher_panel <- function(row, keyword_terms, device_terms, software_terms, storage_terms, population_terms) {
  
  # Highlight keyword terms
  research_interests_html <- HTML(highlight_terms(row$Research_Interests, keyword_terms))
  studies_html <- HTML(highlight_terms(row$Studies, keyword_terms))
  
  # Highlight filter terms
  populations_html <- HTML(highlight_terms(row$Populations, c(keyword_terms, population_terms)))
  devices_html <- HTML(highlight_terms(row$Devices, c(keyword_terms, device_terms)))
  software_html <- HTML(highlight_terms(row$Software, c(keyword_terms, software_terms)))
  storage_html <- HTML(highlight_terms(row$Storage, c(keyword_terms, storage_terms)))
  
  # --- This is the title of the accordion button (what was the card_header) ---
  panel_title <- tagList(
    h3(row$Name), 
    p(strong(row$Title), style = "color: #FFDE7A; margin-bottom: 0.2px"),
    p(em(row$Affiliation), style = "color: #FFDE7A; margin-bottom: 0.2px"),
    p(row$Email, style = "color: #FFDE7A; margin-bottom: 5px")
  )
  
  # --- This is the body that expands/collapses (what was the card_body) ---
  panel_body <- tagList(
    h5("🔎 Research Interests"), p(research_interests_html),
    h5("📋 Current/Recent Studies"), p(studies_html),
    h5("👥 Populations Studied"), p(populations_html),
    h5("⚙️ Technologies Used"),
    tags$ul(
      tags$li(strong("Devices: "), devices_html),
      tags$li(strong("Software: "), software_html),
      tags$li(strong("Storage: "), storage_html)
    )
  )
  
  # --- Return an accordion_panel ---
  accordion_panel(
    title = panel_title,
    value = row$Email, # Use email for a stable unique ID
    panel_body
  )
}


# =============================================================================
# |                                   UI                                      |
# =============================================================================
ui <- fluidPage(
  theme = app_theme,
  useShinyjs(),
  
  # --- MODIFICATION: Added CSS for accordion styling ---
  tags$style(HTML("
    .shiny-options-group label { 
      font-size: 0.9rem; /* 1rem is the default, 0.9rem is 90% */
    }
    
    /* Style the accordion header to look like the old maroon card header */
    .accordion-button {
      background-color: #7A0019 !important;
      color: white !important;
    }
    
    /* Style the text inside the maroon header */
    .accordion-button h3 {
      color: white !important;
      margin-bottom: 0.5rem;
    }
    .accordion-button p {
      margin-bottom: 0.2rem;
    }
    
    /* Make sure the gold text stays gold */
    .accordion-button p strong, .accordion-button p em, .accordion-button p {
       color: #FFDE7A !important;
    }
    
    /* Style the expand/collapse icon to be white */
    .accordion-button::after {
      filter: brightness(0) invert(1);
    }
    .accordion-button:not(.collapsed)::after {
      filter: brightness(0) invert(1);
    }
    
    /* Style the accordion body to look like the old card body */
    .accordion-body {
      background-color: white;
      color: black;
      border: 1px solid #ddd;
    }
    
    /* Add a margin between accordion items */
    .accordion-item {
      margin-bottom: 1rem;
      border: none; /* Remove default accordion border */
    }
  ")),
  
  titlePanel("UMN Mobile & Wearable Technology Researcher Directory"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      textInput("keyword_search", "⌨️ Keyword Search", placeholder = "e.g., mental health, Fitbit"),
      br(),
      card(
        card_header("👥 Populations Studied", style = "background-color: #FFDE7A; color: black;"),
        card_body(
          checkboxGroupInput("population_filter", label = NULL, choices = population_choices),
          fluidRow(
            column(6, actionButton("select_all_populations", "Select All", style = "font-size:10pt")),
            column(6, actionButton("select_none_populations", "Select None", style = "font-size:10pt"))
          )
        )
      ),
      br(),
      card(
        card_header("📱 Device Type(s)", style = "background-color: #FFDE7A; color: black;"),
        card_body(
          checkboxGroupInput("device_filter", label = NULL, choices = device_choices),
          fluidRow(
            column(6, actionButton("select_all_devices", "Select All", style = "font-size:10pt")),
            column(6, actionButton("select_none_devices", "Select None", style = "font-size:10pt"))
          )
        )
      ),
      br(),
      card(
        card_header("💻 Software/Platform(s)", style = "background-color: #FFDE7A; color: black;"),
        card_body(
          checkboxGroupInput("software_filter", label = NULL, choices = software_choices),
          fluidRow(
            column(6, actionButton("select_all_software", "Select All", style = "font-size:10pt")),
            column(6, actionButton("select_none_software", "Select None", style = "font-size:10pt"))
          )
        )
      ),
      br(),
      card(
        card_header("🗄️ Data Storage Location(s)", style = "background-color: #FFDE7A; color: black;"),
        card_body(
          checkboxGroupInput("storage_filter", label = NULL, choices = storage_choices),
          fluidRow(
            column(6, actionButton("select_all_storage", "Select All", style = "font-size:10pt")),
            column(6, actionButton("select_none_storage", "Select None", style = "font-size:10pt"))
          )
        )
      )
    ),
    mainPanel(
      card(
        class = "mb-3", # Add a margin-bottom for spacing
        p("This tool is designed to help you find and connect with researchers at the University of Minnesota working with mobile and wearable technology."), 
        p("You can browse profiles to learn about research interests, current studies, and the specific technologies used."),
        p("This app was created by", a("me", href="http://z.umn.edu/julianw"),  
          "using data from", 
          a("this Google Form", href = "https://forms.gle/SFnrd9kEKH4rswKZ6", target = "_blank"), ".",
          "Only those who have filled out this form have access to the tool."),
        p("I am on sabbatical in the '25-'26 academic year with the goal of building community and research infrastructure to support investigators who use mobile and wearable technology for data collection.
         If you'd like to know more about this initiative or get involved, please contact me at", strong("julianw@umn.edu"), "."),
        style = "background-color:#faebb9"
      ),
      uiOutput("researcher_profiles")
    )
  )
)

# =============================================================================
# |                                 SERVER                                    |
# =============================================================================
server <- function(input, output, session) {
  
  # --- Observers for "Select All" / "Select None" buttons ---
  observeEvent(input$select_all_populations, {
    updateCheckboxGroupInput(session, "population_filter", selected = population_choices)
  })
  observeEvent(input$select_none_populations, {
    updateCheckboxGroupInput(session, "population_filter", selected = character(0))
  })
  
  observeEvent(input$select_all_devices, {
    updateCheckboxGroupInput(session, "device_filter", selected = device_choices)
  })
  observeEvent(input$select_none_devices, {
    updateCheckboxGroupInput(session, "device_filter", selected = character(0))
  })
  
  observeEvent(input$select_all_software, {
    updateCheckboxGroupInput(session, "software_filter", selected = software_choices)
  })
  observeEvent(input$select_none_software, {
    updateCheckboxGroupInput(session, "software_filter", selected = character(0))
  })
  
  observeEvent(input$select_all_storage, {
    updateCheckboxGroupInput(session, "storage_filter", selected = storage_choices)
  })
  observeEvent(input$select_none_storage, {
    updateCheckboxGroupInput(session, "storage_filter", selected = character(0))
  })
  
  # --- Reactive filtering logic ---
  filtered_data <- reactive({
    if (nchar(input$keyword_search) == 0 &&
        is.null(input$device_filter) &&
        is.null(input$software_filter) &&
        is.null(input$storage_filter) &&
        is.null(input$population_filter)) {
      return(tibble())
    }
    
    data <- researcher_data
    
    if (nchar(input$keyword_search) > 0) {
      keyword <- tolower(input$keyword_search)
      data <- data %>%
        filter(str_detect(tolower(paste(Name, Research_Interests, Studies, Populations, Devices, Software, Storage)), keyword))
    }
    
    apply_multi_filter <- function(data, column, selections) {
      if (!is.null(selections) && length(selections) > 0) {
        safe_selections <- str_escape(selections)
        regex_pattern <- paste(safe_selections, collapse = "|")
        data <- data %>%
          filter(str_detect(coalesce(.data[[column]], ""), regex_pattern))
      }
      return(data)
    }
    
    data <- apply_multi_filter(data, "Populations", input$population_filter)
    data <- apply_multi_filter(data, "Devices", input$device_filter)
    data <- apply_multi_filter(data, "Software", input$software_filter)
    data <- apply_multi_filter(data, "Storage", input$storage_filter)
    
    return(data)
  })
  
  # --- Dynamic UI for rendering profiles ---
  output$researcher_profiles <- renderUI({
    results <- filtered_data()
    
    if (nrow(results) == 0) {
      if (nchar(input$keyword_search) > 0 || !is.null(input$device_filter) || !is.null(input$software_filter) || !is.null(input$storage_filter) || !is.null(input$population_filter)) {
        return(card(h4("No Matching Results"), p("No researchers were found with the selected criteria. Try removing or adding a filter.")))
      } else {
        return(NULL)
      }
    }
    
    keyword_terms <- unlist(str_split(input$keyword_search, "\\s+"))
    
    # --- MODIFICATION: Create panels instead of cards ---
    profile_panels <- lapply(1:nrow(results), function(i) {
      create_researcher_panel(
        row = results[i,],
        keyword_terms = keyword_terms,
        device_terms = input$device_filter,
        software_terms = input$software_filter,
        storage_terms = input$storage_filter, # <<< TYPO FIX (was input->storage_filter)
        population_terms = input$population_filter
      )
    })
    
    results_header <- h4(paste(nrow(results), "Researcher(s) Found"))
    
    # --- MODIFICATION: Wrap panels in an accordion, set open = FALSE ---
    tagList(
      results_header,
      accordion(
        !!!profile_panels, # Splice the list of panels
        id = "researcher_accordion",
        open = FALSE, # This makes them all collapsed by default
        multiple = TRUE # Allows opening more than one at a time
      )
    )
  })
}
# --- 6. Run the Application ---
shinyApp(ui = ui, server = server)