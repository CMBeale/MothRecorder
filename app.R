library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(DT)

# ==============================================================================
# 1. FILE PATHS & DATA LOADERS
# ==============================================================================

SITES_FILE <- "sites.csv"
RECORDS_FILE <- "/srv/data/MothRecorder/master_moth_records.csv"
SPECIES_FILE <- "MothSpecies.csv"
LOOKUPS_FILE <- "Mothlookups.csv"

# Load lookups directly from Mothlookups.csv
load_lookups <- function(file_path) {
  defaults <- list(
    vice_counties = c("61-South-east Yorkshire", "5-South Somerset", "62-North-east Yorkshire"),
    methods       = c("LED light", "Moth trap (MV)", "Moth trap (Actinic)", "Wine rope / Sugar", "Daytime observation"),
    sex           = c("Unrecorded", "Male", "Female"),
    stage         = c("Adult", "Larva", "Pupa", "Egg"),
    status        = c("Certain", "Considered correct", "Unconfirmed")
  )
  
  if (!file.exists(file_path)) {
    # Write initial fallback file with padded columns so rows align cleanly
    max_len <- 5
    vc_p <- c(defaults$vice_counties, rep("", max_len - length(defaults$vice_counties)))
    me_p <- c(defaults$methods, rep("", max_len - length(defaults$methods)))
    sx_p <- c(defaults$sex, rep("", max_len - length(defaults$sex)))
    st_p <- c(defaults$stage, rep("", max_len - length(defaults$stage)))
    rs_p <- c(defaults$status, rep("", max_len - length(defaults$status)))
    
    def_df <- data.frame(
      `Vice-county` = vc_p,
      `Method`      = me_p,
      `Sex`         = sx_p,
      `Stage`       = st_p,
      `Status`      = rs_p,
      check.names   = FALSE,
      stringsAsFactors = FALSE
    )
    write_csv(def_df, file_path, na = "")
  }
  
  raw_df <- read_csv(file_path, show_col_types = FALSE)
  cols <- colnames(raw_df)
  
  get_col_vals <- function(patterns, fallback) {
    for (p in patterns) {
      m <- grep(p, cols, ignore.case = TRUE, value = TRUE)
      if (length(m) > 0) {
        vals <- na.omit(raw_df[[m[1]]])
        vals <- trimws(as.character(vals))
        vals <- vals[nchar(vals) > 0]
        if (length(vals) > 0) return(unique(vals))
      }
    }
    return(fallback)
  }
  
  list(
    vice_counties = get_col_vals(c("vice", "county", "vc"), defaults$vice_counties),
    methods       = get_col_vals(c("method", "sampling"), defaults$methods),
    sex           = get_col_vals(c("^sex$"), defaults$sex),
    stage         = get_col_vals(c("^stage$", "life"), defaults$stage),
    status        = get_col_vals(c("^status$", "record"), defaults$status)
  )
}

if (!file.exists(SITES_FILE)) {
  default_sites <- data.frame(
    Site = c("Dunnington Cross, York", "Wimbleball Lake, Somerset"),
    GridRef = c("SE66695280", "SS963306"),
    ViceCounty = c("61-South-east Yorkshire", "5-South Somerset"),
    stringsAsFactors = FALSE
  )
  write_csv(default_sites, SITES_FILE)
}

load_and_clean_species <- function(file_path, records_file = RECORDS_FILE) {
  if (!file.exists(file_path)) {
    default_species <- data.frame(
      ABH_Code = c("73.001", "73.015", "70.226", "73.169"),
      Bradley_Code = c("2107", "2121", "1884", "2250"),
      Vernacular = c("Large Yellow Underwing", "Broad-bordered Yellow Underwing", "Yellow Belle", "Dark Arches"),
      Taxon = c("Noctua pronuba", "Noctua fimbriata", "Aspitates gilvaria", "Apamea monoglypha"),
      Grade = c("1", "1", "2", "1"),
      Shortcut = c("nopro, lyu", "nofim", "asgil", "apmon"),
      stringsAsFactors = FALSE
    )
    write_csv(default_species, file_path)
  }
  
  raw_df <- read_csv(file_path, show_col_types = FALSE)
  # Sanitize invalid UTF-8 bytes in species CSV
  raw_df <- raw_df %>% mutate(across(where(is.character), ~ iconv(.x, to = "UTF-8", sub = "")))
  
  cols <- colnames(raw_df)
  
  get_col <- function(patterns) {
    for (p in patterns) {
      m <- grep(p, cols, ignore.case = TRUE, value = TRUE)
      if (length(m) > 0) return(m[1])
    }
    return(NULL)
  }
  
  tax_c   <- get_col(c("^taxon$", "^scientific", "^species$"))
  vern_c  <- get_col(c("^vernacular$", "^common", "^english"))
  abh_c   <- get_col(c("^abh_code$", "^code$", "^abh$", "^abh code$"))
  brad_c  <- get_col(c("^bradley_code$", "^bradley$", "^bradley code$"))
  grad_c  <- get_col(c("^grade$"))
  short_c <- get_col(c("^shortcut$", "^key$", "^alias$"))
  
  temp <- data.frame(
    Taxon = if (!is.null(tax_c)) trimws(as.character(raw_df[[tax_c]])) else paste0("Species_", seq_len(nrow(raw_df))),
    Vernacular = if (!is.null(vern_c)) trimws(as.character(raw_df[[vern_c]])) else "",
    ABH_Code = if (!is.null(abh_c)) trimws(as.character(raw_df[[abh_c]])) else "",
    Bradley_Code = if (!is.null(brad_c)) trimws(as.character(raw_df[[brad_c]])) else "",
    Grade = if (!is.null(grad_c)) trimws(as.character(raw_df[[grad_c]])) else "1",
    Shortcut = if (!is.null(short_c)) trimws(as.character(raw_df[[short_c]])) else "",
    stringsAsFactors = FALSE
  )
  
  temp$Taxon[is.na(temp$Taxon)] <- ""
  temp$Vernacular[is.na(temp$Vernacular)] <- ""
  temp$ABH_Code[is.na(temp$ABH_Code)] <- ""
  temp$Bradley_Code[is.na(temp$Bradley_Code)] <- ""
  temp$Grade[is.na(temp$Grade)] <- "1"
  temp$Shortcut[is.na(temp$Shortcut)] <- ""
  
  temp <- temp %>% filter(nchar(Taxon) > 0)
  
  out <- temp %>%
    group_by(Taxon) %>%
    summarize(
      Vernacular = {
        v <- Vernacular[nchar(Vernacular) > 0]
        if (length(v) > 0) v[1] else Taxon[1]
      },
      ABH_Code = {
        a <- ABH_Code[nchar(ABH_Code) > 0]
        if (length(a) > 0) a[1] else ""
      },
      Bradley_Code = {
        b <- Bradley_Code[nchar(Bradley_Code) > 0]
        if (length(b) > 0) b[1] else ""
      },
      Grade = {
        g <- Grade[nchar(Grade) > 0]
        if (length(g) > 0) g[1] else "1"
      },
      Shortcut = paste(unique(Shortcut[nchar(Shortcut) > 0]), collapse = ", "),
      .groups = "drop"
    )
  
  # Strictly use preset shortcuts defined in the CSV without auto-generating extra keys
  for (i in seq_len(nrow(out))) {
    existing_keys <- unlist(strsplit(out$Shortcut[i], ",\\s*"))
    existing_keys <- existing_keys[nchar(existing_keys) > 0]
    out$Shortcut[i] <- paste(unique(existing_keys), collapse = ", ")
  }
  
  # Calculate recording frequency from master records file safely
  rec_counts <- data.frame(Taxon = character(), RecFreq = integer(), stringsAsFactors = FALSE)
  if (file.exists(records_file)) {
    rec_df <- tryCatch(read_csv(records_file, show_col_types = FALSE, col_types = cols(.default = "c")), error = function(e) NULL)
    if (!is.null(rec_df) && nrow(rec_df) > 0 && "Taxon" %in% colnames(rec_df)) {
      rec_counts <- rec_df %>%
        mutate(Taxon = iconv(Taxon, to = "UTF-8", sub = "")) %>%
        filter(!is.na(Taxon) & nchar(trimws(Taxon)) > 0) %>%
        group_by(Taxon = trimws(Taxon)) %>%
        summarize(RecFreq = n(), .groups = "drop")
    }
  }
  
  # Order species by frequency (descending), then alphabetically by Taxon for ties/0 records
  out <- out %>%
    left_join(rec_counts, by = "Taxon") %>%
    mutate(RecFreq = ifelse(is.na(RecFreq), 0, RecFreq)) %>%
    arrange(desc(RecFreq), Taxon) %>%
    select(-RecFreq)
  
  return(out)
}


BC_COLUMNS <- c(
  "Shortcut", "Code", "Vernacular", "Taxon", "Grade",
  "Location (64)", "Grid reference (12)", "Vice-county", 
  "Observer(s) (64)", "Determiner (64)", "Date (10)", "Date Validator", 
  "Abundance", "Abundance qualifier", "Sampling method", "Sex", 
  "Life stage", "Record status", "Confidential", "Comment (255)"
)

if (!file.exists(RECORDS_FILE)) {
  empty_master <- as.data.frame(matrix(ncol = length(BC_COLUMNS), nrow = 0))
  colnames(empty_master) <- BC_COLUMNS
  write_csv(empty_master, RECORDS_FILE)
}

# Helper to bind factor levels for DT inline dropdown editing
apply_factor_dropdowns <- function(df, lookups) {
  if (nrow(df) == 0) return(df)
  
  # Sex column dropdown
  sex_opts <- unique(c("", trimws(as.character(df[["Sex"]])), lookups$sex))
  sex_opts <- sex_opts[!is.na(sex_opts) & (nchar(sex_opts) > 0 | sex_opts == "")]
  df[["Sex"]] <- factor(df[["Sex"]], levels = sex_opts)
  
  # Life stage column dropdown
  stage_opts <- unique(c("", trimws(as.character(df[["Life stage"]])), lookups$stage))
  stage_opts <- stage_opts[!is.na(stage_opts) & (nchar(stage_opts) > 0 | stage_opts == "")]
  df[["Life stage"]] <- factor(df[["Life stage"]], levels = stage_opts)
  
  # Record status column dropdown
  status_opts <- unique(c("", trimws(as.character(df[["Record status"]])), lookups$status))
  status_opts <- status_opts[!is.na(status_opts) & (nchar(status_opts) > 0 | status_opts == "")]
  df[["Record status"]] <- factor(df[["Record status"]], levels = status_opts)
  
  # Abundance qualifier column dropdown
  qual_opts <- unique(c("", trimws(as.character(df[["Abundance qualifier"]])), c("Exact", "Estimate")))
  qual_opts <- qual_opts[!is.na(qual_opts) & (nchar(qual_opts) > 0 | qual_opts == "")]
  df[["Abundance qualifier"]] <- factor(df[["Abundance qualifier"]], levels = qual_opts)
  
  # Confidential column dropdown
  conf_opts <- unique(c("", trimws(as.character(df[["Confidential"]])), c("No", "Yes")))
  conf_opts <- conf_opts[!is.na(conf_opts) & (nchar(conf_opts) > 0 | conf_opts == "")]
  df[["Confidential"]] <- factor(df[["Confidential"]], levels = conf_opts)
  
  return(df)
}

# ==============================================================================
# 2. UI DESIGN
# ==============================================================================

ui <- page_navbar(
  title = "Field Moth Recorder",
  id = "main_nav",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#2c3e50"),
  
  tags$head(
    tags$style(HTML("
      /* Light highlight background ONLY for the species dropdown */
      #species-select-wrapper .selectize-dropdown .option.active,
      #species-select-wrapper .selectize-dropdown .active {
        background-color: #e2e8f0 !important; /* Soft light slate/grey background */
      }
    "))
  ),
  
  
  # --- TAB 1: SESSION SETUP ---
  nav_panel(
    title = "1. Session Setup",
    value = "tab_setup",
    icon = icon("map-marker-alt"),
    layout_sidebar(
      sidebar = sidebar(
        open = "always",
        h5("Site Selection"),
        selectInput("site_select", "Select Existing Site", choices = NULL),
        checkboxInput("is_new_site", "Add New Site", value = FALSE),
        conditionalPanel(
          condition = "input.is_new_site == true",
          textInput("new_site_name", "Site Name (Max 64 chars)", placeholder = "e.g. Dunnington Cross, York"),
          textInput("new_grid_ref", "OS Grid Ref (e.g. SE66695280)"),
          selectInput("new_vc", "Vice-County", choices = NULL)
        ),
        hr(),
        actionButton("btn_start_session", "Lock Info & Start Quick Entry", class = "btn-primary w-100")
      ),
      card(
        card_header("Session Metadata"),
        dateInput("rec_date", "Trapping Date (Trap ON Date)", value = Sys.Date()-1, format = "dd/mm/yyyy"),
        textInput("observer", "Observer(s) (Max 64 chars)", value = "A. Recorder"),
        selectInput("sampling_method", "Sampling Method", choices = NULL),
        selectInput("life_stage", "Life Stage", choices = NULL)
      )
    )
  ),
  
  # --- TAB 2: CATCH COUNTER ---
  nav_panel(
    title = "2. Quick Counter",
    value = "tab_counter",
    icon = icon("calculator"),
    fluidRow(
      column(
        width = 12, lg = 5,
        card(
          card_header("Add Moth to Current Day"),
          div(
            id = "species-select-wrapper",
            selectizeInput(
              "species_input", 
              "Search Species (Shortcut, Common, Scientific, Code):", 
              choices = NULL, 
              options = list(
                placeholder = "Type e.g. 'lyu', 'nopro', 'Yellow Belle'...",
                maxOptions = 50
              )
            )
          ),
          numericInput("box_count", "Quantity to Add (This Box/Count):", value = 1, min = 1, step = 1),
          textInput("entry_comment", "Specimen Comment (Optional)", placeholder = "e.g. worn adult, in box 3"),
          br(),
          actionButton("btn_add_count", "Add to Day Count", class = "btn-success btn-lg w-100"),
          hr(),
          actionButton("btn_preview", "Preview Export", class = "btn-info w-100")
        )
      ),
      column(
        width = 12, lg = 7,
        card(
          card_header("Current Session Tally"),
          tableOutput("current_session_table")
        )
      )
    )
  ),
  
  # --- TAB 3: REVIEW & EXPORT ---
  nav_panel(
    title = "3. Export Data",
    value = "tab_export",
    icon = icon("file-export"),
    card(
      card_header("Review & Edit Active Session Output"),
      p(class = "text-muted small", "Double-click cells to edit. Dropdowns are available for Sex, Life stage, Record status, Abundance qualifier, and Confidential."),
      div(style = "overflow-x: auto;", DTOutput("review_export_table")),
      card_footer(
        actionButton("btn_save_master", "Append to Master CSV", class = "btn-primary"),
        downloadButton("btn_download_csv", "Download Session CSV", class = "btn-outline-success")
      )
    )
  ),
  
  # --- TAB 4: MASTER DATABASE ---
  nav_panel(
    title = "4. Master Database",
    value = "tab_master",
    icon = icon("database"),
    card(
      card_header("Master Records Database (master_moth_records.csv)"),
      p(class = "text-muted small", "View, edit, or download all records saved in master_moth_records.csv."),
      div(style = "overflow-x: auto;", DTOutput("master_records_table")),
      card_footer(
        actionButton("btn_save_master_edits", "Save Master Edits", class = "btn-success"),
        downloadButton("btn_download_master_csv", "Download Master CSV", class = "btn-outline-primary")
      )
    )
  )
)

# ==============================================================================
# 3. SERVER LOGIC
# ==============================================================================

server <- function(input, output, session) {
  
  sites_df <- reactiveVal(read_csv(SITES_FILE, show_col_types = FALSE))
  species_df <- reactiveVal(load_and_clean_species(SPECIES_FILE, RECORDS_FILE))
  lookups_data <- reactiveVal(load_lookups(LOOKUPS_FILE))
  
  session_counts <- reactiveVal(data.frame(
    Taxon = character(),
    Count = integer(),
    Comment = character(),
    stringsAsFactors = FALSE
  ))
  
  export_data_val <- reactiveVal(data.frame())
  master_data_val <- reactiveVal(data.frame())
  
  # Populate Lookups into Setup Tab inputs
  observe({
    lk <- lookups_data()
    
    # Update Vice-County choices in Add Site menu from Vice-county column
    updateSelectInput(session, "new_vc", choices = lk$vice_counties)
    
    # Update Sampling Method choices from Method column, defaulting to LED light
    default_method <- lk$methods[1]
    led_match <- grep("LED", lk$methods, ignore.case = TRUE, value = TRUE)
    if (length(led_match) > 0) default_method <- led_match[1]
    
    updateSelectInput(session, "sampling_method", choices = lk$methods, selected = default_method)
    
    # Update Life stage choices from Stage column
    updateSelectInput(session, "life_stage", choices = lk$stage, selected = if ("Adult" %in% lk$stage) "Adult" else lk$stage[1])
  })
  
  observe({
    st <- sites_df()
    updateSelectInput(session, "site_select", choices = st$Site)
  })
  
  # Load Master Data Frame into memory safely as character vectors
  load_master_df <- function() {
    if (file.exists(RECORDS_FILE)) {
      df <- read_csv(RECORDS_FILE, show_col_types = FALSE, col_types = cols(.default = "c"))
      for (col in BC_COLUMNS) {
        if (!col %in% colnames(df)) df[[col]] <- ""
      }
      df <- df[, BC_COLUMNS, drop = FALSE]
      df$Abundance <- as.character(df$Abundance)
      return(apply_factor_dropdowns(df, lookups_data()))
    } else {
      empty_master <- as.data.frame(matrix(ncol = length(BC_COLUMNS), nrow = 0))
      colnames(empty_master) <- BC_COLUMNS
      return(apply_factor_dropdowns(empty_master, lookups_data()))
    }
  }
  
  observe({
    master_data_val(load_master_df())
  })
  
  # Clean display labels, full background shortcut search with explicitly mapped sort_order
  observe({
    sp <- species_df()
    req(nrow(sp) > 0)
    
    sp_options <- lapply(seq_len(nrow(sp)), function(i) {
      # Map Grade column to specific CSS hex/color values
      g_color <- switch(as.character(sp$Grade[i]),
                        "1" = "#006400",  # Dark Green
                        "2" = "#0000FF",  # Blue
                        "3" = "#FF8C00",  # Dark Orange
                        "4" = "#FF0000",  # Red
                        "#000000"         # Default Fallback (Black)
      )
      
      list(
        value = sp$Taxon[i],
        label = paste0(sp$Vernacular[i], " (", sp$Taxon[i], ")"),
        sort_order = i,
        color = g_color,
        search_str = paste(
          sp$Vernacular[i], 
          sp$Taxon[i], 
          sp$ABH_Code[i], 
          sp$Bradley_Code[i], 
          sp$Shortcut[i], 
          sep = " "
        )
      )
    })
    
    updateSelectizeInput(
      session, 
      "species_input", 
      choices = sp$Taxon,
      options = list(
        options = sp_options,
        valueField = "value",
        labelField = "label",
        searchField = c("label", "search_str"),
        sortField = list(list(field = "sort_order", direction = "asc")),
        placeholder = "Type species name, code or shortcut...",
        maxOptions = 50,
        render = I('{
          option: function(item, escape) {
            return \'<div class="option" style="color:\' + escape(item.color) + \'; font-weight: 600;">\' + escape(item.label) + \'</div>\';
          },
          item: function(item, escape) {
            return \'<div class="item" style="color:\' + escape(item.color) + \'; font-weight: 600;">\' + escape(item.label) + \'</div>\';
          }
        }')
      ),
      server = FALSE
    )
  })
  
  current_site_info <- reactive({
    if (input$is_new_site) {
      req(input$new_site_name, input$new_grid_ref)
      return(data.frame(
        Site = input$new_site_name,
        GridRef = input$new_grid_ref,
        ViceCounty = input$new_vc,
        stringsAsFactors = FALSE
      ))
    } else {
      req(input$site_select)
      sites_df() %>% filter(Site == input$site_select) %>% slice(1)
    }
  })
  
  observeEvent(input$btn_start_session, {
    if (input$is_new_site && nchar(input$new_site_name) > 0) {
      new_row <- data.frame(Site = input$new_site_name, GridRef = input$new_grid_ref, ViceCounty = input$new_vc, stringsAsFactors = FALSE)
      updated_sites <- rbind(sites_df(), new_row) %>% distinct(Site, .keep_all = TRUE)
      write_csv(updated_sites, SITES_FILE)
      sites_df(updated_sites)
      updateSelectInput(session, "site_select", choices = updated_sites$Site, selected = input$new_site_name)
      updateCheckboxInput(session, "is_new_site", value = FALSE)
    }
    
    nav_select("main_nav", "tab_counter")
    showNotification("Session metadata locked! Start counting.", type = "message")
  })
  
  observeEvent(input$btn_preview, {
    nav_select("main_nav", "tab_export")
  })
  
  observeEvent(input$btn_add_count, {
    req(input$species_input, input$box_count)
    
    selected_taxon <- trimws(input$species_input)
    add_qty <- as.integer(input$box_count)
    cmt <- trimws(input$entry_comment)
    
    curr <- session_counts()
    
    new_entry <- data.frame(
      Taxon = selected_taxon,
      Count = add_qty,
      Comment = cmt,
      stringsAsFactors = FALSE
    )
    
    updated <- rbind(curr, new_entry) %>%
      group_by(Taxon) %>%
      summarize(
        Count = sum(Count, na.rm = TRUE),
        Comment = paste(unique(Comment[nchar(Comment) > 0]), collapse = "; "),
        .groups = "drop"
      )
    
    session_counts(updated)
    
    updateNumericInput(session, "box_count", value = 1)
    updateTextInput(session, "entry_comment", value = "")
    updateSelectizeInput(session, "species_input", selected = character(0))
  })
  
  output$current_session_table <- renderTable({
    req(nrow(session_counts()) > 0)
    sp_ref <- species_df()
    
    session_counts() %>%
      left_join(sp_ref, by = "Taxon") %>%
      select(Vernacular, Taxon, Abundance = Count, Comment)
  })
  
  formatted_export_data <- reactive({
    counts <- session_counts()
    req(nrow(counts) > 0)
    
    site_data <- current_site_info()
    sp_ref <- species_df()
    date_str <- format(input$rec_date, "%d/%m/%Y")
    
    df <- counts %>%
      left_join(sp_ref, by = "Taxon") %>%
      transmute(
        `Shortcut` = Shortcut,
        `Code` = ABH_Code,
        `Vernacular` = Vernacular,
        `Taxon` = Taxon,
        `Grade` = Grade,
        `Location (64)` = str_trunc(site_data$Site, 64),
        `Grid reference (12)` = str_trunc(site_data$GridRef, 12),
        `Vice-county` = site_data$ViceCounty,
        `Observer(s) (64)` = str_trunc(input$observer, 64),
        `Determiner (64)` = "",
        `Date (10)` = date_str,
        `Date Validator` = "",
        `Abundance` = as.character(Count),
        `Abundance qualifier` = "Exact",
        `Sampling method` = input$sampling_method,
        `Sex` = "Unrecorded",
        `Life stage` = input$life_stage,
        `Record status` = "",
        `Confidential` = "No",
        `Comment (255)` = str_trunc(Comment, 255)
      )
    
    apply_factor_dropdowns(df, lookups_data())
  })
  
  # Sync formatted data to editable state ONLY when new counts are added
  observeEvent(session_counts(), {
    req(nrow(session_counts()) > 0)
    export_data_val(formatted_export_data())
  }, ignoreInit = TRUE)
  
  # Handle manual inline table edits in Tab 3 safely without factor level corruption
  observeEvent(input$review_export_table_cell_edit, {
    info <- input$review_export_table_cell_edit
    current_df <- export_data_val() %>% mutate(across(where(is.factor), as.character))
    updated_df <- DT::editData(current_df, info, rownames = FALSE)
    export_data_val(apply_factor_dropdowns(updated_df, lookups_data()))
  })
  
  # Render interactive DT table for Tab 3 (disabled sorting to keep index mapping safe)
  output$review_export_table <- renderDT({
    req(nrow(export_data_val()) > 0)
    datatable(
      export_data_val(),
      editable = TRUE,
      rownames = FALSE,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 't',
        ordering = FALSE
      )
    )
  })
  
  # Handle manual inline table edits in Tab 4 safely without factor level corruption
  observeEvent(input$master_records_table_cell_edit, {
    info <- input$master_records_table_cell_edit
    current_df <- master_data_val() %>% mutate(across(where(is.factor), as.character))
    updated_df <- DT::editData(current_df, info, rownames = FALSE)
    master_data_val(apply_factor_dropdowns(updated_df, lookups_data()))
  })
  
  # Render interactive DT table for Tab 4 (disabled sorting to keep index mapping safe)
  output$master_records_table <- renderDT({
    req(nrow(master_data_val()) > 0)
    datatable(
      master_data_val(),
      editable = TRUE,
      rownames = FALSE,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'fltip',
        ordering = FALSE
      )
    )
  })
  
  # Save session export data to master file
  observeEvent(input$btn_save_master, {
    out_df <- export_data_val()
    req(nrow(out_df) > 0)
    
    out_df <- out_df %>% mutate(across(where(is.factor), as.character))
    write_csv(out_df, RECORDS_FILE, append = TRUE)
    
    session_counts(data.frame(
      Taxon = character(), 
      Count = integer(), 
      Comment = character(), 
      stringsAsFactors = FALSE
    ))
    export_data_val(data.frame())
    master_data_val(load_master_df())
    
    # Update species dropdown order based on new master recording frequencies
    species_df(load_and_clean_species(SPECIES_FILE, RECORDS_FILE))
    
    showNotification("Records saved to master spreadsheet! Session reset.", type = "message", duration = 5)
    nav_select("main_nav", "tab_setup")
  })
  
  # Save manual edits in Master Database tab directly to master file
  observeEvent(input$btn_save_master_edits, {
    out_df <- master_data_val()
    req(nrow(out_df) > 0)
    
    out_df <- out_df %>% mutate(across(where(is.factor), as.character))
    write_csv(out_df, RECORDS_FILE)
    
    # Update species dropdown order based on modified master records
    species_df(load_and_clean_species(SPECIES_FILE, RECORDS_FILE))
    
    showNotification("Master records spreadsheet updated successfully!", type = "message", duration = 5)
  })
  
  output$btn_download_csv <- downloadHandler(
    filename = function() { paste0("moth_records_", format(input$rec_date, "%Y%m%d"), ".csv") },
    content = function(file) { 
      df <- export_data_val() %>% mutate(across(where(is.factor), as.character))
      write_csv(df, file) 
    }
  )
  
  output$btn_download_master_csv <- downloadHandler(
    filename = function() { paste0("master_moth_records_", format(Sys.Date(), "%Y%m%d"), ".csv") },
    content = function(file) { 
      df <- master_data_val() %>% mutate(across(where(is.factor), as.character))
      write_csv(df, file) 
    }
  )
}

shinyApp(ui, server)