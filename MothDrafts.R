library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(DT)
library(ggplot2)

# ==============================================================================
# 1. FILE PATHS & DATA LOADERS
# ==============================================================================

SITES_FILE <- "sites.csv"
RECORDS_FILE <- if (dir.exists("/srv/data/MothRecorder")) "/srv/data/MothRecorder/master_moth_records.csv"  else "master_moth_records.csv"  # must be used on server
SPECIES_FILE <- "MothSpecies.csv"
LOOKUPS_FILE <- "Mothlookups.csv"

TEMP_DIR <- if (dir.exists("/srv/data/MothRecorder")) "/srv/data/MothRecorder" else "temp_saves"
if (!dir.exists(TEMP_DIR)) {
  dir.create(TEMP_DIR, recursive = TRUE, showWarnings = FALSE)
}

load_lookups <- function(file_path) {
  defaults <- list(
    vice_counties = c("61-South-east Yorkshire", "5-South Somerset", "62-North-east Yorkshire"),
    methods       = c("LED light", "Moth trap (MV)", "Moth trap (Actinic)", "Wine rope / Sugar", "Daytime observation"),
    sex           = c("Unrecorded", "Male", "Female"),
    stage         = c("Adult", "Larva", "Pupa", "Egg"),
    status        = c("Certain", "Considered correct", "Unconfirmed")
  )
  
  if (!file.exists(file_path)) {
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
  
  for (i in seq_len(nrow(out))) {
    existing_keys <- unlist(strsplit(out$Shortcut[i], ",\\s*"))
    existing_keys <- existing_keys[nchar(existing_keys) > 0]
    out$Shortcut[i] <- paste(unique(existing_keys), collapse = ", ")
  }
  
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

apply_factor_dropdowns <- function(df, lookups) {
  if (nrow(df) == 0) return(df)
  
  sex_opts <- unique(c("", trimws(as.character(df[["Sex"]])), lookups$sex))
  sex_opts <- sex_opts[!is.na(sex_opts) & (nchar(sex_opts) > 0 | sex_opts == "")]
  df[["Sex"]] <- factor(df[["Sex"]], levels = sex_opts)
  
  stage_opts <- unique(c("", trimws(as.character(df[["Life stage"]])), lookups$stage))
  stage_opts <- stage_opts[!is.na(stage_opts) & (nchar(stage_opts) > 0 | stage_opts == "")]
  df[["Life stage"]] <- factor(df[["Life stage"]], levels = stage_opts)
  
  status_opts <- unique(c("", trimws(as.character(df[["Record status"]])), lookups$status))
  status_opts <- status_opts[!is.na(status_opts) & (nchar(status_opts) > 0 | status_opts == "")]
  df[["Record status"]] <- factor(df[["Record status"]], levels = status_opts)
  
  qual_opts <- unique(c("", trimws(as.character(df[["Abundance qualifier"]])), c("Exact", "Estimate")))
  qual_opts <- qual_opts[!is.na(qual_opts) & (nchar(qual_opts) > 0 | qual_opts == "")]
  df[["Abundance qualifier"]] <- factor(df[["Abundance qualifier"]], levels = qual_opts)
  
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
    #species-select-wrapper .selectize-dropdown .option.active,
    #species-select-wrapper .selectize-dropdown .active {
      background-color: #e2e8f0 !important;
    }
    
    .stepper-container {
      display: flex;
      align-items: center;
      gap: 8px;
      margin-bottom: 1rem;
    }
    .stepper-container .shiny-input-container {
      margin-bottom: 0 !important;
      width: 100px;
    }
    .stepper-container input {
      text-align: center;
      font-weight: bold;
    }
    .btn-stepper {
      width: 40px;
      height: 38px;
      padding: 0;
      font-size: 1.25rem;
      font-weight: bold;
      line-height: 1;
      display: flex;
      align-items: center;
      justify-content: center;
      border-radius: 6px;
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
        actionButton("btn_start_session", "Lock Info & Start Quick Entry", class = "btn-primary w-100"),
        br(), br(),
        actionButton("recover_btn", "Help, I lost my connection!", class = "btn-warning w-100")
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
          div(
            class = "stepper-container",
            actionButton("minus_one", "-", class = "btn btn-outline-secondary btn-stepper"),
            numericInput("count", label = NULL, value = 1, min = 1),
            actionButton("plus_one", "+", class = "btn btn-primary btn-stepper")
          ),
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
      htmlOutput("session_summary_banner"),
      p(class = "text-muted small", "Double-click cells to edit. Select a row and click 'Delete Selected Row' to remove entries."),
      div(style = "overflow-x: auto;", DTOutput("review_export_table")),
      card_footer(
        div(
          class = "d-flex gap-2 flex-wrap",
          actionButton("btn_save_master", "Append to Master CSV", class = "btn-primary"),
          downloadButton("btn_download_csv", "Download Session CSV", class = "btn-outline-success"),
          actionButton("btn_delete_review_row", "Delete Selected Row(s)", class = "btn-danger"),
          actionButton("btn_stitch_temp", "Stitch Temporary Files", class = "btn-warning ms-auto")
        )
      )
    )
  ),
  
  # --- TAB 4: ADMIN & DATABASE (Natural vertical scrolling enabled) ---
  nav_panel(
    title = "4. Admin & Database",
    value = "tab_admin",
    icon = icon("user-shield"),
    fillable = FALSE,
    div(
      style = "padding-bottom: 4rem;",
      uiOutput("admin_panel_ui")
    )
  ),
  
  # --- TAB 5: VISUALISE DATA ---
  nav_panel(
    title = "5. Visualise Data",
    value = "tab_viz",
    icon = icon("chart-line"),
    layout_sidebar(
      sidebar = sidebar(
        open = "always",
        h5("Plot Controls"),
        selectInput("viz_site", "Select Site:", choices = NULL),
        selectInput(
          "viz_metric", 
          "Select Metric:", 
          choices = c(
            "Daily Total Abundance" = "abundance",
            "Daily Species Richness" = "richness",
            "Cumulative Species Richness" = "cum_richness",
            "Individual Species Total" = "species"
          )
        ),
        conditionalPanel(
          condition = "input.viz_metric == 'species'",
          selectInput("viz_species", "Select Species:", choices = NULL)
        )
      ),
      card(
        card_header("Annual Trends Comparison"),
        plotOutput("trend_plot", height = "500px")
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
  
  active_temp_file <- reactiveVal(NULL)
  admin_unlocked <- reactiveVal(FALSE)
  
  export_data_val <- reactiveVal(data.frame())
  master_data_val <- reactiveVal(data.frame())
  
  # Unique timestamped filepath generator
  get_temp_filepath <- function(site, date_str) {
    clean_site <- gsub("[^A-Za-z0-9]", "_", site)
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    file.path(TEMP_DIR, paste0("temp_", clean_site, "_", date_str, "_", timestamp, ".rds"))
  }
  
  # Dynamic species calculation for Tab 2 dropdown based on current site & last 30 days
  sorted_species_df <- reactive({
    sp <- species_df()
    master <- master_data_val()
    
    site_name <- tryCatch(current_site_info()$Site, error = function(e) NULL)
    trap_date <- input$rec_date
    
    if (is.null(site_name) || nchar(site_name) == 0 || is.null(trap_date) || nrow(master) == 0) {
      return(sp)
    }
    
    date_start <- trap_date - 30
    
    recent_site_counts <- master %>%
      mutate(
        Taxon_clean = trimws(iconv(Taxon, to = "UTF-8", sub = "")),
        Location_clean = trimws(iconv(`Location (64)`, to = "UTF-8", sub = "")),
        Parsed_Date = as.Date(`Date (10)`, format = "%d/%m/%Y")
      ) %>%
      filter(
        Location_clean == trimws(site_name),
        !is.na(Parsed_Date),
        Parsed_Date >= date_start,
        Parsed_Date <= trap_date
      ) %>%
      group_by(Taxon = Taxon_clean) %>%
      summarize(RecentFreq = n(), .groups = "drop")
    
    sp %>%
      left_join(recent_site_counts, by = "Taxon") %>%
      mutate(RecentFreq = ifelse(is.na(RecentFreq), 0, RecentFreq)) %>%
      arrange(desc(RecentFreq), Taxon)
  })
  
  # Stepper counter actions
  observeEvent(input$plus_one, {
    current <- ifelse(is.na(input$count), 0, input$count)
    updateNumericInput(session, "count", value = current + 1)
  })
  
  observeEvent(input$minus_one, {
    current <- ifelse(is.na(input$count), 1, input$count)
    updateNumericInput(session, "count", value = max(1, current - 1))
  })
  
  # Recovery Trigger Modal
  observeEvent(input$recover_btn, {
    temp_files <- list.files(TEMP_DIR, pattern = "^temp_.*\\.rds$")
    
    if (length(temp_files) == 0) {
      showModal(modalDialog(
        title = "No Saved Sessions Found",
        "No uncommitted temporary session backups were found on the server.",
        easyClose = TRUE
      ))
    } else {
      showModal(modalDialog(
        title = "Recover Lost Session",
        size = "l",
        selectInput(
          "selected_recovery_file", 
          "Select your Site & Date session backup:", 
          choices = temp_files,
          width = "100%"
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_recovery", "Restore Session Data", class = "btn-success")
        )
      ))
    }
  })
  
  observeEvent(input$confirm_recovery, {
    req(input$selected_recovery_file)
    filepath <- file.path(TEMP_DIR, input$selected_recovery_file)
    
    if (file.exists(filepath)) {
      restored_obj <- readRDS(filepath)
      
      # Extract counts table & metadata (backward compatible with old dataframe-only files)
      if (is.data.frame(restored_obj)) {
        session_counts(restored_obj)
      } else {
        session_counts(restored_obj$counts)
        meta <- restored_obj$metadata
        
        # Restore Site Selection
        if (!is.null(meta$site)) {
          st <- sites_df()
          if (meta$site %in% st$Site) {
            updateSelectInput(session, "site_select", selected = meta$site)
            updateCheckboxInput(session, "is_new_site", value = FALSE)
          } else {
            updateCheckboxInput(session, "is_new_site", value = TRUE)
            updateTextInput(session, "new_site_name", value = meta$site)
            if (!is.null(meta$grid_ref)) updateTextInput(session, "new_grid_ref", value = meta$grid_ref)
            if (!is.null(meta$vice_county)) updateSelectInput(session, "new_vc", selected = meta$vice_county)
          }
        }
        
        # Restore Date & Other Session Metadata
        if (!is.null(meta$date)) updateDateInput(session, "rec_date", value = as.Date(meta$date))
        if (!is.null(meta$observer)) updateTextInput(session, "observer", value = meta$observer)
        if (!is.null(meta$method)) updateSelectInput(session, "sampling_method", selected = meta$method)
        if (!is.null(meta$stage)) updateSelectInput(session, "life_stage", selected = meta$stage)
      }
      
      # Delete restored temp file from disk upon recovery
      tryCatch({ file.remove(filepath) }, error = function(e) NULL)
      active_temp_file(NULL)
      
      removeModal()
      showNotification("Session tally and site/date metadata successfully restored!", type = "message")
      nav_select("main_nav", "tab_counter")
    }
  })
  
  # Stitch Temporary Files Action (Excludes working/latest file of current site, deletes stitched files)
  observeEvent(input$btn_stitch_temp, {
    temp_files <- list.files(TEMP_DIR, pattern = "^temp_.*\\.rds$", full.names = FALSE)
    
    # Identify active working file & latest temp file for current site
    active_fname <- if (!is.null(active_temp_file())) basename(active_temp_file()) else NULL
    
    site_name <- tryCatch(current_site_info()$Site, error = function(e) "")
    clean_site <- gsub("[^A-Za-z0-9]", "_", site_name)
    site_temp_files <- temp_files[grepl(paste0("^temp_", clean_site, "_"), temp_files)]
    
    most_recent_site_file <- NULL
    if (length(site_temp_files) > 0) {
      file_paths <- file.path(TEMP_DIR, site_temp_files)
      mtimes <- file.info(file_paths)$mtime
      most_recent_site_file <- site_temp_files[which.max(mtimes)]
    }
    
    exclude_files <- unique(c(active_fname, most_recent_site_file))
    available_files <- setdiff(temp_files, exclude_files)
    
    if (length(available_files) == 0) {
      showModal(modalDialog(
        title = "No Available Temporary Files",
        "No prior temporary files were found to stitch (the active working session file is excluded).",
        easyClose = TRUE
      ))
    } else {
      showModal(modalDialog(
        title = "Stitch Temporary Files",
        size = "l",
        p("Select one or more prior temporary session files to combine into the active session tally:"),
        selectInput(
          "stitch_files_select",
          "Select Files to Combine:",
          choices = available_files,
          multiple = TRUE,
          width = "100%"
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_stitch", "Combine & Delete Selected Files", class = "btn-primary")
        )
      ))
    }
  })
  
  observeEvent(input$confirm_stitch, {
    req(input$stitch_files_select)
    
    current_df <- session_counts()
    stitched_dfs <- list(current_df)
    
    for (fname in input$stitch_files_select) {
      fpath <- file.path(TEMP_DIR, fname)
      if (file.exists(fpath)) {
        obj <- readRDS(fpath)
        tdf <- if (is.data.frame(obj)) obj else obj$counts
        stitched_dfs[[length(stitched_dfs) + 1]] <- tdf
        # Delete stitched temporary file from disk
        tryCatch({ file.remove(fpath) }, error = function(e) NULL)
      }
    }
    
    combined <- bind_rows(stitched_dfs) %>%
      group_by(Taxon) %>%
      summarize(
        Count = sum(Count, na.rm = TRUE),
        Comment = paste(unique(Comment[nchar(trimws(Comment)) > 0]), collapse = "; "),
        .groups = "drop"
      )
    
    session_counts(combined)
    removeModal()
    showNotification("Selected temporary files successfully stitched and removed from server!", type = "message")
  })
  
  output$session_summary_banner <- renderUI({
    counts <- session_counts()
    
    total_moths <- sum(counts$Count, na.rm = TRUE)
    total_species <- length(unique(counts$Taxon[nchar(trimws(counts$Taxon)) > 0]))
    
    div(
      class = "alert alert-primary mb-3",
      style = "font-size: 1.15rem; font-weight: 600;",
      sprintf("In this session you recorded %d moth%s of %d species.", 
              total_moths, ifelse(total_moths == 1, "", "s"), total_species)
    )
  })
  
  observe({
    lk <- lookups_data()
    updateSelectInput(session, "new_vc", choices = lk$vice_counties)
    
    default_method <- lk$methods[1]
    led_match <- grep("LED", lk$methods, ignore.case = TRUE, value = TRUE)
    if (length(led_match) > 0) default_method <- led_match[1]
    
    updateSelectInput(session, "sampling_method", choices = lk$methods, selected = default_method)
    updateSelectInput(session, "life_stage", choices = lk$stage, selected = if ("Adult" %in% lk$stage) "Adult" else lk$stage[1])
  })
  
  observe({
    st <- sites_df()
    updateSelectInput(session, "site_select", choices = st$Site)
  })
  
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
  
  # Render Tab 2 Species Dropdown with dynamically sorted species
  observe({
    sp <- sorted_species_df()
    req(nrow(sp) > 0)
    
    sp_options <- lapply(seq_len(nrow(sp)), function(i) {
      g_color <- switch(as.character(sp$Grade[i]),
                        "1" = "#006400",
                        "2" = "#0000FF",
                        "3" = "#FF8C00",
                        "4" = "#FF0000",
                        "#000000"
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
    req(input$species_input, input$count)
    
    selected_taxon <- trimws(input$species_input)
    add_qty <- as.integer(input$count)
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
    
    tryCatch({
      site_info <- current_site_info()
      date_str <- format(input$rec_date, "%Y%m%d")
      
      if (is.null(active_temp_file())) {
        active_temp_file(get_temp_filepath(site_info$Site, date_str))
      }
      
      # Package count table + metadata together for backup
      save_payload <- list(
        counts = updated,
        metadata = list(
          site = site_info$Site,
          grid_ref = site_info$GridRef,
          vice_county = site_info$ViceCounty,
          date = input$rec_date,
          observer = input$observer,
          method = input$sampling_method,
          stage = input$life_stage
        )
      )
      
      saveRDS(save_payload, file = active_temp_file())
    }, error = function(e) {
      warning("Auto-save backup failed: ", e$message)
    })
    
    updateNumericInput(session, "count", value = 1)
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
  
  observeEvent(session_counts(), {
    if (nrow(session_counts()) > 0) {
      export_data_val(formatted_export_data())
    } else {
      export_data_val(data.frame())
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$review_export_table_cell_edit, {
    info <- input$review_export_table_cell_edit
    current_df <- export_data_val() %>% mutate(across(where(is.factor), as.character))
    updated_df <- DT::editData(current_df, info, rownames = FALSE)
    export_data_val(apply_factor_dropdowns(updated_df, lookups_data()))
  })
  
  # Delete Selected Row in Review & Export Tab
  observeEvent(input$btn_delete_review_row, {
    selected_rows <- input$review_export_table_rows_selected
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("Please select a row to delete.", type = "warning")
      return()
    }
    
    current_export <- export_data_val()
    deleted_taxons <- current_export$Taxon[selected_rows]
    
    updated_counts <- session_counts() %>% filter(!Taxon %in% deleted_taxons)
    session_counts(updated_counts)
    showNotification("Row deleted from session.", type = "message")
  })
  
  output$review_export_table <- renderDT({
    req(nrow(export_data_val()) > 0)
    datatable(
      export_data_val(),
      editable = TRUE,
      rownames = FALSE,
      selection = "single",
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 't',
        ordering = FALSE
      )
    )
  })
  
  # Append to Master
  observeEvent(input$btn_save_master, {
    out_df <- export_data_val()
    req(nrow(out_df) > 0)
    
    out_df <- out_df %>% mutate(across(where(is.factor), as.character))
    write_csv(out_df, RECORDS_FILE, append = TRUE)
    
    tryCatch({
      if (!is.null(active_temp_file()) && file.exists(active_temp_file())) {
        file.remove(active_temp_file())
      }
      active_temp_file(NULL)
    }, error = function(e) NULL)
    
    session_counts(data.frame(
      Taxon = character(), 
      Count = integer(), 
      Comment = character(), 
      stringsAsFactors = FALSE
    ))
    export_data_val(data.frame())
    master_data_val(load_master_df())
    
    species_df(load_and_clean_species(SPECIES_FILE, RECORDS_FILE))
    
    showNotification("Records saved to master spreadsheet! Session reset.", type = "message", duration = 5)
    nav_select("main_nav", "tab_setup")
  })
  
  output$btn_download_csv <- downloadHandler(
    filename = function() { paste0("moth_records_", format(input$rec_date, "%Y%m%d"), ".csv") },
    content = function(file) { 
      df <- export_data_val() %>% mutate(across(where(is.factor), as.character))
      write_csv(df, file) 
    }
  )
  
  # ==============================================================================
  # 4. TAB 4: PUBLIC MASTER PREVIEW & ADMIN PANEL
  # ==============================================================================
  
  observeEvent(input$btn_admin_login, {
    if (input$admin_pwd == "Moth Recorder Admin") {
      admin_unlocked(TRUE)
      showNotification("Admin panel unlocked. Full editing enabled.", type = "message")
    } else {
      showNotification("Incorrect admin password.", type = "error")
    }
  })
  
  observeEvent(input$btn_admin_lock, {
    admin_unlocked(FALSE)
    showNotification("Admin features locked.", type = "message")
  })
  
  output$admin_panel_ui <- renderUI({
    is_admin <- admin_unlocked()
    
    tagList(
      card(
        fill = FALSE,
        class = "mb-4",
        card_header(
          div(
            class = "d-flex justify-content-between align-items-center",
            span("Master Database Records (master_moth_records.csv)"),
            span(
              class = if (is_admin) "badge bg-success" else "badge bg-secondary",
              if (is_admin) "Admin Unlocked (Edit Mode)" else "Public View (Read Only)"
            )
          )
        ),
        p(class = "text-muted small ms-3 mt-2", 
          if (is_admin) "Double-click cells to edit. Select rows and click delete to remove master records." 
          else "Anyone can view master records. Enter admin password below to enable record editing and file cleanup."
        ),
        div(style = "overflow-x: auto; padding: 0 1rem;", DTOutput("master_records_table")),
        card_footer(
          if (is_admin) {
            div(
              class = "d-flex gap-2 flex-wrap align-items-center",
              actionButton("btn_save_master_edits", "Save Master Edits", class = "btn-success"),
              actionButton("btn_delete_master_row", "Delete Selected Master Row(s)", class = "btn-danger"),
              actionButton("btn_admin_lock", "Lock Admin", class = "btn-outline-secondary"),
              downloadButton("btn_download_master_csv", "Download Master CSV", class = "btn-outline-primary ms-auto")
            )
          } else {
            div(
              class = "d-flex gap-2 flex-wrap align-items-center",
              div(style = "width: 250px;", passwordInput("admin_pwd", NULL, placeholder = "Admin Password")),
              actionButton("btn_admin_login", "Unlock Admin Editing", class = "btn-primary"),
              downloadButton("btn_download_master_csv", "Download Master CSV", class = "btn-outline-primary ms-auto")
            )
          }
        )
      ),
      
      if (is_admin) {
        card(
          fill = FALSE,
          class = "mb-4",
          card_header("Temporary Files Cleanup Manager"),
          p(class = "text-muted small ms-3 mt-2", "Manage or clean up leftover temporary backup working files from the server."),
          div(style = "overflow-x: auto; padding: 0 1rem;", DTOutput("admin_temp_files_table")),
          card_footer(
            actionButton("btn_delete_temp_files", "Delete Selected Temp File(s)", class = "btn-danger")
          )
        )
      }
    )
  })
  
  output$admin_temp_files_table <- renderDT({
    req(admin_unlocked())
    files <- list.files(TEMP_DIR, pattern = "^temp_.*\\.rds$", full.names = TRUE)
    
    if (length(files) == 0) {
      df <- data.frame(Filename = character(), Size_KB = numeric(), Last_Modified = character())
    } else {
      info <- file.info(files)
      df <- data.frame(
        Filename = basename(files),
        Size_KB = round(info$size / 1024, 2),
        Last_Modified = format(info$mtime, "%Y-%m-%d %H:%M:%S"),
        stringsAsFactors = FALSE
      )
    }
    
    datatable(
      df, 
      rownames = FALSE, 
      selection = "multiple", 
      options = list(
        pageLength = 10, 
        scrollX = TRUE, 
        autoWidth = TRUE,
        dom = 'fltip'
      )
    )
  })
  
  observeEvent(input$btn_delete_temp_files, {
    req(admin_unlocked())
    selected_rows <- input$admin_temp_files_table_rows_selected
    
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("Please select temporary files to delete.", type = "warning")
      return()
    }
    
    files <- list.files(TEMP_DIR, pattern = "^temp_.*\\.rds$", full.names = TRUE)
    files_to_del <- files[selected_rows]
    
    file.remove(files_to_del)
    showNotification("Selected temporary backup files removed.", type = "message")
  })
  
  observeEvent(input$master_records_table_cell_edit, {
    req(admin_unlocked())
    info <- input$master_records_table_cell_edit
    current_df <- master_data_val() %>% mutate(across(where(is.factor), as.character))
    updated_df <- DT::editData(current_df, info, rownames = FALSE)
    master_data_val(apply_factor_dropdowns(updated_df, lookups_data()))
  })
  
  observeEvent(input$btn_delete_master_row, {
    req(admin_unlocked())
    selected_rows <- input$master_records_table_rows_selected
    
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("Please select row(s) in the Master Database table to delete.", type = "warning")
      return()
    }
    
    current_df <- master_data_val()
    updated_df <- current_df[-selected_rows, , drop = FALSE]
    master_data_val(apply_factor_dropdowns(updated_df, lookups_data()))
    showNotification("Selected row(s) removed. Click 'Save Master Edits' to apply changes to disk.", type = "message")
  })
  
  output$master_records_table <- renderDT({
    req(nrow(master_data_val()) > 0)
    is_editable <- admin_unlocked()
    
    datatable(
      master_data_val(),
      editable = is_editable,
      rownames = FALSE,
      selection = if (is_editable) "multiple" else "none",
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = 'fltip',
        ordering = FALSE
      )
    )
  })
  
  observeEvent(input$btn_save_master_edits, {
    req(admin_unlocked())
    out_df <- master_data_val()
    req(nrow(out_df) > 0)
    
    out_df <- out_df %>% mutate(across(where(is.factor), as.character))
    write_csv(out_df, RECORDS_FILE)
    
    species_df(load_and_clean_species(SPECIES_FILE, RECORDS_FILE))
    showNotification("Master records spreadsheet updated successfully!", type = "message", duration = 5)
  })
  
  output$btn_download_master_csv <- downloadHandler(
    filename = function() { paste0("master_moth_records_", format(Sys.Date(), "%Y%m%d"), ".csv") },
    content = function(file) { 
      df <- master_data_val() %>% mutate(across(where(is.factor), as.character))
      write_csv(df, file) 
    }
  )
  
  # ==============================================================================
  # 5. TAB 5: VISUALISATION LOGIC
  # ==============================================================================
  
  observe({
    df <- master_data_val()
    if (is.null(df) || nrow(df) == 0) {
      updateSelectInput(session, "viz_site", choices = character(0))
      return()
    }
    
    sites <- df %>%
      pull(`Location (64)`) %>%
      iconv(to = "UTF-8", sub = "") %>%
      trimws() %>%
      unique()
    
    sites <- sites[nchar(sites) > 0]
    updateSelectInput(session, "viz_site", choices = sort(sites))
  })
  
  observe({
    req(input$viz_site)
    df <- master_data_val()
    if (is.null(df) || nrow(df) == 0) {
      updateSelectInput(session, "viz_species", choices = character(0))
      return()
    }
    
    sp_list <- df %>%
      filter(trimws(iconv(`Location (64)`, to = "UTF-8", sub = "")) == input$viz_site) %>%
      mutate(
        Taxon = trimws(iconv(Taxon, to = "UTF-8", sub = "")),
        Vernacular = trimws(iconv(Vernacular, to = "UTF-8", sub = ""))
      ) %>%
      filter(!is.na(Taxon) & nchar(Taxon) > 0) %>%
      filter(str_detect(Taxon, "\\s+")) %>%
      mutate(Display_Name = ifelse(!is.na(Vernacular) & nchar(Vernacular) > 0, Vernacular, Taxon)) %>%
      pull(Display_Name) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "viz_species", choices = sp_list)
  })
  
  output$trend_plot <- renderPlot({
    req(input$viz_site)
    df <- master_data_val()
    req(nrow(df) > 0)
    
    site_raw <- df %>%
      filter(trimws(iconv(`Location (64)`, to = "UTF-8", sub = "")) == input$viz_site) %>%
      mutate(
        Taxon = trimws(iconv(Taxon, to = "UTF-8", sub = "")),
        Vernacular = trimws(iconv(Vernacular, to = "UTF-8", sub = "")),
        Parsed_Date = as.Date(`Date (10)`, format = "%d/%m/%Y"),
        Abundance_Num = suppressWarnings(as.numeric(Abundance))
      ) %>%
      filter(!is.na(Parsed_Date), !is.na(Abundance_Num)) %>%
      filter(str_detect(Taxon, "\\s+"))
    
    req(nrow(site_raw) > 0)
    
    site_df <- site_raw %>%
      group_by(Parsed_Date, Taxon, Vernacular) %>%
      summarize(Abundance_Num = sum(Abundance_Num, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        Year = factor(format(Parsed_Date, "%Y")),
        Dummy_Date = as.Date(paste0("2000-", format(Parsed_Date, "%m-%d")))
      )
    
    latest_year <- max(as.numeric(as.character(site_df$Year)), na.rm = TRUE)
    rug_dates <- site_df %>%
      filter(Year == as.character(latest_year)) %>%
      pull(Parsed_Date) %>%
      unique()
    
    rug_df <- data.frame(
      Dummy_Date = as.Date(paste0("2000-", format(rug_dates, "%m-%d")))
    )
    
    base_plot <- function(plot_data, y_title, plot_title) {
      ggplot(plot_data, aes(x = Dummy_Date, y = Metric, color = Year, group = Year)) +
        geom_line(linewidth = 0.9) +
        geom_point(size = 2) +
        geom_rug(
          data = rug_df, 
          aes(x = Dummy_Date), 
          inherit.aes = FALSE, 
          sides = "b", 
          color = "#2c3e50", 
          linewidth = 0.7,
          length = unit(0.04, "npc")
        ) +
        scale_x_date(
          date_labels = "%b", 
          date_breaks = "1 month",
          limits = c(as.Date("2000-01-01"), as.Date("2000-12-31")),
          expand = c(0.01, 0.01)
        ) +
        labs(
          title = plot_title,
          subtitle = paste("Site:", input$viz_site),
          caption = paste("Rug ticks on x-axis indicate trapping days in", latest_year),
          x = "Date (Jan – Dec)",
          y = y_title,
          color = "Year"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold")
        )
    }
    
    if (input$viz_metric == "abundance") {
      summary_df <- site_df %>%
        group_by(Year, Dummy_Date, Parsed_Date) %>%
        summarize(Metric = sum(Abundance_Num, na.rm = TRUE), .groups = "drop")
      
      base_plot(summary_df, "Total Moth Count", "Daily Total Abundance")
      
    } else if (input$viz_metric == "richness") {
      summary_df <- site_df %>%
        group_by(Year, Dummy_Date, Parsed_Date) %>%
        summarize(Metric = n_distinct(Taxon), .groups = "drop")
      
      base_plot(summary_df, "Unique Species Count", "Daily Species Richness")
      
    } else if (input$viz_metric == "cum_richness") {
      first_obs <- site_df %>%
        group_by(Year, Taxon) %>%
        summarize(First_Date = min(Parsed_Date), .groups = "drop") %>%
        group_by(Year, First_Date) %>%
        summarize(New_Species = n(), .groups = "drop")
      
      all_trap_days <- site_df %>%
        select(Year, Parsed_Date, Dummy_Date) %>%
        distinct()
      
      summary_df <- all_trap_days %>%
        left_join(first_obs, by = c("Year", "Parsed_Date" = "First_Date")) %>%
        mutate(New_Species = ifelse(is.na(New_Species), 0, New_Species)) %>%
        arrange(Year, Parsed_Date) %>%
        group_by(Year) %>%
        mutate(Metric = cumsum(New_Species)) %>%
        ungroup()
      
      base_plot(summary_df, "Cumulative Species Count", "Cumulative Species Richness")
      
    } else if (input$viz_metric == "species") {
      req(input$viz_species)
      
      all_trap_days <- site_df %>%
        select(Parsed_Date, Year, Dummy_Date) %>%
        distinct()
      
      spec_records <- site_df %>%
        filter(Vernacular == input$viz_species | Taxon == input$viz_species) %>%
        group_by(Parsed_Date) %>%
        summarize(Metric = sum(Abundance_Num, na.rm = TRUE), .groups = "drop")
      
      summary_df <- all_trap_days %>%
        left_join(spec_records, by = "Parsed_Date") %>%
        mutate(Metric = ifelse(is.na(Metric), 0, Metric))
      
      base_plot(summary_df, "Count", paste("Abundance Trend:", input$viz_species))
    }
  })
}

shinyApp(ui, server)