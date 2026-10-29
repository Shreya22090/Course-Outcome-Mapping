# ============================================================
# CO - PO/PSO Mapping System  (R Shiny + SQLite)
# Run:  shiny::runApp()
# ============================================================
library(shiny); library(bslib); library(DT)
library(DBI); library(RSQLite); library(openxlsx)

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

con <- db_connect()
onStop(function() DBI::dbDisconnect(con))

LEVEL_COLORS <- c("#fff3cd", "#cfe2ff", "#d1e7dd")   # 1, 2, 3

ui <- page_navbar(
  title = "CO – PO/PSO Mapper",
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  # ---------------- 1. Course & COs ----------------
  nav_panel("1. Course & COs",
    layout_columns(col_widths = c(4, 8),
      card(card_header("Course details"),
        selectInput("course_sel", "Course", choices = NULL),
        textInput("course_code", "Course code"),
        textInput("course_name", "Course name"),
        textInput("faculty", "Faculty name"),
        fileInput("co_csv", "Import COs from CSV (columns: CO, Description)", accept = ".csv")),
      card(card_header("Course outcomes"),
        helpText("One outcome per line, e.g.  CO1: Apply ... (lines without a CO label are auto-numbered)."),
        textAreaInput("co_text", NULL, rows = 10, width = "100%"),
        div(actionButton("save_course", "Save course & COs", class = "btn-primary"),
            actionButton("delete_course", "Delete course", class = "btn-outline-danger"))))),

  # ---------------- 2. Mapping ----------------
  nav_panel("2. Mapping",
    layout_sidebar(
      sidebar = sidebar(width = 300,
        h5("Settings"),
        sliderInput("high_cut", "Level 3 if match ≥ (%)", 50, 100, 90),
        sliderInput("med_cut",  "Level 2 if match ≥ (%)", 20, 95, 70),
        selectInput("norm", "Score scaling",
          c("Per CO (best match = 100%)" = "per_co",
            "Whole table (best = 100%)"  = "global",
            "None (raw similarity)"      = "none")),
        sliderInput("min_raw", "No mapping if raw match < (%)", 0, 30, 5),
        checkboxInput("use_verbs", "Use Bloom's verb hints", TRUE),
        actionButton("generate", "Generate mapping", class = "btn-primary w-100"),
        hr(),
        actionButton("save_mapping", "Save mapping", class = "btn-success w-100"),
        br(), br(),
        downloadButton("download_xlsx", "Download Excel", class = "w-100")),
      card(card_header("CO – PO/PSO mapping (double-click a cell to edit: 1, 2, 3 or blank)"),
           DTOutput("map_table")),
      card(card_header("Column average"), DTOutput("avg_table")))),

  # ---------------- 3. Similarity ----------------
  nav_panel("Similarity %",
    card(card_header("Match percentage behind each level"), DTOutput("sim_table"))),

  # ---------------- 4. PO/PSO library ----------------
  nav_panel("PO/PSO Library",
    card(card_header("Predefined outcomes (title and description are editable)"),
         DTOutput("lib_table"),
         div(actionButton("save_lib", "Save changes", class = "btn-primary"))))
)

server <- function(input, output, session) {
  rv  <- reactiveValues(levels = NULL, sim = NULL, ver = 0)
  lib <- reactiveVal(db_get_outcomes(con))

  refresh_courses <- function(selected = "") {
    cs <- db_list_courses(con)
    ch <- c("— New course —" = "")
    if (nrow(cs)) ch <- c(ch, setNames(cs$id, paste(cs$code, cs$name, sep = " – ")))
    updateSelectInput(session, "course_sel", choices = ch, selected = selected)
  }
  refresh_courses()

  clear_mapping <- function() { rv$levels <- NULL; rv$sim <- NULL; rv$ver <- rv$ver + 1 }

  # ---- load a course when selected ----
  observeEvent(input$course_sel, {
    req(!is.null(input$course_sel))
    if (input$course_sel == "") {
      updateTextInput(session, "course_code", value = "")
      updateTextInput(session, "course_name", value = "")
      updateTextInput(session, "faculty", value = "")
      updateTextAreaInput(session, "co_text", value = "")
      return(clear_mapping())
    }
    id <- as.integer(input$course_sel)
    c1 <- db_get_course(con, id); cos <- db_get_cos(con, id)
    updateTextInput(session, "course_code", value = c1$code)
    updateTextInput(session, "course_name", value = c1$name)
    updateTextInput(session, "faculty", value = c1$faculty)
    updateTextAreaInput(session, "co_text",
      value = paste0(cos$co_code, ": ", cos$description, collapse = "\n"))
    m <- db_load_mapping(con, id)
    rv$levels <- m$levels; rv$sim <- m$sim; rv$ver <- rv$ver + 1
  })

  # ---- CSV import ----
  observeEvent(input$co_csv, {
    df <- tryCatch(read.csv(input$co_csv$datapath, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(df) || ncol(df) < 2) return(showNotification("CSV needs two columns: CO, Description", type = "error"))
    updateTextAreaInput(session, "co_text", value = paste0(df[[1]], ": ", df[[2]], collapse = "\n"))
  })

  # ---- save / delete course ----
  observeEvent(input$save_course, {
    cos <- parse_cos(input$co_text)
    if (!nzchar(trimws(input$course_code)) || nrow(cos) == 0)
      return(showNotification("Enter a course code and at least one CO.", type = "error"))
    id <- if (input$course_sel == "") {
      db_create_course(con, input$course_code, input$course_name, input$faculty)
    } else {
      db_update_course(con, as.integer(input$course_sel), input$course_code, input$course_name, input$faculty)
      as.integer(input$course_sel)
    }
    db_save_cos(con, id, cos)
    refresh_courses(selected = id)
    clear_mapping()
    showNotification("Course saved. Go to 'Mapping' and click Generate.", type = "message")
  })

  observeEvent(input$delete_course, {
    req(input$course_sel != "")
    db_delete_course(con, as.integer(input$course_sel))
    refresh_courses(); showNotification("Course deleted.")
  })

  # ---- generate mapping ----
  observeEvent(input$generate, {
    if (is.null(input$course_sel) || input$course_sel == "")
      return(showNotification("Save the course first.", type = "error"))
    cos <- db_get_cos(con, as.integer(input$course_sel))
    res <- tryCatch(
      compute_mapping(cos, db_get_outcomes(con), input$high_cut, input$med_cut,
                      input$norm, input$min_raw, input$use_verbs),
      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
    req(res)
    rv$levels <- res$levels; rv$sim <- res$sim; rv$ver <- rv$ver + 1
  })

  # ---- mapping table (editable) ----
  output$map_table <- renderDT({
    rv$ver; req(isolate(rv$levels))
    df <- as.data.frame(isolate(rv$levels), check.names = FALSE)
    datatable(df, selection = "none",
              editable = list(target = "cell", disable = list(columns = 0)),
              options = list(dom = "t", pageLength = 50, scrollX = TRUE)) |>
      formatStyle(names(df), backgroundColor = styleEqual(1:3, LEVEL_COLORS), textAlign = "center")
  })
  proxy <- dataTableProxy("map_table")

  observeEvent(input$map_table_cell_edit, {
    info <- input$map_table_cell_edit
    v <- suppressWarnings(as.numeric(info$value))
    if (is.na(v) || !(v %in% 1:3)) v <- NA_real_
    rv$levels[info$row, info$col] <- v
    replaceData(proxy, as.data.frame(rv$levels, check.names = FALSE),
                resetPaging = FALSE, rownames = TRUE)
  })

  output$avg_table <- renderDT({
    req(rv$levels)
    avg <- as.data.frame(t(column_average(rv$levels)), check.names = FALSE)
    datatable(avg, rownames = "Average", selection = "none",
              options = list(dom = "t", scrollX = TRUE)) |>
      formatStyle(names(avg), fontWeight = "bold", textAlign = "center")
  })

  output$sim_table <- renderDT({
    req(rv$sim)
    df <- as.data.frame(round(rv$sim, 1), check.names = FALSE)
    datatable(df, selection = "none", options = list(dom = "t", pageLength = 50, scrollX = TRUE)) |>
      formatStyle(names(df), backgroundColor = styleInterval(c(70, 90), c("white", LEVEL_COLORS[2:3])))
  })

  # ---- save mapping ----
  observeEvent(input$save_mapping, {
    req(rv$levels, input$course_sel != "")
    db_save_mapping(con, as.integer(input$course_sel), rv$sim, rv$levels)
    showNotification("Mapping saved to database.", type = "message")
  })

  # ---- Excel export ----
  output$download_xlsx <- downloadHandler(
    filename = function() paste0("CO_PO_PSO_", gsub("[^A-Za-z0-9]", "_", input$course_code), ".xlsx"),
    content = function(file) {
      req(rv$levels)
      wb <- createWorkbook()
      hdr <- createStyle(textDecoration = "bold", fgFill = "#2C3E50", fontColour = "white",
                         halign = "center", border = "TopBottomLeftRight")
      ctr <- createStyle(halign = "center", border = "TopBottomLeftRight")
      addWorksheet(wb, "Mapping")
      writeData(wb, "Mapping", paste("Course:", input$course_code, "-", input$course_name), startRow = 1)
      writeData(wb, "Mapping", paste("Faculty:", input$faculty), startRow = 2)
      tbl <- rbind(rv$levels, Average = column_average(rv$levels))
      tbl <- cbind(CO = rownames(tbl), as.data.frame(tbl, check.names = FALSE))
      writeData(wb, "Mapping", tbl, startRow = 4, keepNA = TRUE, na.string = "-", headerStyle = hdr)
      addStyle(wb, "Mapping", ctr, rows = 5:(4 + nrow(tbl)), cols = 1:ncol(tbl), gridExpand = TRUE)
      setColWidths(wb, "Mapping", cols = 1:ncol(tbl), widths = 9)
      addWorksheet(wb, "Similarity %")
      writeData(wb, "Similarity %", cbind(CO = rownames(rv$sim), as.data.frame(round(rv$sim, 1), check.names = FALSE)),
                headerStyle = hdr)
      addWorksheet(wb, "Course Outcomes")
      writeData(wb, "Course Outcomes", db_get_cos(con, as.integer(input$course_sel)), headerStyle = hdr)
      setColWidths(wb, "Course Outcomes", cols = 1:2, widths = c(10, 100))
      addWorksheet(wb, "PO-PSO Reference")
      writeData(wb, "PO-PSO Reference", db_get_outcomes(con), headerStyle = hdr)
      setColWidths(wb, "PO-PSO Reference", cols = 1:4, widths = c(10, 8, 35, 110))
      saveWorkbook(wb, file, overwrite = TRUE)
    })

  # ---- PO/PSO library ----
  output$lib_table <- renderDT({
    datatable(isolate(lib()), rownames = FALSE, selection = "none",
              editable = list(target = "cell", disable = list(columns = c(0, 1))),
              options = list(dom = "t", pageLength = 20))
  })
  observeEvent(input$lib_table_cell_edit,
    lib(editData(lib(), input$lib_table_cell_edit, rownames = FALSE)))
  observeEvent(input$save_lib, {
    db_save_outcomes(con, lib())
    showNotification("PO/PSO library updated.", type = "message")
  })
}

shinyApp(ui, server)
