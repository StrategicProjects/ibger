## ibger Aggregate Explorer
## Launched via ibge_explorer()

# ── UI ─────────────────────────────────────────────────────────────────────

require(htmltools)
require(bsicons)
require(shiny)
require(DT)
require(utils)

ui <- bslib::page_sidebar(
  title = "ibger",
  theme = bslib::bs_theme(
    version = 5,
    preset = "shiny",
    primary = "#0d3b66",
    "navbar-bg" = "#0d3b66",
    base_font = bslib::font_google("Jost"),
    heading_font = bslib::font_google("Jost"),
    font_scale = 0.9
  ),
  
  shiny::tags$head(
    shiny::tags$style(
      htmltools::HTML("
      /* Overall font sizing */
      body { font-size: 0.92rem; }

      /* Make DT table body slightly smaller than the rest of the app */
      .dataTables_wrapper table.dataTable { font-size: 0.82rem; }

      /* Header filters (filter='top') */
      .dataTables_wrapper thead input {
        font-size: 0.78rem;
        padding: 2px 6px;
        height: 28px;
      }

      /* Tighten spacing so pagination hugs the table */
      .dataTables_wrapper .dt-top,
      .dataTables_wrapper .dt-bottom {
        padding: 6px 10px;
        margin: 0;
      }
      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate {
        margin: 0;
      }

      /* Keep the card from creating its own scroll */
      .bslib-card .card-body { overflow: visible; }
      ")
    )
  ),
  
  # ── Sidebar ──
  sidebar = bslib::sidebar(
    width = 320,
    title = "Filters",
    
    shiny::selectizeInput(
      "pesquisa",
      label = "Survey",
      choices = NULL,
      options = list(placeholder = "All surveys")
    ),
    
    shiny::textInput(
      "busca",
      label = "Search aggregate name",
      placeholder = "e.g. IPCA, abate, PIB..."
    ),
    
    shiny::actionButton(
      "clear_filters",
      "Clear all filters",
      class = "btn-sm btn-secondary w-100"
    ),
    
    shiny::hr(),
    
    shiny::markdown(
      "Use the filters above or the column search in the table.
      Click any row to copy the aggregate ID."
    )
  ),
  
  # ── Main panel ──
  bslib::layout_columns(
    col_widths = c(4, 4, 4),
    bslib::value_box(
      title = "Aggregates",
      value = shiny::textOutput("n_aggregates", inline = TRUE),
      #showcase = bsicons::bs_icon("table"),
      theme = "primary"
    ),
    bslib::value_box(
      title = "Surveys",
      value = shiny::textOutput("n_surveys", inline = TRUE),
      #showcase = bsicons::bs_icon("clipboard-data"),
      theme = "info"
    ),
    bslib::value_box(
      title = "Showing",
      value = shiny::textOutput("n_filtered", inline = TRUE),
      #showcase = bsicons::bs_icon("funnel"),
      theme = "dark"
    )
  ),
  
  bslib::card(
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center",
      "Aggregate catalog",
      shiny::downloadButton("download_csv", "CSV", class = "btn-sm btn-outline-primary")
    ),
    bslib::card_body(
      padding = 0,
      DT::DTOutput("tabela")
    ),
    full_screen = TRUE
  )
)


# ── Server ─────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  
  # Load data once
  dataset <- shiny::reactive({
    shiny::withProgress(message = "Fetching aggregates from IBGE...", {
      ibger::ibge_aggregates()
    })
  })
  
  # Populate survey dropdown after data loads
  shiny::observe({
    df <- dataset()
    nomes <- sort(unique(df$survey_name))
    shiny::updateSelectizeInput(
      session, "pesquisa",
      choices = c("All surveys" = "", nomes),
      server = TRUE
    )
  })
  
  # Filtered data
  df_filtrado <- shiny::reactive({
    df <- dataset()
    
    if (!is.null(input$pesquisa) && nzchar(input$pesquisa)) {
      df <- df[df$survey_name == input$pesquisa, ]
    }
    
    if (nzchar(input$busca)) {
      pattern <- input$busca
      df <- df[grepl(pattern, df$aggregate_name, ignore.case = TRUE), ]
    }
    
    df
  })
  
  # Value boxes
  output$n_aggregates <- shiny::renderText(nrow(dataset()))
  output$n_surveys    <- shiny::renderText(length(unique(dataset()$survey_name)))
  output$n_filtered   <- shiny::renderText(nrow(df_filtrado()))
  
  # Table
  output$tabela <- DT::renderDT({
    DT::datatable(
      df_filtrado(),
      colnames = c(
        "Survey ID"      = "survey_id",
        "Survey"         = "survey_name",
        "Aggregate ID"   = "aggregate_id",
        "Aggregate name" = "aggregate_name"
      ),
      selection = "single",
      filter = "top",
      rownames = FALSE,
      class = "compact stripe hover",
      options = list(
        autoWidth = TRUE, # Important for columnDefs width to work consistently
        columnDefs = list(
          list(className = 'dt-center', targets = 0:2), # Center align first three columns
          list(className = 'dt-left', targets = 3)   # Right align the remaining columns
        ),
        pageLength = 25,
        scrollX = TRUE,
        deferRender = TRUE,
        dom = "<'dt-top d-flex justify-content-between align-items-center'ip>t<'dt-bottom d-flex justify-content-between align-items-center'ip>",
        language = list(
          info = "Showing _START_ to _END_ of _TOTAL_ aggregates",
          infoFiltered = "(filtered from _MAX_)",
          infoEmpty = "No aggregates found",
          zeroRecords = "No aggregates match the current filters"
        )
      )
    )
  })
  
  # Clear all filters (sidebar + table header filters)
  shiny::observeEvent(input$clear_filters, {
    # Sidebar inputs
    shiny::updateSelectizeInput(session, "pesquisa", selected = "")
    shiny::updateTextInput(session, "busca", value = "")
    
    # DT filters (global + per-column + header inputs from filter='top')
    proxy <- DT::dataTableProxy("tabela")
    
    
    # Replace data (keeps table in sync and resets paging)
    DT::clearSearch(proxy)
    #DT::replaceData(proxy, df_filtrado(), resetPaging = TRUE, clearSelection = "all")
    
  })
  
  
  # Copy aggregate ID on row click
  shiny::observeEvent(input$tabela_rows_selected, {
    row <- df_filtrado()[input$tabela_rows_selected, ]
    id <- row$aggregate_id
    name <- row$aggregate_name
    shiny::showNotification(
      shiny::tagList(
        shiny::tags$b(paste0("Aggregate ", id)),
        shiny::br(),
        name,
        shiny::br(),
        shiny::tags$code(paste0('ibge_metadata(', id, ')'))
      ),
      type = "message",
      duration = 6
    )
  })
  
  # CSV download
  output$download_csv <- shiny::downloadHandler(
    filename = function() {
      paste0("ibge_aggregates_", Sys.Date(), ".csv")
    },
    content = function(file) {
      utils::write.csv(df_filtrado(), file, row.names = FALSE)
    }
  )
}

shiny::shinyApp(ui, server)
