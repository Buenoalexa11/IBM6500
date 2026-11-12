# Shopify Sales Explorer: interactive Shiny app
# Alexa Bueno | IBM 6540 W07-M10 Dashboards Assignment

library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(plotly)
library(scales)

# ---- Data -------------------------------------------------------------------
sales <- read_csv("shopify_sales_dataset_ml_eda.csv", show_col_types = FALSE) |>
  mutate(order_date = as.Date(order_date))

breakdowns <- c(
  "Traffic source"   = "traffic_source",
  "Product category" = "product_category",
  "Country"          = "customer_country",
  "Payment method"   = "payment_method"
)

metrics <- c(
  "Revenue"             = "revenue",
  "Profit"              = "profit",
  "Orders"              = "orders",
  "Average order value" = "aov",
  "Return rate"         = "return_rate",
  "Average rating"      = "rating"
)

# Summarise every metric for whatever grouping is passed in
summarise_metrics <- function(df) {
  df |>
    summarise(
      revenue     = sum(revenue),
      profit      = sum(profit),
      orders      = n(),
      aov         = mean(revenue),
      return_rate = mean(is_returned),
      rating      = mean(rating),
      .groups = "drop"
    )
}

# Pick the right number format for each metric
metric_label <- function(metric) {
  switch(metric,
    revenue     = label_dollar(scale_cut = cut_short_scale()),
    profit      = label_dollar(scale_cut = cut_short_scale()),
    aov         = label_dollar(),
    return_rate = label_percent(accuracy = 0.1),
    rating      = label_number(accuracy = 0.01),
    label_comma()
  )
}

# ---- UI ---------------------------------------------------------------------
ui <- page_sidebar(
  title = "Shopify Sales Explorer (2023–2025)",
  theme = bs_theme(bootswatch = "cosmo"),

  sidebar = sidebar(
    width = 300,
    dateRangeInput("dates", "Date range",
                   start = min(sales$order_date), end = max(sales$order_date),
                   min   = min(sales$order_date), max = max(sales$order_date)),
    selectInput("metric", "Metric", choices = metrics, selected = "revenue"),
    selectInput("breakdown", "Compare by", choices = breakdowns,
                selected = "traffic_source"),
    selectizeInput("groups", "Show these groups", choices = NULL, multiple = TRUE),
    radioButtons("grain", "Time grouping",
                 choices = c("Week" = "week", "Month" = "month", "Quarter" = "quarter"),
                 selected = "month", inline = TRUE),
    hr(),
    p(class = "small text-muted",
      "Data: 60,000 Shopify orders, Jan 2023 – Jun 2025 (IBM 6540 Group Project).")
  ),

  layout_columns(
    fill = FALSE,
    value_box("Revenue",             textOutput("vb_revenue"), theme = "primary"),
    value_box("Orders",              textOutput("vb_orders"),  theme = "info"),
    value_box("Average order value", textOutput("vb_aov"),     theme = "success"),
    value_box("Return rate",         textOutput("vb_return"),  theme = "warning")
  ),

  layout_columns(
    col_widths = c(8, 4),
    card(card_header(textOutput("trend_title")), plotlyOutput("trend")),
    card(card_header(textOutput("bar_title")),   plotlyOutput("bars"))
  ),

  card(
    card_header("How to use this app"),
    p("This app lets you explore how a Shopify store's sales change over time and across marketing channels,",
      "product categories, countries, and payment methods."),
    tags$ol(
      tags$li(strong("Pick a date range"), " to focus on a season, a year, or a campaign period."),
      tags$li(strong("Pick a metric"), " such as revenue, return rate, or average rating."),
      tags$li(strong("Pick what to compare"), " and choose which groups to show."),
      tags$li("Hover over the charts to see exact values. Drag on the trend chart to zoom in.")
    )
  )
)

# ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {

  # Refresh the group choices whenever the breakdown changes
  observeEvent(input$breakdown, {
    choices <- sort(unique(sales[[input$breakdown]]))
    updateSelectizeInput(session, "groups", choices = choices, selected = choices)
  })

  filtered <- reactive({
    req(input$dates, input$groups)
    sales |>
      filter(order_date >= input$dates[1],
             order_date <= input$dates[2],
             .data[[input$breakdown]] %in% input$groups)
  })

  metric_name    <- reactive(names(metrics)[metrics == input$metric])
  breakdown_name <- reactive(names(breakdowns)[breakdowns == input$breakdown])

  # Value boxes
  totals <- reactive({
    validate(need(nrow(filtered()) > 0, "No orders match these filters."))
    summarise_metrics(filtered())
  })
  output$vb_revenue <- renderText(dollar(totals()$revenue, scale_cut = cut_short_scale()))
  output$vb_orders  <- renderText(comma(totals()$orders))
  output$vb_aov     <- renderText(dollar(totals()$aov, accuracy = 1))
  output$vb_return  <- renderText(percent(totals()$return_rate, accuracy = 0.1))

  output$trend_title <- renderText(paste(metric_name(), "over time by", tolower(breakdown_name())))
  output$bar_title   <- renderText(paste(metric_name(), "by", tolower(breakdown_name())))

  # Trend chart
  output$trend <- renderPlotly({
    validate(need(nrow(filtered()) > 0, "No orders match these filters."))
    trend <- filtered() |>
      mutate(period = floor_date(order_date, input$grain),
             group  = .data[[input$breakdown]]) |>
      group_by(period, group) |>
      summarise_metrics()

    p <- ggplot(trend, aes(period, .data[[input$metric]], color = group,
                           text = paste0(group, "<br>", period, "<br>",
                                         metric_label(input$metric)(.data[[input$metric]])))) +
      geom_line(aes(group = group)) +
      scale_y_continuous(labels = metric_label(input$metric)) +
      labs(x = NULL, y = metric_name(), color = breakdown_name()) +
      theme_minimal()

    ggplotly(p, tooltip = "text")
  })

  # Bar chart
  output$bars <- renderPlotly({
    validate(need(nrow(filtered()) > 0, "No orders match these filters."))
    bars <- filtered() |>
      mutate(group = .data[[input$breakdown]]) |>
      group_by(group) |>
      summarise_metrics() |>
      mutate(group = reorder(group, .data[[input$metric]]))

    p <- ggplot(bars, aes(.data[[input$metric]], group,
                          text = metric_label(input$metric)(.data[[input$metric]]))) +
      geom_col(fill = "#2780e3") +
      scale_x_continuous(labels = metric_label(input$metric)) +
      labs(x = metric_name(), y = NULL) +
      theme_minimal()

    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui, server)
