
library(readxl)
#library(rio)
library(dplyr)
library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(plotly)
library(lubridate)
library(highcharter)
library(tidyr)
library(DT)
library(waiter)
library(shinycssloaders)
library(rio)
library(rmarkdown)
library(shinyjs)

local_file_path <- "data/online_retail_II.xlsx"

#use_waitress(color = "grey", percent_color = "black")

#waitress <- Waitress$new("#import")

df0910 <- readxl::read_excel(local_file_path, sheet = 1)
#waitress$inc(25)

df1011 <- readxl::read_excel(local_file_path, sheet = 2)
#waitress$inc(25)

retail_data <- rbind(df0910, df1011)
#waitress$inc(25)

retail_data <- retail_data %>%
  filter(!is.na("Customer ID")) %>%  # Remove missing customers
  mutate(Revenue = Quantity * Price) %>%  # Calculate revenue
  arrange(InvoiceDate) %>%  # Sorting revenue values (not required, just for data pre-visualization)
  mutate(InvoiceDateOnly = date(InvoiceDate), InvoiceYear = year(InvoiceDateOnly))
#waitress$inc(25)
#waitress$close()


waiting_screen <- tagList(
  spin_flower(),
  h4("Carefully preparing data...")
)

preloader <- list(html = tagList(spin_1(), "Loading ..."), color = "#343a40")

ui <- bs4DashPage(preloader = preloader,
                  #' # Include shinyjs
                  #' useShinyjs(),
                  #' 
                  #' # Add some CSS for the loading spinner
                  #' tags$head(
                  #'   tags$style(HTML("
                  #'     .spinner {
                  #'     display: none;
                  #'     border: 4px solid #f3f3f3;
                  #'     border-top: 4px solid #3498db;
                  #'     border-radius: 50%;
                  #'     width: 20px;
                  #'     height: 20px;
                  #'     animation: spin 1s linear infinite;
                  #'     margin-left: 10px;
                  #'     vertical-align: middle;
                  #'     }
                  #'     @keyframes spin {
                  #'     0% { transform: rotate(0deg); }
                  #'     100% { transform: rotate(360deg); }
                  #'     }
                  #'   "))
                  #' ),
                  dashboardHeader(title = "Online Retail Dashboard"),
                  bs4DashSidebar(width = '300px', minified = TRUE, expandOnHover = TRUE,
                    sidebarMenu(
                      menuItem("Revenue over Time", tabName = "rev-over-time", icon = icon("dashboard")),
                      menuItem("Top-selling Products", tabName = "top-sell-prod", icon = icon("dashboard")),
                      menuItem("Interactive Data Table", tabName = "inter-dt", icon = icon("th")),
                      br(),
                      pickerInput(
                        inputId = "filterByCountry", 
                        label = "Filter by Country:", 
                        choices = unique(retail_data$Country), 
                        options = pickerOptions(
                          actionsBox = TRUE, 
                          selectedTextFormat = "count > 2"
                        ), 
                        multiple = TRUE,
                        selected = "United Kingdom"
                      ),
                      br(),
                      #br(),
                      br(),
                      selectInput("fileType", "Select File Format:", choices = c("CSV" = "csv", "Excel" = "xlsx"), selected = "CSV"),
                      #br(),
                      downloadButton("downloadData", "Export Data"),
                      br(),
                      br(),
                      selectInput("reportFormat", "Select Report Format:", choices = c("HTML" = "html", "PDF" = "pdf", "Word" = "docx"), selected = "HTML"),
                      # Download button with inline spinner
                      div(
                        style = "display: inline-flex; align-items: center;",
                        downloadButton("download_report", "Download Report"),
                        div(id = "loading-spinner", class = "spinner")
                      )
                    )
                  ),
                  bs4DashBody(
                    tabItems(
                      tabItem(tabName = "rev-over-time",
                              fluidRow(
                                box(width = 12, withSpinner(plotlyOutput("lineChart")), title = "Interactive Sales Trend")
                              )
                      ),
                      tabItem(tabName = "top-sell-prod",
                              fluidRow(
                                box(width = 12, withSpinner(highchartOutput("stackedBarChart")), title = "Top-Selling Products")
                              )
                      ),
                      tabItem(tabName = "inter-dt",
                              fluidRow(
                                box(width = 12, withSpinner(DTOutput("table")), title = "Interactive Data Table")
                              )
                      )
                    )
                  )
)



server <- function(input, output) {

  observeEvent(
    input$filterByCountry, {
      waiter_show(html = waiting_screen, color = "grey")

  filtered_data1 <- reactive({
    req(input$filterByCountry)
    retail_data %>%
      filter(Country %in% input$filterByCountry) %>%
      group_by(InvoiceDateOnly) %>%
      summarise(RevenueDay = sum(Revenue))
  })

  output$lineChart <- renderPlotly({
    plot_ly(filtered_data1(), x = ~filtered_data1()$InvoiceDateOnly, y = ~filtered_data1()$RevenueDay, type = 'scatter', mode = 'lines', 
            line = list(color = 'blue')) %>%
              layout(title = "Revenue by Invoice Date",
                xaxis = list(title = "Invoice Date"),
                yaxis = list(title = "Daily Revenue (Qty. x Unit Price)"))
  })  

  filtered_data2 <- reactive({
    req(input$filterByCountry)
    retail_data %>%
      filter(Country %in% input$filterByCountry) %>%
      group_by(Description, InvoiceYear) %>%
      summarise(YearQuantity = sum(Quantity)) %>%
#      arrange(Description, desc(YearQuantity)) %>%
#      slice_head(n = 3) %>%
      filter(YearQuantity >= 1000) %>%
      mutate(InvoiceYear = paste0("Y", InvoiceYear)) %>%
      pivot_wider(names_from = InvoiceYear, values_from = YearQuantity)
  })

  output$stackedBarChart <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Top Selling Products in 2009-2011, per Country") %>%
      hc_xAxis(title = list(text = "Product Descriptions"),
               categories = filtered_data2()$Description) %>%
      hc_yAxis(
        title = list(text = "Quantity Sold"),
        stackLabels = list(enabled = TRUE),
        reversed = FALSE  # Does not reverse the stack order (larger items at the bottom)
      ) %>%
      hc_plotOptions(
        series = list(
          stacking = "normal"
        )
      ) %>%
      hc_add_series(name = "2009", data = filtered_data2()$Y2009) %>%
      hc_add_series(name = "2010", data = filtered_data2()$Y2010) %>%
      hc_add_series(name = "2011", data = filtered_data2()$Y2011)
  })

  filtered_data3 <- reactive({
    req(input$filterByCountry)
    retail_data %>%
      filter(Country %in% input$filterByCountry) %>%
      select(Invoice, InvoiceDateOnly, StockCode, Description, Quantity, Price, "Customer ID", Country)
  })
  
  output$table <- renderDataTable({
    datatable(filtered_data3(), filter = "top", colnames = c("Invoice ID",
                                                             "Invoice Date",
                                                             "Product Stock Code",
                                                             "Product Description",
                                                             "Product Quantity",
                                                             "Product Unit Price",
                                                             "Customer ID",
                                                             "Country"),
              rownames = FALSE,
              options = list(pageLength = 6)
    ) %>%
      formatCurrency("Price", " €", digits = 2)
  })

  fileContent <- reactive({
    retail_data %>%
      filter(Country %in% input$filterByCountry)
  })

  output$downloadData <- downloadHandler(
    filename <- function() {
      paste(paste(input$filterByCountry, collapse = "-"), "-", Sys.Date(), ".", input$fileType, sep = "")
    },
    content <- function(file) {
      rio::export(fileContent(), file, format = input$fileType)
    }
  )

  # Download handler for the R Markdown report (HTML)
  output$download_report <- downloadHandler(
    filename = function() {
      paste("report-", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      # Show the spinner and disable the button during rendering
      shinyjs::show("loading-spinner")
      shinyjs::disable("download_report")
      
      # Ensure the spinner hides and button re-enables even if rendering fails
      on.exit({
        shinyjs::hide("loading-spinner")
        shinyjs::enable("download_report")
      })
      
      req(data())
      # Render the R Markdown file
      rmarkdown::render(
        input = "www/rmd_template.Rmd",
        output_file = file,
        params = list(data = filtered_data1()),
        envir = new.env(parent = globalenv())
      )
    }
  )
  
    Sys.sleep(1.0)
    waiter_hide()}
  )
}



# Shiny app ----
#shinyApp(ui, server, options = list(display.mode = "showcase"))
shinyApp(ui, server)


