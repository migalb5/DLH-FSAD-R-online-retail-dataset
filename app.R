
library(readxl)
#library(rio)
library(dplyr)
library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(plotly)

local_file_path <- "data/online_retail_II.xlsx"

df0910 <- readxl::read_excel(local_file_path, sheet = 1)
df1011 <- readxl::read_excel(local_file_path, sheet = 2)

retail_data <- rbind(df0910, df1011)

retail_data <- retail_data %>%
  filter(!is.na("Customer ID")) %>%  # Remove missing customers
  mutate(Revenue = Quantity * Price) %>%  # Calculate revenue
  arrange(InvoiceDate)  # Sorting revenue values according to invoice date (over time)




ui <- bs4DashPage(dashboardHeader(title = "Online Retail Dashboard"),
                  bs4DashSidebar(width = '300px', minified = TRUE, expandOnHover = TRUE,
                    sidebarMenu(
                      menuItem("Revenue over Time", tabName = "rev-over-time", icon = icon("dashboard")),
#                      menuItem("Table", tabName = "table", icon = icon("th")),
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
                      )
                    )
                  ),
                  bs4DashBody(
                    tabItems(
                      tabItem(tabName = "rev-over-time",
                              fluidRow(
                                box(width = 12, plotlyOutput("lineChart"), title = "Interactive Sales Trend")
                              )
                      )
                      # tabItem(tabName = "table",
                      #         fluidRow(
                      #           box(tableOutput("view"), title = "Table")
                      #         )
                    )
                  )
)



server <- function(input, output) {
  
  filtered_data <- reactive({
    req(input$filterByCountry)
    retail_data %>%
      filter(Country %in% input$filterByCountry)
  })

  output$lineChart <- renderPlotly({
    plot_ly(filtered_data(), x = ~filtered_data()$InvoiceDate, y = ~filtered_data()$Revenue, type = 'scatter', mode = 'lines', 
            line = list(color = 'blue')) %>%
              layout(title = "Revenue by Invoice Date",
                xaxis = list(title = "Invoice Date"),
                yaxis = list(title = "Revenue (Qty. x Unit Price)"))
  })  
}



# Shiny app ----
#shinyApp(ui, server, options = list(display.mode = "showcase"))
shinyApp(ui, server)


