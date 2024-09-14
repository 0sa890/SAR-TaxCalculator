# ui.R
library(shinydashboard)

# Define UI with shinydashboard
ui <- dashboardPage(
  dashboardHeader(title = "Cryptocurrency Tax Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tax Calculator", tabName = "tax_calculator", icon = icon("calculator")),
      menuItem("Transaction Overview", tabName = "transaction_overview", icon = icon("table")),
      menuItem("Gain/Loss Analysis", tabName = "gain_loss_analysis", icon = icon("chart-line")),
      menuItem("Cryptocurrency Overview", tabName = "crypto_overview", icon = icon("chart-pie")),
      menuItem("Historical Price Trend", tabName = "historical_price_trend", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Dashboard 1: Tax Calculator
      tabItem(tabName = "tax_calculator",
              fluidPage(
                titlePanel("Enhanced Cryptocurrency Tax Calculator"),
                
                sidebarLayout(
                  sidebarPanel(
                    selectInput("currency", "Select Cryptocurrency:", choices = c("Bitcoin", "Ethereum", "Litecoin")),
                    numericInput("amount", "Amount of Cryptocurrency:", value = 1, min = 0),
                    numericInput("buyPrice", "Purchase Price per Unit (USD):", value = 0),
                    numericInput("sellPrice", "Selling Price per Unit (USD):", value = 0),
                    dateInput("buyDate", "Purchase Date:"),
                    dateInput("sellDate", "Selling Date:"),
                    numericInput("taxRate", "Tax Rate (%):", value = 15, min = 0),
                    checkboxInput("isLongTerm", "Long-Term Capital Gain", value = FALSE),
                    fileInput("file", "Or Upload CSV File", accept = c(".csv", ".xlsx")),
                    actionButton("calculate", "Calculate Tax"),
                    downloadButton("downloadReport", "Download Tax Report")
                  ),
                  mainPanel(
                    h3("Tax Calculation"),
                    verbatimTextOutput("result"),
                    DTOutput("transactionTable")
                  )
                )
              )
      ),
      
      # Dashboard 2: Transaction Overview
      tabItem(tabName = "transaction_overview",
              fluidPage(
                h2("Transaction Overview"),
                DTOutput("transactionTable")
              )
      ),
      
      # Dashboard 3: Gain/Loss Analysis
      tabItem(tabName = "gain_loss_analysis",
              fluidPage(
                h2("Gain/Loss Analysis"),
                plotOutput("gainPlot")
              )
      ),
      
      # Dashboard 4: Cryptocurrency Overview (Combined Pie Charts)
      tabItem(tabName = "crypto_overview",
              fluidPage(
                h2("Cryptocurrency Overview"),
                fluidRow(
                  column(6, plotOutput("volumePieChart")),
                  column(6, plotOutput("taxPieChart"))
                )
              )
      ),
      
      # Dashboard 5: Historical Price Trend
      tabItem(tabName = "historical_price_trend",
              fluidPage(
                h2("Historical Price Trend"),
                fluidRow(
                  column(6, selectInput("trendCrypto", "Select Cryptocurrency:", choices = c("Bitcoin", "Ethereum", "Litecoin"))),
                  column(6, selectInput("timeRange", "Select Time Range:", choices = c("Last 7 Days", "Last 30 Days", "Last 1 Year")))
                ),
                plotOutput("historicalPricePlot")
              )
      )
    )
  )
)