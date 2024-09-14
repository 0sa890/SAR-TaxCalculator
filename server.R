library(shiny)
library(readxl)
library(ggplot2)
library(DT)
library(dplyr)

# Placeholder for real-time exchange rates
get_exchange_rate <- function(currency) {
  rates <- c(Bitcoin = 45000, Ethereum = 3000, Litecoin = 150)
  return(rates[currency])
}

# Placeholder for historical data
get_historical_data <- function(currency) {
  dates <- seq.Date(from = Sys.Date() - 30, by = "day", length.out = 30)
  prices <- runif(30, min = 10000, max = 60000)  # Random prices for illustration
  data.frame(Date = dates, Price = prices)
}

# Function to calculate capital gains tax
calculate_tax <- function(gain, tax_rate, is_long_term) {
  if (is_long_term) {
    return(gain * (tax_rate / 100) * 0.8)  # 20% discount for long-term gains
  } else {
    return(gain * (tax_rate / 100))
  }
}

# Define Server logic
server <- function(input, output) {
  
  calculate_and_display <- reactive({
    if (is.null(input$file)) {
      # Manual input calculation
      amount <- input$amount
      buyPrice <- input$buyPrice
      sellPrice <- input$sellPrice
      gain <- (sellPrice - buyPrice) * amount
      tax <- calculate_tax(gain, input$taxRate, input$isLongTerm)
      
      data <- data.frame(
        Currency = input$currency,
        Amount = amount,
        BuyPrice = buyPrice,
        SellPrice = sellPrice,
        Gain = gain,
        TaxRate = input$taxRate,
        TaxOwed = tax,
        BuyDate = as.character(input$buyDate),
        SellDate = as.character(input$sellDate)
      )
      
      list(data = data, gain = gain, tax = tax)
      
    } else {
      # File upload handling
      file_ext <- tools::file_ext(input$file$datapath)
      
      # Read the file based on extension
      uploaded_data <- switch(file_ext,
                              "csv" = read.csv(input$file$datapath),
                              "xlsx" = read_excel(input$file$datapath),
                              {
                                showNotification("Invalid file format", type = "error")
                                return(NULL)
                              })
      
      # Validate columns
      if (!all(c("Amount", "BuyPrice", "SellPrice", "TaxRate", "Currency", "BuyDate", "SellDate", "IsLongTerm") %in% colnames(uploaded_data))) {
        showNotification("Invalid file structure", type = "error")
        return(NULL)
      }
      
      # Calculate tax for each row in the uploaded data
      uploaded_data$Gain <- (uploaded_data$SellPrice - uploaded_data$BuyPrice) * uploaded_data$Amount
      uploaded_data$TaxOwed <- mapply(calculate_tax, uploaded_data$Gain, uploaded_data$TaxRate, uploaded_data$IsLongTerm)
      
      # Group by Cryptocurrency and calculate separate totals
      aggregated_data <- uploaded_data %>%
        group_by(Currency) %>%
        summarise(
          TotalGain = sum(Gain),
          TotalTaxOwed = sum(TaxOwed),
          TotalVolume = sum(Amount),
          .groups = 'drop'
        )
      
      list(data = uploaded_data, aggregated = aggregated_data)
    }
  })
  
  observeEvent(input$calculate, {
    calc_data <- calculate_and_display()
    
    output$result <- renderText({
      if (!is.null(calc_data)) {
        paste("Cryptocurrency Calculations:", 
              paste(calc_data$data$Currency, ": Total Gain: $", round(calc_data$aggregated$TotalGain, 2),
                    ", Total Tax Owed: $", round(calc_data$aggregated$TotalTaxOwed, 2), collapse = "\n"))
      } else {
        "No data available for calculation."
      }
    })
    
    output$gainPlot <- renderPlot({
      ggplot(calc_data$data, aes(x = as.Date(BuyDate), y = Gain, color = Currency)) +
        geom_line() +
        geom_point() +
        labs(title = "Gain/Loss Over Time by Cryptocurrency", x = "Date", y = "Gain/Loss (USD)")
    })
    
    output$transactionTable <- renderDT({
      datatable(calc_data$data)
    })
    
    output$volumePieChart <- renderPlot({
      ggplot(calc_data$aggregated, aes(x = "", y = TotalVolume, fill = Currency)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start = 0) +
        theme_minimal() +
        labs(title = "Volume Percentage by Cryptocurrency", x = NULL, y = NULL) +
        theme(axis.text.x = element_blank())
    })
    
    output$taxPieChart <- renderPlot({
      ggplot(calc_data$aggregated, aes(x = "", y = TotalTaxOwed, fill = Currency)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start = 0) +
        theme_minimal() +
        labs(title = "Tax Percentage by Cryptocurrency", x = NULL, y = NULL) +
        theme(axis.text.x = element_blank())
    })
  })
  
  # Historical Price Trend Plot
  output$historicalPricePlot <- renderPlot({
    currency <- input$trendCrypto
    data <- get_historical_data(currency)
    
    ggplot(data, aes(x = Date, y = Price)) +
      geom_line(color = "blue") +
      labs(title = paste("Historical Price Trend of", currency), x = "Date", y = "Price (USD)") +
      theme_minimal()
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("Tax_Report_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      params <- calculate_and_display()
      
      rmarkdown::render(tempReport, output_file = file, params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
}