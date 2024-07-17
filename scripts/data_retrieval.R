# Author: gp1981
# Date: 16 July 2024
# Purpose: Load all libraries and files
# Disclaimer: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.

# 01 - Get data from fmp  ------------------------------------------------------

get_stock_data_df <- function(API_Key){
  # Prepare URL for accessing Symbol List API
  Symbol_List_API_path <- "https://financialmodelingprep.com/api/v3/stock/list?apikey="
  Symbol_List_API_path <- paste0(Symbol_List_API_path, API_Key)
  
  # Download stock list data via API
  Symbol_List <- fromJSON(Symbol_List_API_path)
  
  # Download financial statement symbol lists via API
  Financial_Statement_Symbol_List <- "https://financialmodelingprep.com/api/v3/financial-statement-symbol-lists?apikey="
  Financial_Statement_Symbol_List <- paste0(Financial_Statement_Symbol_List, API_Key)
  Financial_Statement_Symbol_List <- fromJSON(Financial_Statement_Symbol_List)
  
  
  # Download S&P500 constituent list via API
  SP500_Symbol_list <- "https://financialmodelingprep.com/api/v3/sp500_constituent?apikey="
  SP500_Symbol_list <- paste0(SP500_Symbol_list, API_Key)
  SP500_Symbol_list <- fromJSON(SP500_Symbol_list)
  
  # Download NASDAQ constituent list via API
  NASDAQ_Symbol_list <- "https://financialmodelingprep.com/api/v3/nasdaq_constituent?apikey="
  NASDAQ_Symbol_list <- paste0(NASDAQ_Symbol_list, API_Key)
  NASDAQ_Symbol_list <- fromJSON(NASDAQ_Symbol_list)
  
  # Download Dow Jones constituent list via API
  DOW_Symbol_list <- "https://financialmodelingprep.com/api/v3/dowjones_constituent?apikey="
  DOW_Symbol_list <- paste0(DOW_Symbol_list, API_Key)
  DOW_Symbol_list <- fromJSON(DOW_Symbol_list)
  
  # Convert JSON data into data frames
  symbol_df <- as.data.frame(Symbol_List)
  Financial_Statement_symbol_df <- as.data.frame(Financial_Statement_Symbol_List)
  SP500_symbol_df <- as.data.frame(SP500_Symbol_list)
  NASDAQ_symbol_df <- as.data.frame(NASDAQ_Symbol_list)
  DOW_symbol_df <- as.data.frame(DOW_Symbol_list)
  
  # Add a column to mark only stocks with financial statements
  Financial_Statement_symbol_df <- Financial_Statement_symbol_df %>% 
    mutate(Financial_statements = 'TRUE') %>% 
    rename(symbol = Financial_Statement_Symbol_List)
  
  # Add a column to mark only the S&P500, NASDAQ and Dow Jones stocks
  SP500_symbol_df <- SP500_symbol_df %>% 
    mutate(SP500 = 'S&P500') %>% 
    select(-name, -dateFirstAdded)
  
  NASDAQ_symbol_df <- NASDAQ_symbol_df %>% 
    mutate(NASDAQ = 'NASDAQ') %>% 
    select(-c("name","dateFirstAdded", "sector","subSector","headQuarter","cik", "founded"))
  
  DOW_symbol_df <- DOW_symbol_df %>% 
    mutate(Dow_Jones = 'DOW') %>% 
    select(-c("name","dateFirstAdded", "sector","subSector","headQuarter","cik", "founded"))
  
  # Merge stock data with financial statement symbol list and key indexes
  symbol_df  <- semi_join(symbol_df , Financial_Statement_symbol_df, by = "symbol")
  symbol_df  <- left_join(symbol_df , SP500_symbol_df, by = "symbol")
  symbol_df  <- left_join(symbol_df , NASDAQ_symbol_df, by = "symbol" )
  symbol_df  <- left_join(symbol_df , DOW_symbol_df, by = "symbol")
  
  return(symbol_df)
}

get_fundamentals_data_df <- function(symbols_df, period, limit, API_Key){
  
  # Create API URLs for various calls to collect Financial Statements
  API_IncomeStatement_path_base <- 'https://financialmodelingprep.com/api/v3/income-statement/'
  API_BalanceSheet_path_base <- 'https://financialmodelingprep.com/api/v3/balance-sheet-statement/'
  API_CashFlow_path_base <- 'https://financialmodelingprep.com/api/v3/cash-flow-statement/'
  API_KeyMetrics_path_base <- 'https://financialmodelingprep.com/api/v3/key-metrics/'
  API_Profile_path_base <- 'https://financialmodelingprep.com/api/v3/profile/'
  API_Ratio_path_base <- 'https://financialmodelingprep.com/api/v3/ratios/'
  
  if (period == "quarter") {
    API_IncomeStatement_path_suffix <- '?period=quarter'
    API_BalanceSheet_path_suffix <- '?period=quarter'
    API_CashFlow_path_suffix <- '?period=quarter'
    API_KeyMetrics_path_suffix <- '?period=quarter'
    API_Ratio_path_suffix <- '?period=quarter'
  } else {
    API_IncomeStatement_path_suffix <- '?period=annual'
    API_BalanceSheet_path_suffix <- '?period=annual'
    API_CashFlow_path_suffix <- '?period=annual'
    API_KeyMetrics_path_suffix <- '?period=annual'
    API_Ratio_path_suffix <- '?period=annual'
  }
  
  API_IncomeStatement_path <- paste0(API_IncomeStatement_path_base, symbols_df$symbol, API_IncomeStatement_path_suffix, '&limit=', limit, '&apikey=', API_Key)
  API_BalanceSheet_path <- paste0(API_BalanceSheet_path_base, symbols_df$symbol, API_BalanceSheet_path_suffix, '&limit=', limit, '&apikey=', API_Key)
  API_CashFlow_path <- paste0(API_CashFlow_path_base, symbols_df$symbol, API_CashFlow_path_suffix, '&limit=', limit, '&apikey=', API_Key)
  API_KeyMetrics_path <- paste0(API_KeyMetrics_path_base, symbols_df$symbol, API_KeyMetrics_path_suffix, '&limit=', limit, '&apikey=', API_Key)
  API_Profile_path <- paste0(API_Profile_path_base, symbols_df$symbol, '?apikey=', API_Key)
  API_Ratio_path <- paste0(API_Ratio_path_base, symbols_df$symbol, API_Ratio_path_suffix, '&apikey=', API_Key)
  
  # Progress bar
  total_symbols <- nrow(symbols_df) * 6 # Adjust the total to the number of different data
  pb <- progress_bar$new(
    format = "  [:bar] :percent in :elapsed",
    total = total_symbols, 
    width = 60
  )
  
  # Function to retrieve all statements, key metrics, profile and ratios from symbols_df
  fetch_fundamentals_data <- function(paths, type) {
    bind_rows(lapply(1:length(paths), function(x) {
      pb$tick()
      tryCatch({
        data <- fromJSON(paths[x])
        if (length(data) == 0) {
          NULL
        } else {
          data.frame(data)
        }
      }, error = function(cond) {
        message(paste("API provided an error for", type, "Ticker:", symbols_df$symbol[x]))
        message("Here's the original error message:")
        message(cond)
        return(NULL)
      }, warning = function(cond) {
        message(paste("API provided a warning for", type, "Ticker:", symbols_df$symbol[x]))
        message("Here's the original warning message:")
        message(cond)
        return(NULL)
      })
    }))
  }
  
  IS <- fetch_fundamentals_data(API_IncomeStatement_path, "Income Statement")
  BS <- fetch_fundamentals_data(API_BalanceSheet_path, "Balance Sheet")
  CF <- fetch_fundamentals_data(API_CashFlow_path, "Cash Flow")
  KeyMetrics <- fetch_fundamentals_data(API_KeyMetrics_path, "Key Metrics")
  Profile <- fetch_fundamentals_data(API_Profile_path, "Profile data")
  Ratios <- fetch_fundamentals_data(API_Ratio_path, "Ratios")
  
  # Formatting data
  IS <- IS %>% 
    mutate(across(c(date,fillingDate,acceptedDate), as.Date)) %>% 
    mutate_at(vars(calendarYear), as.integer)
  
  BS <- BS %>% 
    mutate(across(c(date,fillingDate,acceptedDate), as.Date)) %>% 
    mutate_at(vars(calendarYear), as.integer)
  
  CF <- CF %>% 
    mutate(across(c(date,fillingDate,acceptedDate), as.Date)) %>% 
    mutate_at(vars(calendarYear), as.integer)
  
  Ratios <- Ratios %>% 
    mutate_at(vars(date), as.Date) %>% 
    mutate_at(vars(calendarYear), as.integer)
  
  KeyMetrics <- KeyMetrics %>% 
    mutate_at(vars(date), as.Date) %>% 
    mutate_at(vars(calendarYear), as.integer)
  
  # Combine all data into a dataframe or a single data frame, depending on your needs
  symbols_df <- left_join(symbols_df, IS) %>% select(-price)
  symbols_df <- left_join(symbols_df, BS) 
  symbols_df <- left_join(symbols_df, CF) 
  symbols_df <- left_join(symbols_df, KeyMetrics) 
  symbols_df <- left_join(symbols_df, Profile) 
  symbols_df <- left_join(symbols_df, Ratios) 
  
  return(symbols_df)
}

get_financial_statements_as_reported_list <- function(symbols_df, period, limit, API_Key) {
  
  # Base URL for the API
  API_financial_as_reported_base <- 'https://financialmodelingprep.com/api/v3/financial-statement-full-as-reported/'
  
  # URL suffix based on the period
  API_financial_as_reported_suffix <- ifelse(period == "quarter", '?period=quarter', '?period=annual')
  
  # Construct full API URLs
  API_paths <- paste0(API_financial_as_reported_base, symbols_df$symbol, API_financial_as_reported_suffix, '&limit=', limit, '&apikey=', API_Key)
  
  # Progress bar
  total_symbols <- nrow(symbols_df)
  pb <- progress::progress_bar$new(
    format = "  [:bar] :percent in :elapsed",
    total = total_symbols,
    width = 60
  )
  
  # Function to fetch data from JSON URLs
  fetch_fundamentals_as_reported <- function(paths) {
    data_list <- lapply(1:length(paths), function(i) {
      pb$tick()
      tryCatch({
        data <- fromJSON(paths[i])
        if (length(data) > 0) {
          data_frame <- data.frame(data)
          data_frame$date <- data_frame$date %>% as.Date()
          return(data_frame)
        } else {
          return(NULL)
        }
      }, error = function(cond) {
        message(paste("API provided an error for Ticker:", symbols_df$symbol[i]))
        message("Error message:", cond)
        return(NULL)
      }, warning = function(cond) {
        message(paste("API provided a warning for Ticker:", symbols_df$symbol[i]))
        message("Warning message:", cond)
        return(NULL)
      })
    })
    names(data_list) <- symbols_df$symbol
    return(data_list)
  }
  
  # Fetch data
  data_list <- fetch_fundamentals_as_reported(API_paths)
  
  # Combine all data frames into a list
  df_list <- lapply(data_list, function(df) {
    if (!is.null(df)) {
      return(df)
    } else {
      return(data.frame())
    }
  })
  

  # Return the list of data frames
  return(df_list)
}

get_price_history_data_df <- function(symbols_df, startDate, endDate , API_Key){
  
  # Create API URLs for various calls to collect historical price
  API_historical_price_path_base <- 'https://financialmodelingprep.com/api/v3/historical-price-full/'
  API_historical_price_path <- paste0(API_historical_price_path_base, symbols_df$symbol, '?from=', startDate, '&to=', endDate,  '&apikey=', API_Key)
  
  # Progress bar
  total_symbols <- nrow(symbols_df) # Adjust the total to the number of different data
  pb <- progress_bar$new(
    format = "  [:bar] :percent in :elapsed",
    total = total_symbols, 
    width = 60
  )
  
  # Function to retrieve all statements, key metrics, profile and ratios from symbols_df
  fetch_historical_price_data <- function(paths) {
    bind_rows(lapply(1:length(paths), function(x) {
      pb$tick()
      tryCatch({
        data <- fromJSON(paths[x])
        if (length(data) == 0) {
          NULL
        } else {
          data.frame(data)
        }
      }, error = function(cond) {
        message(paste("API provided an error for", type, "Ticker:", symbols_df$symbol[x]))
        message("Here's the original error message:")
        message(cond)
        return(NULL)
      }, warning = function(cond) {
        message(paste("API provided a warning for", type, "Ticker:", symbols_df$symbol[x]))
        message("Here's the original warning message:")
        message(cond)
        return(NULL)
      })
    }))
  }
  
  historical_price_df <- fetch_historical_price_data(API_historical_price_path) %>% as.data.frame()
  
}

# 02 - Analysis  ---------------------------------------------------------------
history_total_equity_book_value <- function(symbols_df, fundamentals_df, financial_statements_as_reported_list){
  # calculation to include:
  # fundamentals_df$totalStockholdersEquity
  
}