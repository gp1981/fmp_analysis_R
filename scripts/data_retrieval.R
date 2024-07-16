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
  Symbol_df <- as.data.frame(Symbol_List)
  Financial_Statement_Symbol_df <- as.data.frame(Financial_Statement_Symbol_List)
  SP500_Symbol_df <- as.data.frame(SP500_Symbol_list)
  NASDAQ_Symbol_df <- as.data.frame(NASDAQ_Symbol_list)
  DOW_Symbol_df <- as.data.frame(DOW_Symbol_list)
  
  # Add a column to mark only stocks with financial statements
  Financial_Statement_Symbol_df <- Financial_Statement_Symbol_df %>% 
    mutate(Financial_statements = 'TRUE') %>% 
    rename(symbol = Financial_Statement_Symbol_List)
  
  # Add a column to mark only the S&P500, NASDAQ and Dow Jones stocks
  SP500_Symbol_df <- SP500_Symbol_df %>% 
    mutate(SP500 = 'S&P500') %>% 
    select(-name, -dateFirstAdded)
  
  NASDAQ_Symbol_df <- NASDAQ_Symbol_df %>% 
    mutate(NASDAQ = 'NASDAQ') %>% 
    select(-c("name","dateFirstAdded", "sector","subSector","headQuarter","cik", "founded"))
  
  DOW_Symbol_df <- DOW_Symbol_df %>% 
    mutate(Dow_Jones = 'DOW') %>% 
    select(-c("name","dateFirstAdded", "sector","subSector","headQuarter","cik", "founded"))
  
  # Merge stock data with financial statement symbol list and key indexes
  Symbol_df  <- semi_join(Symbol_df , Financial_Statement_Symbol_df, by = "symbol")
  Symbol_df  <- left_join(Symbol_df , SP500_Symbol_df, by = "symbol")
  Symbol_df  <- left_join(Symbol_df , NASDAQ_Symbol_df, by = "symbol" )
  Symbol_df  <- left_join(Symbol_df , DOW_Symbol_df, by = "symbol")
  
  return(Symbol_df)
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
  fetch_data <- function(paths, type) {
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
  
  IS <- fetch_data(API_IncomeStatement_path, "Income Statement")
  BS <- fetch_data(API_BalanceSheet_path, "Balance Sheet")
  CF <- fetch_data(API_CashFlow_path, "Cash Flow")
  KeyMetrics <- fetch_data(API_KeyMetrics_path, "Key Metrics")
  Profile <- fetch_data(API_Profile_path, "Profile data")
  Ratios <- fetch_data(API_Ratio_path, "Ratios")
  
    # Combine all data into a dataframe or a single data frame, depending on your needs
  symbols_df1 <- left_join(symbols_df, fundamentals_df$IS) 
  symbols_df1 <- left_join(symbols_df1, fundamentals_df$BS)
  symbols_df1 <- left_join(symbols_df1, fundamentals_df$CF)
  symbols_df1 <- left_join(symbols_df1, fundamentals_df$KeyMetrics)
  symbols_df1 <- left_join(symbols_df1, fundamentals_df$Profile)
  symbols_df1 <- left_join(symbols_df1, fundamentals_df$Ratios)
}
  
   

get_price_history_data <- function(symbol, API_Key){
  
}