# Author: gp1981
# Date: 16 July 2024
# Purpose: Perform analysis of companies' fundamentals and valuation. 
# Data source: [Financialmodelingprep API](https://financialmodelingprep.com/developer/docs/)
# Disclaimer: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.

# 01 - Source libraries and utility functions -----------------------------------------------
source('scripts/utils.R')
source('scripts/data_retrieval.R')

# 02 - Get stock data from fmp --------------------------------------------------

symbols_df <- get_stock_data_df(API_Key = API_Key)


## 02.1 Get Fundamentals data from fmp -----------------------------------------

# Select specific stocks (to prevent out of memory)
symbols_df <- symbols_df %>% 
  filter(!is.na(Dow_Jones))

# Retrieve fundamentals
fundamentals_df <- get_fundamentals_data_df(symbols_df, period = "quarter", 
                                            limit = 12, API_Key = API_Key)

financial_statements_as_reported_list <- get_financial_statements_as_reported_list(symbols_df, period = "quarter", limit = 12, API_Key = API_Key)




# Retrieve historical price data
price_history_data_df <- get_price_history_data_df(symbols_df, startDate = historical_dates$date_20Y, endDate = today(), API_Key = API_Key)
