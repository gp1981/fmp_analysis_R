# Author: gp1981
# Date: 16 July 2024
# Purpose: Perform analysis of companies' fundamentals and valuation. 
# Data source: [Financialmodelingprep API](https://financialmodelingprep.com/developer/docs/)
# Disclaimer: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.

# 01 - Source libraries and utility functions ----------------------------------
source('scripts/utils.R')
source('scripts/data_retrieval.R')

# 02 - Get stock data from fmp and MF ------------------------------------------
symbols_df <- get_stock_data_df(API_Key = API_Key)

# Retrieve data from www.magicformulainvesting.com (MF) ----------------
MF_df <- get_MF_data_df(mktCap_limit_lower = 1000, mktCap_limit_upper = 10000, mktCap_step_M = 100)

# Select only MF companies
symbols_df_MF <- MF_df %>% 
  left_join(symbols_df, by = "symbol") %>% 
  select(-name.x) %>% 
  rename(name = name.y) %>% 
  select(name, everything())

## 02.1 - Get fundamentals from fmp -------------------------
fundamentals_df <- get_fundamentals_data_df(symbols_df_MF, period = "quarter", 
                                            limit = 12, API_Key = API_Key)

## 02.2 - Retrieve historical price and quote from fmp ---------------------------
price_history_data_df <- get_price_history_data_df(symbols_df, startDate = historical_dates$date_20Y, endDate = today(), API_Key = API_Key)

quote_data_df <- get_quote_data_df(symbols_df, API_Key = API_Key)

## 02.3 - Retrieve filing as reported from fmp (WIP) ------------------
financial_statements_as_reported_list <- get_financial_statements_as_reported_list(symbols_df, period = "quarter", limit = 12, API_Key = API_Key)

# Identify specific variables of filings as reported ---
variables_of_interest_df <- search_and_retrieve_columns(financial_statements_as_reported_list, words = c("treasury", "dividend"))


# Extract specific variables from financial_statements_as_reported (EXAMPLE)
equity_variables_to_extract <- c("date", "symbol", "treasurystockvalue", "treasurystockcommonshares",
                          "paymentsforrepurchaseofcommonstock", "proceedsfromissuanceorsaleofequity",
                          "dividends", "paymentsofdividendscommonstock")

# Extract the specific variables and combine into a single data frame
combined_equity_variable_df <- extract_specific_variables(financial_statements_as_reported_list, equity_variables_to_extract)



export_excel_data(MF_df)