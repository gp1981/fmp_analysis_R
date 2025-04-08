# Author: gp1981
# Date: 16 July 2024
# Purpose: Perform analysis of companies' fundamentals and valuation. 
# Data source: [Financialmodelingprep API](https://financialmodelingprep.com/developer/docs/)
# Disclaimer: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.

# 01 - Source libraries and utility functions ----------------------------------
source('scripts/utils.R')
source('scripts/data_retrieval.R')
source('scripts/analysis.R')

# 02 - Get stock data S&P500, NASDAQ, DOW ------------------------------------------
symbols_df <- get_stock_data_df(API_Key = API_Key)

## 02.1 - Get stock data of Magic Formula ----------------------------------
MF_df <- get_MF_data_df(mktCap_limit_lower = 1000, mktCap_limit_upper = 20000, mktCap_step_M = 100)

# 03 - Get historical data of S&P500, NASDAQ, DOW (WIP) ------------------------------------------
hist_SP500_df <- get_hist_index_df(index = "SP500", API_Key)
hist_NASDAQ_df <- get_hist_index_df(index = "NASDAQ", API_Key)
hist_DOW_df <- get_hist_index_df(index = "DOW", API_Key)

# 04 - Select manually stocks -------------------------------------------
symbols_df <- symbols_df %>% filter(symbol %in% c("CRUS"))

## 04.1 - Select companies from www.magicformulainvesting.com (MF) ----------------
symbols_df <- MF_df %>% 
  left_join(symbols_df, by = "symbol") %>% 
  select(-name.x) %>% 
  rename(name = name.y) %>% 
  select(name, everything())

# 06 - Get fundamentals of selected stocks -------------------------
limit = 60
period = "quarter"
fundamentals_df_original <- get_fundamentals_data_df(symbols_df, period, 
                                            limit, API_Key = API_Key)

fundamentals_df <- reduce_financialsMetricsProfile(fundamentals_df_original)

fundamentals_df <- ttm_fundamentals(fundamentals_df, 
                                    fundamentals = c("revenue",
                                                     "costOfRevenue",
                                                     "sellingGeneralAndAdministrativeExpenses",
                                                     "otherExpenses",
                                                     "researchAndDevelopmentExpenses",
                                                     "operatingIncome",
                                                     "interestExpense",
                                                     "incomeTaxExpense",
                                                     "netIncome",
                                                     "operatingCashFlow",
                                                     "dividendsPaid",
                                                     "commonStockIssued",
                                                     "commonStockRepurchased",
                                                     "depreciationAndAmortization",
                                                     "capitalExpenditure"))


# fundamentals_df_TTM <- fundamentals_df %>%
#   select(date,symbol, ends_with("_TTM")) 
# # %>%
# #   filter(month(date) == 9)  # to change based on TTM
# 
# df<- print_fundamentals_TTM(df = fundamentals_df_TTM,
#                  Ticker= "NOMD")

# 06 - Get price and quote data of selected stocks -------------------------
quote_data_df <- get_quote_data_df(symbols_df, API_Key = API_Key)
# price_history_data_df <- get_price_history_data_df(symbols_df, startDate = historical_dates$date_20Y, endDate = today(), API_Key = API_Key)


# 0X - Get filing as reported from fmp (WIP) ------------------
# financial_statements_as_reported_list <- get_financial_statements_as_reported_list(symbols_df, period = "quarter", limit = 12, API_Key = API_Key)
# 
# # Identify specific variables of filings as reported ---
# variables_of_interest_df <- search_and_retrieve_columns(financial_statements_as_reported_list, words = c("treasury", "dividend"))
# 
# 
# # Extract specific variables from financial_statements_as_reported (EXAMPLE)
# equity_variables_to_extract <- c("date", "symbol", "treasurystockvalue", "treasurystockcommonshares",
#                           "paymentsforrepurchaseofcommonstock", "proceedsfromissuanceorsaleofequity",
#                           "dividends", "paymentsofdividendscommonstock")
# 
# # Extract the specific variables and combine into a single data frame
# combined_equity_variable_df <- extract_specific_variables(financial_statements_as_reported_list, equity_variables_to_extract)
# 
# 
# 
# 07 - Maintenance CAPEX, Owner Earnings, Full Equity Growth-----------------------------------------------------
fundamentals_df <- excess_cash(fundamentals_df)
fundamentals_df <- maintenance_CAPEX(fundamentals_df)
fundamentals_df <- ownerEarnings(fundamentals_df)
fundamentals_df <- full_equity_CAGR(fundamentals_df)
fundamentals_df <- negative_FCF(fundamentals_df)
fundamentals_df <- multipliers(fundamentals_df)

export_excel_data(fundamentals_df)

# 08 - Combine fundamentals and quotes ------------------------------------
data_df <- left_join(fundamentals_df, quote_data_df)

# 09 - Magic Formula Ranking ---------------------------------------------
df_MF_rank <- calculate_MF_ranking(data_df)
export_excel_data(df_MF_rank, "MF_Rank")
# 09 - Ratio analysis --------------------------------------------------------

# Select companies for ratio analysis (max 5 companies e.g. peers)
df_ratio <- fundamentals_df

ratio_analysis_plot <- ratio_analysis_chart(df_ratio)

# Charts
ratio_analysis_plot$current_assets_plot

ratio_analysis_plot$cash_conversion_plot

ratio_analysis_plot$debt_ratios_plot

ratio_analysis_plot$debt_coverage_plot



# 09 - Capex Equity growth ----------------------------------------------------------
capex_equity_growth_plot(fundamentals_df)
seasonality(fundamentals_df)

