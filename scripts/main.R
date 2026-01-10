# Author: gp1981
# Date: 16 July 2024
# Purpose: Perform analysis of companies' fundamentals and valuation. 
# Data source: [Financialmodelingprep API](https://financialmodelingprep.com/developer/docs/)
# Disclaimer: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.

# 01 - Source libraries and utility functions ----------------------------------
source('scripts/utils.R')
source('scripts/data_retrieval.R')
source('scripts/analysis.R')

# 02 - Get stock data  ------------------------------------------
Stock_List_data <- get_stock_data_df(API_Key = API_Key)
FX_rates_USD_df <- API_FX_rate(API_Key)

## 02.1 - Get stock data of Magic Formula ----------------------------------
MF_df <- get_MF_data_df(mktCap_limit_lower = 1000, mktCap_limit_upper = 20000, mktCap_step_M = 100)

# 04 - Select manually stocks -------------------------------------------
Stock_List_data <- Stock_List_data %>% filter(Ticker %in% c("CALM"))
Stock_List_data <- API_Profile(Stock_List_data, API_Key)

# 04.1 - OPTION Read the tickers from Excel (sheet "Full_Equity", column H4:H28) ----
library(readxl)
tickers <- read_excel("your_file.xlsx", sheet = "Full_Equity", range = "H4:H28", col_names = FALSE)

# 04.2. Convert to vector (and drop any NA values)
ticker_vector <- tickers[[1]] %>% na.omit() %>% unique()

# 04.3. Filter your data frame
symbols_df <- symbols_df %>% filter(symbol %in% ticker_vector)



## 05 - Select companies from www.magicformulainvesting.com (MF) ----------------
Stock_List_data <- MF_df %>% 
  left_join(Stock_List_data, by = "Ticker")

# 06 - Get fundamentals of selected stocks -------------------------
period_limit = 60
period = "quarter"
fundamentals_df_original <- get_fundamentals_data_df(Stock_List_data, period, period_limit, 
                                                     API_Key = API_Key)

fundamentals_df <- reduce_financialsMetricsProfile(fundamentals_df_original, FX_rates_USD_df)

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
                                                     "commonDividendsPaid",
                                                     "preferredDividendsPaid",
                                                     "commonStockIssuance",
                                                     "commonStockRepurchased",
                                                     "depreciationAndAmortization",
                                                     "capitalExpenditure"))


fundamentals_df_TTM <- fundamentals_df %>%
  select(date,Ticker, ends_with("_TTM"))
# # %>%
# #   filter(month(date) == 9)  # to change based on TTM
# 
# df<- print_fundamentals_TTM(df = fundamentals_df_TTM,
#                  Ticker= "NOMD")

# 06 - Get price and quote data of selected stocks -------------------------
quote_data_df <- get_quote_data_df(Stock_List_data, API_Key = API_Key)
# price_history_data_df <- get_price_history_data_df(symbols_df, startDate = historical_dates$date_20Y, endDate = today(), API_Key = API_Key)

 
# 07 - Maintenance CAPEX, Owner Earnings, Full Equity Growth-----------------------------------------------------
fundamentals_df <- excess_cash(fundamentals_df)
fundamentals_df <- maintenance_CAPEX(fundamentals_df)
fundamentals_df <- ownerEarnings(fundamentals_df)
fundamentals_df <- full_equity_CAGR(fundamentals_df)
fundamentals_df <- negative_FCF(fundamentals_df)
fundamentals_df <- multipliers(fundamentals_df)

export_excel_data(fundamentals_df)
export_excel_data(fundamentals_df_TTM)

# 08 - Combine fundamentals and quotes ------------------------------------
data_df <- left_join(fundamentals_df, quote_data_df)

# 09 - Magic Formula Ranking ---------------------------------------------
df_MF_rank <- calculate_MF_ranking(data_df)
export_excel_data(df_MF_rank, "MF_Rank")
# 09 - Ratio analysis --------------------------------------------------------

# Select companies for ratio analysis (max 5 companies e.g. peers)
df_ratio <- fundamentals_df

ratio_analysis_plot <- ratio_analysis_chart(df_ratio) #-> CHECK RATIO

# Charts
ratio_analysis_plot$current_assets_plot

ratio_analysis_plot$cash_conversion_plot

ratio_analysis_plot$debt_ratios_plot

ratio_analysis_plot$debt_coverage_plot



# 09 - Capex Equity growth ----------------------------------------------------------
capex_equity_growth_plot(fundamentals_df)

seasonality(fundamentals_df)

