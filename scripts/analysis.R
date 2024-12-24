# Author: gp1981
# Date: 16 July 2024
# Purpose: Perform analysis
# Disclaimer: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.


# Excess of Cash ----------------------------------------------------------

excess_cash <- function(df) {
  
  # Define year
  df <- df %>% 
    mutate(year = year(date))
  
  
  # Calculate median cash over revenue for each year and industry
  median.cash.industry <- df %>%
    group_by(year, industry) %>%
    filter(
      !is.na(cashAndShortTermInvestments),
      !is.na(revenue),
      !revenue <= 0
    ) %>%
    dplyr::summarise(
      median.cash_over_revenue = median(cashAndShortTermInvestments / revenue),
      count.stocks.industry = n()
    ) %>% 
    ungroup()
  
  # Join the median cash data with the original data frame
  df <- left_join(df, median.cash.industry, by = c('year', 'industry'))
  
  # Calculate the excess of cash based on different conditions
  df <- df %>%
    mutate(
      excess.cash = case_when(
        cashAndShortTermInvestments > cashAndCashEquivalents ~ cashAndShortTermInvestments - cashAndCashEquivalents - 
          (dividendsPaid + commonStockRepurchased),
        cashAndShortTermInvestments <= cashAndCashEquivalents & revenue > 0 ~ as.numeric(revenue) * 0.05,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      Cash_ST.Industry.Benchmark = median.cash_over_revenue * revenue,
      Excess.Cash.Industry.Benchmark = cashAndShortTermInvestments - Cash_ST.Industry.Benchmark) %>%
    
    mutate(
      Excess.Cash.2 = case_when(
        cashAndShortTermInvestments > cashAndCashEquivalents & revenue * 0.05 ~ coalesce(excess.cash,0),
        Excess.Cash.Industry.Benchmark < cashAndShortTermInvestments & Excess.Cash.Industry.Benchmark > 0 & revenue * 0.05 ~ 
          coalesce(Excess.Cash.Industry.Benchmark, 0),
        TRUE ~ coalesce(pmax(0.05 * revenue, 0), 0)
      )
    )
  
  # Remove intermediate columns
  df <- df %>%
    select(-Cash_ST.Industry.Benchmark, -Excess.Cash.Industry.Benchmark, -median.cash_over_revenue, -excess.cash) %>% 
    rename(excess_cash = Excess.Cash.2)
  
  return(df)
}
# MF ranking   --------------------------------------------------------------
calculate_MF_ranking <- function(df){
  ## 01 - Calculation of 4FQ rolling sums, Earnings Yield and Return on Capital
  df <- df %>% 
    group_by(symbol) %>%
    arrange(desc(date)) %>% 
    mutate(
      Revenue.4FQ = rollapply(revenue,
                              width = 4, FUN = sum, align = "left", fill = NA),
      EBIT.4FQ = rollapply(operatingIncome,
                           width = 4, FUN = sum, align = "left", fill = NA),
      FCF.4FQ = rollapply(as.numeric(freeCashFlow),
                          width = 4, FUN = sum, align = "left", fill = NA),
      Op_CashFlow.4FQ = rollapply(netCashProvidedByOperatingActivities,
                                  width = 4, FUN = sum, align = "left", fill = NA),
      Fin.CashFlow.4FQ = rollapply(netCashUsedForInvestingActivites,
                                   width = 4, FUN = sum, align = "left", fill = NA),
      Inv_CashFlow.4Q = rollapply(netCashUsedProvidedByFinancingActivities,
                                  width = 4, FUN = sum, align = "left", fill = NA),
      Capex.4FQ = rollapply(capitalExpenditure,
                            width = 4, FUN = sum, align = "left", fill = NA),
    ) %>% ungroup()
  
  ## 02 - Calculation of excess of cash based on industry statistics
  
  df <- excess_cash(df)
  
  ## 03 - Calculation of FCF to Equity Net Premium
  
  df <- df %>% 
    mutate(Tangible_Equity_book = totalAssets - totalLiabilities - 
             goodwillAndIntangibleAssets,
           
           Equity_Net_Premium = mktCap - Tangible_Equity_book,
           
           FCFtoEquity_Net_premium= Equity_Net_Premium / FCF.4FQ) %>% 
    
    mutate(Net_Working_Capital = (totalCurrentAssets - excess_cash) - (totalCurrentLiabilities - shortTermDebt),
           
           Tangible_Capital_Employed = totalAssets - (otherCurrentAssets + 
                                                        otherNonCurrentAssets +
                                                        goodwillAndIntangibleAssets +
                                                        excess_cash) - 
             (totalCurrentLiabilities - 
                shortTermDebt - 
                deferredRevenue - 
                0.5 * otherCurrentLiabilities)
    ) %>% 
    
    mutate(Return_On_Capital_Employed = EBIT.4FQ / Tangible_Capital_Employed,
           Net_Interest_Bearing_Debt = totalDebt + capitalLeaseObligations) %>% 
    
    mutate(Enterprise_Value = mktCap + Net_Interest_Bearing_Debt + 
             minorityInterest + preferredStock) %>% 
    
    mutate(Earnings_Yield = EBIT.4FQ / Enterprise_Value)
    
  ## 04 - Calculation of MF Earnings Yield and Return on Capital
  df <- EY_ROCE_ranking(df)
  
  df <- df %>% 
    select(date, symbol, companyName, 
           country, price, Market_Cap_Millions,Rank_EY_ROCE, Earnings_Yield, 
           Return_On_Capital_Employed, priceEarningsRatioTTM, priceBookValueRatioTTM, pbRatioTTM,
           totalDebtToCapitalizationTTM, debtRatioTTM, debtEquityRatioTTM,  FCFtoEquity_Net_premium,
           currentRatioTTM, quickRatioTTM, cashRatioTTM, returnOnEquityTTM, returnOnAssetsTTM, returnOnCapitalEmployedTTM, 
           everything()) %>% 
    arrange(Rank_EY_ROCE)
  
  return(df)
}

## 04 - Calculation of MF ranking

EY_ROCE_ranking <- function(df){
  ## 01.2 - Calculation of ranking MF
  
  df <- df %>%
    group_by(symbol) %>%
    arrange(desc(date)) %>%
    slice(1) %>%
    ungroup()
  
  df <- df %>% 
    filter(!is.na(Return_On_Capital_Employed) & is.finite(Return_On_Capital_Employed)) %>% 
    arrange(desc(Return_On_Capital_Employed)) %>% 
    mutate(Rank_Return_On_Capital_Employed = dplyr::row_number()) %>% 
    select(date,symbol, companyName, mktCap, Enterprise_Value, Earnings_Yield, Rank_Return_On_Capital_Employed,
           Return_On_Capital_Employed, EBIT.4FQ, Tangible_Capital_Employed, Net_Working_Capital, 
           excess_cash, everything()) %>% 
    ungroup()
  
  df <- df %>% 
    filter(!is.na(Earnings_Yield) & is.finite(Earnings_Yield) ) %>% 
    arrange(desc(Earnings_Yield)) %>% 
    mutate(Rank_Earnings_Yield = dplyr::row_number()) %>% 
    select(date,symbol, companyName, mktCap, Enterprise_Value, Rank_Earnings_Yield, 
           Earnings_Yield, Rank_Return_On_Capital_Employed, Return_On_Capital_Employed, 
           EBIT.4FQ, Tangible_Capital_Employed, Net_Working_Capital, excess_cash, 
           everything())%>% 
    ungroup()
  
  df <- df %>% 
    mutate(Rank_EY_ROCE_absolute = Rank_Return_On_Capital_Employed + Rank_Earnings_Yield)%>% 
    ungroup()
  
  df <- df %>% 
    arrange(Rank_EY_ROCE_absolute) %>% 
    mutate(Rank_EY_ROCE = dplyr::row_number()) %>%
    select(date,symbol, companyName,threshold_mktCap,TopGreenblatt,industry, 
           Market_Cap_Millions,Rank_EY_ROCE, Earnings_Yield, Return_On_Capital_Employed,
           priceEarningsRatioTTM, priceBookValueRatioTTM,
           totalDebtToCapitalizationTTM, debtRatioTTM, debtEquityRatioTTM,
           FCFtoEquity_Net_premium, Equity_Net_Premium,  
           EBIT.4FQ, Tangible_Capital_Employed,
           Net_Working_Capital, excess_cash, everything()
    )%>% 
    ungroup()
  
  
  return(df)
}


# Ratio Analysis -----------------------------------------------------------

ratio_analysis_chart <- function(financial_data_df){
  
  # Data processing ---------------------------------------------------------
  
  # Function to calculate IQR limits
  calculate_iqr_limits <- function(df, financial_data_df_column) {
    Q1 <- quantile(df[[financial_data_df_column]], 0.25, na.rm = TRUE)
    Q3 <- quantile(df[[financial_data_df_column]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_limit <- Q1 - 1.5 * IQR
    upper_limit <- Q3 + 1.5 * IQR
    return(c(lower_limit, upper_limit))
  }
  
  # List of ratio columns to process
  financial_data_df_columns <- c("currentRatio", "quickRatio", "cashRatio", 
                                 "daysOfSalesOutstanding", "daysOfInventoryOutstanding", 
                                 "daysOfPayablesOutstanding", "operatingCycle", 
                                 "cashConversionCycle", "debtEquityRatio", 
                                 "totalDebtToCapitalization", "longTermDebtToCapitalization", 
                                 "shortTermCoverageRatios", "cashFlowToDebtRatio")
  
  # Reshape the data to long format
  current_assets_ratio_data_long <- financial_data_df %>%
    select(symbol, date, currentRatio, quickRatio, cashRatio) %>%
    pivot_longer(cols = c(currentRatio, quickRatio, cashRatio),
                 names_to = "ratio_type", 
                 values_to = "value")
  
  cash_conversion_ratio_data_long <- financial_data_df %>%
    select(symbol, date, daysOfSalesOutstanding, daysOfInventoryOutstanding, 
           daysOfPayablesOutstanding, operatingCycle, cashConversionCycle) %>%
    pivot_longer(cols = c(daysOfSalesOutstanding, daysOfInventoryOutstanding, 
                          daysOfPayablesOutstanding, operatingCycle, cashConversionCycle),
                 names_to = "ratio_type", 
                 values_to = "value")
  
  debt_ratio_data_long <- financial_data_df %>%
    select(symbol, date, debtEquityRatio, totalDebtToCapitalization, longTermDebtToCapitalization) %>%
    pivot_longer(cols = c(debtEquityRatio, totalDebtToCapitalization, longTermDebtToCapitalization),
                 names_to = "ratio_type", 
                 values_to = "value")
  
  debt_coverage_data_long <- financial_data_df %>%
    select(symbol, date, shortTermCoverageRatios, cashFlowToDebtRatio) %>%
    pivot_longer(cols = c(shortTermCoverageRatios, cashFlowToDebtRatio),
                 names_to = "ratio_type", 
                 values_to = "value")
  
  
  # Calculate IQR limits for each combination of symbol and ratio_type
  current_assets_iqr_limits <- current_assets_ratio_data_long %>%
    group_by(symbol, ratio_type) %>%
    summarize(ymin = calculate_iqr_limits(cur_data(), "value")[1],
              ymax = calculate_iqr_limits(cur_data(), "value")[2],
              .groups = 'drop')
  
  cash_conversion_iqr_limits <- cash_conversion_ratio_data_long %>%
    group_by(symbol, ratio_type) %>%
    summarize(ymin = calculate_iqr_limits(cur_data(), "value")[1],
              ymax = calculate_iqr_limits(cur_data(), "value")[2],
              .groups = 'drop')
  
  debt_ratio_iqr_limits <- debt_ratio_data_long %>%
    group_by(symbol, ratio_type) %>%
    summarize(ymin = calculate_iqr_limits(cur_data(), "value")[1],
              ymax = calculate_iqr_limits(cur_data(), "value")[2],
              .groups = 'drop')
  
  debt_coverage_iqr_limits <- debt_coverage_data_long %>%
    group_by(symbol, ratio_type) %>%
    summarize(ymin = calculate_iqr_limits(cur_data(), "value")[1],
              ymax = calculate_iqr_limits(cur_data(), "value")[2],
              .groups = 'drop')
  
  # Convert date to Date format if it's not already
  current_assets_ratio_data_long$date <- as.Date(current_assets_ratio_data_long$date)
  cash_conversion_ratio_data_long$date <- as.Date(cash_conversion_ratio_data_long$date)
  debt_ratio_data_long$date <- as.Date(debt_ratio_data_long$date)
  debt_coverage_data_long$date <- as.Date(debt_coverage_data_long$date)
  
  # Plotting ----------------------------------------------------------------
  
  
  
  ## 01 - Current asset ratios ----------------------------------------------------
  
  # Plotting current asset ratios
  current_assets_plot <- ggplot(current_assets_ratio_data_long, aes(x = date, y = value, color = ratio_type)) +
    geom_line(size = 1) +
    facet_wrap(~ symbol, scales = "free_y", ncol = 2) +
    labs(x = NULL, y = "Ratio Value",
         title = "Trends of Financial Ratios Over Time",
         subtitle = "Current, Quick, and Cash Ratios by Symbol",
         color = "Ratio Type") +
    scale_color_manual(values = c("currentRatio" = "#0072B2", 
                                  "quickRatio" = "#009E73", 
                                  "cashRatio" = "#D55E00"),
                       labels = c("currentRatio" = "Current Ratio", 
                                  "quickRatio" = "Quick Ratio", 
                                  "cashRatio" = "Cash Ratio")) +
    scale_y_continuous(limits = c(min(current_assets_iqr_limits$ymin, na.rm = TRUE) * 0.8, 
                                  max(current_assets_iqr_limits$ymax, na.rm = TRUE) * 0.8)) +
    theme_minimal() +
    theme(panel.spacing = unit(1, "lines"),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 11),
          plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          legend.position = "bottom",
          legend.title = element_text(face = "bold", size = 11),
          legend.text = element_text(size = 10),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 11, face = "bold"))
  
  ## 02 - Cash conversion ratios ----------------------------------------------------
  
  # Plotting cash conversion ratios
  cash_conversion_plot <- ggplot(cash_conversion_ratio_data_long, aes(x = date, y = value, color = ratio_type)) +
    geom_line(size = 1) +
    facet_wrap(~ symbol, scales = "free_y", ncol = 2) +
    labs(x = NULL, y = "Ratio Value",
         title = "Trends of Financial Ratios Over Time",
         subtitle = "Cash conversion ratio by Symbol",
         color = "Ratio Type") +
    scale_color_manual(values = c("daysOfSalesOutstanding" = "#0072B2",
                                  "daysOfInventoryOutstanding" = "#009E73",
                                  "daysOfPayablesOutstanding" = "#D55E00",
                                  "operatingCycle" = "#CC79A7",
                                  "cashConversionCycle" = "#E69F00"),
                       labels = c("daysOfSalesOutstanding" = "Days of Sales Outstanding (DSO)", 
                                  "daysOfInventoryOutstanding" = "Days of Inventory Outstanding (DIO)", 
                                  "daysOfPayablesOutstanding" = "Days of Payable Outstanding (DPO)",
                                  "operatingCycle" = "Operating Cycle (DSO + DIO)",
                                  "cashConversionCycle" = "Cash Conversion Cycle (DSO + DIO + DPO)")) +
    scale_y_continuous(limits = c(min(cash_conversion_iqr_limits$ymin, na.rm = TRUE) * 0.8, 
                                  max(cash_conversion_iqr_limits$ymax, na.rm = TRUE) * 0.8)) +
    theme_minimal() +
    theme(panel.spacing = unit(1, "lines"),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 11),
          plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          legend.position = "bottom",
          legend.title = element_text(face = "bold", size = 11),
          legend.text = element_text(size = 10),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 11, face = "bold"))
  
  
  ## 03 - Debt ratios ----------------------------------------------------
  
  # Plotting debt ratios
  debt_ratios_plot <- ggplot(debt_ratio_data_long, aes(x = date, y = value, color = ratio_type)) +
    geom_line(size = 1) +
    facet_wrap(~ symbol, scales = "free_y", ncol = 2) +
    labs(x = NULL, y = "Ratio Value",
         title = "Trends of Financial Ratios Over Time",
         subtitle = "Debt ratios by Symbol",
         color = "Ratio Type") +
    scale_color_manual(values = c("debtEquityRatio" = "#0072B2",
                                  "totalDebtToCapitalization" = "#009E73",
                                  "longTermDebtToCapitalization" = "#D55E00"),
                       labels = c("debtEquityRatio" = "Debt to Equity (Total Liabilities / Total Equity)",
                                  "totalDebtToCapitalization" = "Total Debt to Capitalization (Total Debt / (Total Debt + Total Equity))", 
                                  "longTermDebtToCapitalization" = "Long Term Capitalization (Long Term Debt / (Long Term Debt + Total Equity))")) + 
    scale_y_continuous(limits = c(min(debt_ratio_iqr_limits$ymin, na.rm = TRUE) * 0.8, 
                                  max(debt_ratio_iqr_limits$ymax, na.rm = TRUE) * 0.8)) +
    theme_minimal() +
    theme(panel.spacing = unit(1, "lines"),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 11),
          plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          legend.position = "bottom",
          legend.title = element_text(face = "bold", size = 11),
          legend.text = element_text(size = 10),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 11, face = "bold")
    )
  
  # Plotting debt coverage
  debt_coverage_plot <- ggplot(debt_coverage_data_long, aes(x = date, y = value, color = ratio_type)) +
    geom_line(size = 1) +
    facet_wrap(~ symbol, scales = "free_y", ncol = 2) +
    labs(x = NULL, y = "Ratio Value",
         title = "Trends of Debt Coverage Over Time",
         subtitle = "Debt coverage by Symbol",
         color = "Ratio Type") +
    scale_color_manual(values = c("shortTermCoverageRatios" = "#0072B2", 
                                  "cashFlowToDebtRatio" = "#E69F00"),
                       labels = c("shortTermCoverageRatios" = "Short Term Coverage Ratio (Operating Cash Flow / Short Term Debt)",
                                  "cashFlowToDebtRatio" =  "Cash Flow to Debt Ratio (Operating Cash Flow / Total Debt)")) + 
    scale_y_continuous(limits = c(min(debt_coverage_iqr_limits$ymin, na.rm = TRUE) * 0.3, 
                                  max(debt_coverage_iqr_limits$ymax, na.rm = TRUE) * 0.3)) +
    theme_minimal() +
    theme(panel.spacing = unit(1, "lines"),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 11),
          plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          legend.position = "bottom",
          legend.title = element_text(face = "bold", size = 11),
          legend.text = element_text(size = 10),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 11, face = "bold")
    )
  
  plot_ratio_analysis <- list(
    current_assets_plot = current_assets_plot, 
    cash_conversion_plot = cash_conversion_plot, 
    debt_ratios_plot = debt_ratios_plot, 
    debt_coverage_plot = debt_coverage_plot)
  
  return(plot_ratio_analysis)
}




# Capex vs Equity growth --------------------------------------------------

capex_equity_growth_plot <- function(fundamentals_df) {
  
  # Step 1: Calculate Capex ()
  fundamentals_df <- fundamentals_df %>%
    group_by(symbol) %>% 
    arrange(date) %>%
    mutate(capex = capexToRevenue * revenue,
           capex_TTM = rollapply(capex, width = 4, FUN = sum, fill = NA, align = "right")) %>% 
    ungroup()
  
  # Step 2: Calculate Equity (no TTM, just sum of total equity and dividends)
  fundamentals_df <- fundamentals_df %>%
    group_by(symbol) %>% 
    arrange(date) %>% 
    mutate(
      # Get the first (oldest) value of totalStockholdersEquity
      initial_equity = first(totalStockholdersEquity),
      
      # Calculate cumulative sums for required fields up to each row
      cum_dividends = cumsum(dividendsPaid),
      cum_issued = cumsum(commonStockIssued),
      cum_repurchased = cumsum(commonStockRepurchased),
      
      
      # Calculate full equity using the first value of totalStockholdersEquity
      full_equity = initial_equity + (-1) * cum_dividends - cum_issued + (-1) * cum_repurchased
    ) %>% 
    ungroup()
  
  # Step 3: Calculate Equity Increases  2, 3, 4, 6 years prior
  fundamentals_df <- fundamentals_df %>%
    group_by(symbol) %>% 
    arrange(desc(date)) %>% 
    mutate(
      equity_increase_annual = full_equity - lead(full_equity, 4),
      
      capex_TTM_2y = lead(capex_TTM,8),
      capex_TTM_3y = lead(capex_TTM,12),
      capex_TTM_4y = lead(capex_TTM,16),
      capex_TTM_8y = lead(capex_TTM,32)
      
    ) %>% 
    ungroup()
  
  
  # Step 4: Calculate Ratios using Capex 
  fundamentals_df <- fundamentals_df %>%
    group_by(symbol) %>% 
    mutate(
      full_Equity_increase_capex_2y = equity_increase_annual / capex_TTM_2y,
      full_Equity_increase_capex_3y = equity_increase_annual / capex_TTM_3y,
      full_Equity_increase_capex_4y = equity_increase_annual / capex_TTM_4y,
      full_Equity_increase_capex_8y = equity_increase_annual / capex_TTM_8y
    ) %>% 
    ungroup()
  
  # Function to replace outliers in a column with NA
  replace_outliers_with_na <- function(x) {
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR_value <- Q3 - Q1
    
    lower_bound <- Q1 - 1.5 * IQR_value
    upper_bound <- Q3 + 1.5 * IQR_value
    
    x <- ifelse(x < lower_bound | x > upper_bound, NA, x)
    return(x)
  }
  
  # Step 4.5: Replace outliers with NA for ratios
  fundamentals_df <- fundamentals_df %>%
    group_by(symbol) %>%  # Apply outlier removal within each symbol group
    mutate(
      full_Equity_increase_capex_2y = replace_outliers_with_na(full_Equity_increase_capex_2y),
      full_Equity_increase_capex_3y = replace_outliers_with_na(full_Equity_increase_capex_3y),
      full_Equity_increase_capex_4y = replace_outliers_with_na(full_Equity_increase_capex_4y),
      full_Equity_increase_capex_8y = replace_outliers_with_na(full_Equity_increase_capex_8y)
    ) %>%
    ungroup()
  
  # Step 5: Calculate Correlations for Each Symbol
  correlation_results <- fundamentals_df %>%
    group_by(symbol) %>%
    summarise(
      full_Equity_increase_capex_corr_2y = ifelse(sum(!is.na(equity_increase_annual) & !is.na(capex_TTM_2y)) > 1, 
                       cor(equity_increase_annual, capex_TTM_2y, use = "complete.obs"), NA),
      full_Equity_increase_capex_corr_3y = ifelse(sum(!is.na(equity_increase_annual) & !is.na(capex_TTM_3y)) > 1, 
                       cor(equity_increase_annual, capex_TTM_3y, use = "complete.obs"), NA),
      full_Equity_increase_capex_corr_4y = ifelse(sum(!is.na(equity_increase_annual) & !is.na(capex_TTM_4y)) > 1, 
                       cor(equity_increase_annual, capex_TTM_4y, use = "complete.obs"), NA),
      full_Equity_increase_capex_corr_8y = ifelse(sum(!is.na(equity_increase_annual) & !is.na(capex_TTM_8y)) > 1, 
                       cor(equity_increase_annual, capex_TTM_8y, use = "complete.obs"), NA)
    ) %>% 
    ungroup()

  print(correlation_results)
  
  
  # Step 6: Prepare data for plotting
  plot_data <- fundamentals_df %>%
    select(date, symbol, full_Equity_increase_capex_2y, full_Equity_increase_capex_3y,
           full_Equity_increase_capex_4y, full_Equity_increase_capex_8y) %>%
    pivot_longer(cols = starts_with("full_Equity_increase_capex_"), names_to = "lag", values_to = "ratio") %>%
    mutate(lag_year = as.numeric(str_extract(lag, "\\d+")))  # Extract numeric digits
  
  
  # Step 7: Plotting the ratios as percentages
  p <- ggplot(plot_data, aes(x = date, y = ratio, color = as.factor(lag))) +
    geom_point(size = 2) +
    geom_line() +
    labs(title = "Impact of Capex on Total Equity Growth",
         subtitle = "Ratios of Equity Increase to Capex",
         y = "Equity Increase / Capex ",
         x = "Date",
         color = "Lag (Years)") +
    theme_minimal(base_size = 14) + 
    facet_wrap(~ symbol, scales = "fixed", ncol = 1)
  
  # Step 8: Prepare correlation labels for annotations
  correlation_labels <- correlation_results %>%
    pivot_longer(
      cols = starts_with("full_Equity_increase_capex_corr"), 
      names_to = "lag", 
      values_to = "correlation"
    ) %>%
    mutate(
      lag = str_extract(lag, "\\d+")  # Extract only the year (e.g., 2, 3, 4, 8)
    )
  
  
  # Step 9: Add correlation results as annotations on the plot
  p <- p + geom_text(
    data = correlation_labels,
    aes(),  # Empty aes since we manually provide x, y
    x = max(plot_data$date),
    y = max(plot_data$ratio, na.rm = TRUE) * 0.9,
    label = paste0("Corr (", correlation_labels$lag, "y): ", round(correlation_labels$correlation, 2)),
    color = "black",
    size = 4,
    hjust = 0, vjust = 1.2,
    inherit.aes = FALSE
  )
  
  
  # Return the plot
  return(p)
}



# Calculate TTM -----------------------------------------------------------

library(zoo)

ttm_fundamentals <- function(df, fundamentals) {
  # Check if inputs are valid
  if (!all(fundamentals %in% colnames(df))) {
    stop("One or more fundamentals are not present in the dataframe.")
  }
  
  # Apply TTM calculation for each selected fundamental
  df <- df %>%
    group_by(symbol) %>%            # Group by symbol (assuming 'symbol' column exists)
    arrange(desc(date), .by_group = TRUE) %>% # Sort by date within each group
    mutate(across(all_of(fundamentals), 
                  ~rollapply(.x, width = 4, FUN = sum, align = "left", fill = NA),
                  .names = "{.col}_TTM"))      # Apply rollapply and create new columns
  
  return(df)
}