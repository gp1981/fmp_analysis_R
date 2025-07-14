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
          (commonDividendsPaid + preferredDividendsPaid + commonStockRepurchased),
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
    group_by(Ticker) %>%
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
      Fin.CashFlow.4FQ = rollapply(netCashProvidedByOperatingActivities,
                                   width = 4, FUN = sum, align = "left", fill = NA),
      Inv_CashFlow.4Q = rollapply(netCashProvidedByFinancingActivities,
                                  width = 4, FUN = sum, align = "left", fill = NA),
      Capex.4FQ = rollapply(capitalExpenditure,
                            width = 4, FUN = sum, align = "left", fill = NA),
    ) %>% ungroup()
  
  ## 02 - Calculation of FCF to Equity Net Premium
  
  df <- df %>% 
    mutate(Tangible_Equity_book = totalAssets - totalLiabilities - 
             goodwillAndIntangibleAssets,
           
           Net_Working_Capital = (totalCurrentAssets - excess_cash) - 
             (totalCurrentLiabilities - shortTermDebt),
           
           Tangible_Capital_Employed = totalAssets - (otherCurrentAssets + 
                                                        otherNonCurrentAssets +
                                                        goodwillAndIntangibleAssets +
                                                        excess_cash) - 
             (totalCurrentLiabilities - 
                shortTermDebt - 
                deferredRevenue - 
                0.5 * otherCurrentLiabilities),
           
           Return_On_Capital_Employed = EBIT.4FQ / Tangible_Capital_Employed,
           
           Net_Interest_Bearing_Debt = totalDebt + capitalLeaseObligations,
           
           Enterprise_Value = marketCap + Net_Interest_Bearing_Debt + 
             minorityInterest + preferredStock,
           
           Earnings_Yield = EBIT.4FQ / Enterprise_Value)
  
  ## 04 - Calculation of MF Earnings Yield and Return on Capital
  df <- EY_ROCE_ranking(df)
  
  ## 05 - Create risk code
  df <- df %>% 
    mutate(Risk.Code = case_when(
      quickRatioTTM >= 2 & 
        cashRatioTTM >= 1 & 
        debtToEquityRatioTTM <= 0.33 & 
        debtToCapitalRatioTTM <= 0.2 ~ "GREEN",
      
      quickRatioTTM >= 1 & 
        cashRatioTTM >= 0.3 & 
        debtToEquityRatioTTM <= 1 &
        debtToCapitalRatioTTM <= 0.35 ~ "YELLOW",
      
      TRUE ~ "RED"
    ))
    
  ## 06 - Prepare output
  df <- df %>% 
    select(date, Ticker, companyName, Risk.Code,
           country, price, Market_Cap_Millions,Rank_EY_ROCE,
           Earning.Power.per.Share.TTM,
           Owner.Earnings.Buffet.per.Share.TTM,
           Owner.Earnings.Buffet.IGVI.per.Share.TTM,
           Owner.Earnings.IGVI.per.Share.TTM,
           everything()) %>% 
    arrange(Rank_EY_ROCE)
  
  return(df)
}

## 04 - Calculation of MF ranking

EY_ROCE_ranking <- function(df){
  ## 01.2 - Calculation of ranking MF
  
  df <- df %>%
    group_by(Ticker) %>%
    arrange(desc(date)) %>%
    slice(1) %>%
    ungroup()
  
  df <- df %>% 
    filter(!is.na(Return_On_Capital_Employed) & is.finite(Return_On_Capital_Employed)) %>% 
    arrange(desc(Return_On_Capital_Employed)) %>% 
    mutate(Rank_Return_On_Capital_Employed = dplyr::row_number()) %>% 
    select(date,Ticker, companyName, marketCap, Enterprise_Value, Earnings_Yield, Rank_Return_On_Capital_Employed,
           Return_On_Capital_Employed, EBIT.4FQ, Tangible_Capital_Employed, Net_Working_Capital, 
           excess_cash, everything()) %>% 
    ungroup()
  
  df <- df %>% 
    filter(!is.na(Earnings_Yield) & is.finite(Earnings_Yield) ) %>% 
    arrange(desc(Earnings_Yield)) %>% 
    mutate(Rank_Earnings_Yield = dplyr::row_number()) %>% 
    select(date,Ticker, companyName, marketCap, Enterprise_Value, Rank_Earnings_Yield, 
           Earnings_Yield, Rank_Return_On_Capital_Employed, Return_On_Capital_Employed, 
           EBIT.4FQ, Net_Working_Capital, excess_cash, 
           everything()) %>% 
    ungroup()
  
  df <- df %>% 
    mutate(Rank_EY_ROCE_absolute = Rank_Return_On_Capital_Employed + Rank_Earnings_Yield)%>% 
    ungroup()
  
  df <- df %>% 
    arrange(Rank_EY_ROCE_absolute) %>% 
    mutate(Rank_EY_ROCE = dplyr::row_number()) %>%
    select(date,Ticker, companyName,threshold_mktCap,TopGreenblatt,industry, 
           Market_Cap_Millions,Rank_EY_ROCE, Earnings_Yield, Return_On_Capital_Employed,
           priceToEarningsRatioTTM, priceToBookRatioTTM,
           debtToCapitalRatioTTM, debtToEquityRatioTTM,
           Equity_Net_premiumToFCF, Equity_Net_Premium,  
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
  financial_data_df_columns <- c("currentRatio", "quickRatio","cashRatio",
                                 "daysOfSalesOutstanding", "daysOfInventoryOutstanding", 
                                 "daysOfPayablesOutstanding", "operatingCycle", 
                                 "cashConversionCycle", "debtEquityRatio", 
                                 "totalDebtToCapitalization", "longTermDebtToCapitalization", 
                                 "shortTermCoverageRatios", "cashFlowToDebtRatio")
  
  # Reshape the data to long format
  current_assets_ratio_data_long <- financial_data_df %>%
    select(Ticker, date, currentRatio, cashRatio) %>%
    pivot_longer(cols = c(currentRatio, cashRatio),
                 names_to = "ratio_type", 
                 values_to = "value") %>% 
    filter(!is.na(value))
  
  cash_conversion_ratio_data_long <- financial_data_df %>%
    select(Ticker, date, daysOfSalesOutstanding, daysOfInventoryOutstanding, 
           daysOfPayablesOutstanding, operatingCycle, cashConversionCycle) %>%
    pivot_longer(cols = c(daysOfSalesOutstanding, daysOfInventoryOutstanding, 
                          daysOfPayablesOutstanding, operatingCycle, cashConversionCycle),
                 names_to = "ratio_type", 
                 values_to = "value") %>% 
    filter(!is.na(value))
  
  debt_ratio_data_long <- financial_data_df %>%
    select(Ticker, date, debtToEquityRatio, debtToCapitalRatio, longTermDebtToCapitalRatio) %>%
    pivot_longer(cols = c(debtToEquityRatio, debtToCapitalRatio, longTermDebtToCapitalRatio),
                 names_to = "ratio_type", 
                 values_to = "value") %>% 
    filter(!is.na(value))
  
  debt_coverage_data_long <- financial_data_df %>%
    select(Ticker, date, debtServiceCoverageRatio, shortTermOperatingCashFlowCoverageRatio) %>%
    pivot_longer(cols = c(debtServiceCoverageRatio, shortTermOperatingCashFlowCoverageRatio),
                 names_to = "ratio_type", 
                 values_to = "value") %>% 
    filter(!is.na(value))
  
  
  # Calculate IQR limits for each combination of Ticker and ratio_type
  current_assets_iqr_limits <- current_assets_ratio_data_long %>%
    group_by(Ticker, ratio_type) %>%
    summarize(ymin = calculate_iqr_limits(cur_data(), "value")[1],
              ymax = calculate_iqr_limits(cur_data(), "value")[2],
              .groups = 'drop')
  
  cash_conversion_iqr_limits <- cash_conversion_ratio_data_long %>%
    group_by(Ticker, ratio_type) %>%
    summarize(ymin = calculate_iqr_limits(cur_data(), "value")[1],
              ymax = calculate_iqr_limits(cur_data(), "value")[2],
              .groups = 'drop')
  
  debt_ratio_iqr_limits <- debt_ratio_data_long %>%
    group_by(Ticker, ratio_type) %>%
    summarize(ymin = calculate_iqr_limits(cur_data(), "value")[1],
              ymax = calculate_iqr_limits(cur_data(), "value")[2],
              .groups = 'drop')
  
  debt_coverage_iqr_limits <- debt_coverage_data_long %>%
    group_by(Ticker, ratio_type) %>%
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
    facet_wrap(~ Ticker, scales = "free_y", ncol = 2) +
    labs(x = NULL, y = "Ratio Value",
         title = "Trends of Financial Ratios Over Time",
         subtitle = "Current, Quick, and Cash Ratios by Ticker",
         color = "Ratio Type") +
    scale_color_manual(values = c("currentRatio" = "#0072B2",
                                  "cashRatio" = "#D55E00",
                                  "quickRatio" = "#009E73"),
                       labels = c("currentRatio" = "Current Ratio",
                                  "cashRatio" = "Cash Ratio",
                                  "quickRatio" = "Quick Ratio")) +
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
    facet_wrap(~ Ticker, scales = "free_y", ncol = 2) +
    labs(x = NULL, y = "Ratio Value",
         title = "Trends of Financial Ratios Over Time",
         subtitle = "Cash conversion ratio by Ticker",
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
    facet_wrap(~ Ticker, scales = "free_y", ncol = 2) +
    labs(x = NULL, y = "Ratio Value",
         title = "Trends of Financial Ratios Over Time",
         subtitle = "Debt ratios by Ticker",
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
    facet_wrap(~ Ticker, scales = "free_y", ncol = 2) +
    labs(x = NULL, y = "Ratio Value",
         title = "Trends of Debt Coverage Over Time",
         subtitle = "Debt coverage by Ticker",
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




# Calculate fundamentals TTM -----------------------------------------------------------

library(zoo)

ttm_fundamentals <- function(df, fundamentals) {
  # Check if inputs are valid
  if (!all(fundamentals %in% colnames(df))) {
    stop("One or more fundamentals are not present in the dataframe.")
  }
  
  # Apply TTM calculation for each selected fundamental
  df <- df %>%
    group_by(Ticker) %>%            # Group by symbol (assuming 'symbol' column exists)
    arrange(desc(date), .by_group = TRUE) %>% # Sort by date within each group
    mutate(across(all_of(fundamentals), 
                  ~rollapply(.x, width = 4, FUN = sum, align = "left", fill = NA),
                  .names = "{.col}_TTM"))      # Apply rollapply and create new columns
  
  return(df)
}
# Maintenace CAPEX -----------------------------------------------------------
maintenance_CAPEX <- function(df) {
  # Step 1: Initial Calculations - Cumulative Sums
  # - Group data by ticker to apply cumulative sums starting from the oldest quarter.
  df <- df %>%
    group_by(Ticker) %>%
    arrange(desc(date)) %>%
    mutate(
      # Reverse cumulative sum of capital expenditure and depreciation to ensure trailing values are from the oldest date.
      total.capital_expenditure = rev(cumsum(rev(capitalExpenditure))),
      total.depreciation_amortization = rev(cumsum(rev(depreciationAndAmortization)))
    ) %>%
    ungroup()
  
  # Step 2: Rolling Averages and Growth CAPEX Calculations
  # - Group by ticker and apply rolling operations for ratios and growth calculations.
  df <- df %>%
    group_by(Ticker) %>%
    arrange(desc(date)) %>%
    mutate(
      # Calculate the ratio of revenue to capital employed.
      ratio.revenue_FixedAsset = coalesce(revenue / (propertyPlantEquipmentNet + otherNonCurrentAssets),0),
      
      # Calculate the trailing average of the ratio over the limit.
      avg_revenue = rollapply(
        revenue, width = period_limit, FUN = mean, align = "left", partial = TRUE
      ),
      
      avg_ratio.revenue_FixedAsset = rollapply(
        ratio.revenue_FixedAsset, width = period_limit, FUN = mean, align = "left", partial = TRUE
      ),
      
      # Calculate the ratio to avg revenue.
      avg_ratio.revenue = coalesce(revenue / avg_revenue,0),
      
      avg_ratio.revenue = ifelse(avg_ratio.revenue > 1, avg_ratio.revenue, 1),
      
      # Calculate the difference in revenue between the first and last periods within the limit.
      revenue_diff = rollapply(
        revenue, width = period_limit, FUN = function(x) -1 * (last(x) - first(x)), align = "left", partial = TRUE
      ),
      
      # Set any negative revenue_diff to zero (representing no growth in revenue).
      revenue_diff = ifelse(revenue_diff < 0, 0, revenue_diff),
      
      # Compute growth CAPEX as the product of average ratio and revenue difference.
      growth_capex = avg_ratio.revenue_FixedAsset * revenue_diff,
      
      # Calculate total maintenance CAPEX by subtracting growth CAPEX from total capital expenditure.
      total.maintenance_capex = (-1) * total.capital_expenditure - growth_capex,
      
      total.maintenance_capex = ifelse(total.maintenance_capex < 0, (-1) * total.capital_expenditure, total.maintenance_capex)
      
    ) %>%
    ungroup()
  
  # Step 3: Annualizing Maintenance CAPEX
  # - Calculate quarter distance from the oldest date and use it to annualize maintenance CAPEX.
  df <- df %>%
    group_by(Ticker) %>%
    arrange(date) %>%
    mutate(
      # Calculate quarter distance from the earliest date to estimate the length of each maintenance CAPEX period in years.
      quarter_distance = row_number()
    ) %>%
    arrange(desc(date)) %>%
    mutate(
      # Annualized maintenance CAPEX by dividing by quarter distance converted to years. Used revenue increase respect to avg as multiplying factor 
      annualised.maintenance_capex = (coalesce(total.maintenance_capex / (quarter_distance / 4),0) * avg_ratio.revenue)
    ) %>%
    ungroup() %>%
    
    # Step 4: Output Formatting
    # - Select specific columns for the final output.
    select(Ticker, date, annualised.maintenance_capex, capitalExpenditure, industry, sector, everything())
  
  return(df)
}

# Owner Earnings -----------------------------------------------------------
ownerEarnings <- function(df){
  
  # Calculation Earning Power Greenwald
  df <- df %>% 
    group_by(Ticker) %>% 
    arrange(desc(date)) %>% 
    mutate(
      
      # Calculate the cumulative sum of revenue, SG&A, R&D, other expenses, capex, depreciation, income tax, and full equity
      sum_Revenue = rollapply(
        revenue, width = period_limit, FUN = sum, align = "left", partial = TRUE),
      
      sust_Revenue.TTM = rollapply(
        revenue, width = 4, FUN = mean, align = "left", partial = TRUE),
      
      sum_SGA = rollapply(
        sellingGeneralAndAdministrativeExpenses, width = period_limit, FUN = sum, align = "left", partial = TRUE),
      
      sum_RD = rollapply(
        researchAndDevelopmentExpenses, width = period_limit, FUN = sum, align = "left", partial = TRUE),
      
      sum_Other_Expenses = rollapply(
        otherExpenses, width = period_limit, FUN = sum, align = "left", partial = TRUE),
      
      sum_Operating_Income = rollapply(
        operatingIncome, width = period_limit, FUN = sum, align = "left", partial = TRUE),
      
      sum_Capex = rollapply(
        capitalExpenditure, width = period_limit, FUN = sum, align = "left", partial = TRUE),
      
      sum_Income_Tax = rollapply(
        incomeTaxExpense, width = period_limit, FUN = sum, align = "left", partial = TRUE),
      
      sum_Income_Before_Tax = rollapply(
        incomeBeforeTax, width = period_limit, FUN = sum, align = "left", partial = TRUE)
      
    ) %>% 
    ungroup()
  
  df <- df %>% 
    mutate(
      avg.SGA_pct = sum_SGA / sum_Revenue,
      avg.RD_pct = sum_RD / sum_Revenue,
      avg.Other_Expenses_pct = sum_Other_Expenses / sum_Revenue,
      avg.Operating_Income_pct = sum_Operating_Income / sum_Revenue,
      avg.Capex_pct  = sum_Capex / sum_Revenue,
      avg.Income_Tax_pct = sum_Income_Tax / sum_Income_Before_Tax
    )
  
  # Calculate adjustments
  df <- df %>% 
    mutate(
      adj.SGA = ((sellingGeneralAndAdministrativeExpenses  / revenue) - avg.SGA_pct) * sust_Revenue.TTM,
      adj.RD = ((researchAndDevelopmentExpenses  / revenue) - avg.RD_pct) * sust_Revenue.TTM,
      adj.Other_Expenses = ((otherExpenses  / revenue) - avg.Other_Expenses_pct) * sust_Revenue.TTM,
      adj.Restoring_Assets = (depreciationAndAmortization - annualised.maintenance_capex/4),
      Earning.Power = (sust_Revenue.TTM * avg.Operating_Income_pct + adj.SGA + adj.RD + adj.Other_Expenses + adj.Restoring_Assets)*(1- avg.Income_Tax_pct)
    ) %>% 
    ungroup()
  
  
  # Calculation of Owner Earnings
  df  <- df %>% 
    mutate( 
      Owner.Earnings.Buffet = 
        netIncome + (depreciationAndAmortization - annualised.maintenance_capex/4) + 
        stockBasedCompensation + changeInWorkingCapital + deferredIncomeTax,
      
      Owner.Earnings.Buffet.IGVI = 
        netIncome + (depreciationAndAmortization - annualised.maintenance_capex/4) + 
        changeInWorkingCapital + deferredIncomeTax,
      
      Owner.Earnings.IGVI = 
        netCashProvidedByOperatingActivities - stockBasedCompensation - annualised.maintenance_capex/4 * 1.1,
    ) 
  
  # Calculating output: Earning Power, Owner Earnings, TTM and per Share
  df <- df %>% 
    group_by(Ticker) %>% 
    arrange(desc(date)) %>% 
    mutate(
      Earning.Power.TTM = rollapply(Earning.Power,
                                    width = 4, FUN = sum, align = "left", fill = NA)
    ) %>% 
    ungroup()
  
  df <- df %>% 
    group_by(Ticker) %>% 
    arrange(desc(date)) %>% 
    mutate(
      marketCap_LocalFX = ifelse(row_number() == 1, marketCap_USD_Profile/FX_rates, marketCapitalization_EV_LocalFX_EV)
    )
  
  df <- df %>% 
    group_by(Ticker) %>% 
    arrange(desc(date)) %>% 
    mutate(
      Earning.Power.per.Share.TTM = Earning.Power.TTM / outstandingShares,
      
      Owner.Earnings.Buffet.TTM = rollapply(Owner.Earnings.Buffet,
                                            width = 4, FUN = sum, align = "left", fill = NA),
      
      Owner.Earnings.Buffet.per.Share.TTM = Owner.Earnings.Buffet.TTM / outstandingShares,
      
      Owner.Earnings.Buffet.IGVI.TTM = rollapply(Owner.Earnings.Buffet.IGVI,
                                                 width = 4, FUN = sum, align = "left", fill = NA),
      
      Owner.Earnings.Buffet.IGVI.per.Share.TTM = Owner.Earnings.Buffet.IGVI.TTM / outstandingShares,
      
      Owner.Earnings.IGVI.TTM = rollapply(Owner.Earnings.IGVI,
                                          width = 4, FUN = sum, align = "left", fill = NA),
      
      Owner.Earnings.IGVI.per.Share.TTM = Owner.Earnings.IGVI.TTM / outstandingShares,
      
      mktCap.per.Share = marketCap_LocalFX / outstandingShares
      
    ) %>% 
    ungroup()
  
  return(df)
} 
# Full Equity CAGR-----------------------------------------------------------
full_equity_CAGR <- function(df){
  
  # 01 - Calculation full Equity growth ------------------------------------------
  
  
  # Calculate full equity and CAGR
  df_cagr <- df %>%
    group_by(Ticker) %>% 
    arrange(date) %>% # Ensure data is sorted by date
    mutate(
      # Get the last (newest) values
      last_equity = last(
        coalesce(totalStockholdersEquity, lag(totalStockholdersEquity, 1))
      ),
      
      # Cumulative financial items
      cum_dividends = cumsum(coalesce(commonDividendsPaid, 0) + coalesce(preferredDividendsPaid, 0)),
      cum_issued = cumsum(coalesce(commonStockIssuance, 0)),
      cum_repurchased = cumsum(coalesce(commonStockRepurchased, 0)),
      cum_netIncome = cumsum(coalesce(netIncome, 0)),
      
      # Calculate change in TotalDebt
      totalDebt_interp = na.approx(totalDebt, x = date, na.rm = FALSE),
      change_totalDebt = totalDebt_interp - lag(totalDebt_interp,1),
      cum_change_totalDebt = cumsum(coalesce(change_totalDebt, 0)),
      
      # Calculate full equity using the first value of totalStockholdersEquity
      full_equity = totalStockholdersEquity + (-1) * cum_dividends - cum_issued + (-1) * cum_repurchased - 
        cum_change_totalDebt,
      full_equity_noDebt = totalStockholdersEquity + (-1) * cum_dividends - cum_issued + (-1) * cum_repurchased
    ) %>% 
    ungroup()
  
  # Calculate CAGR
  df_cagr <- df_cagr %>%
    group_by(Ticker) %>% 
    arrange(date) %>% # Ensure data is sorted by date  
    mutate(
      first_totalStockholdersEquity = first(totalStockholdersEquity), # Equity at the first quarter
      full_equity_netIncome = first_totalStockholdersEquity + cum_netIncome, # Full equity 
      
      years_elapsed = as.numeric(difftime(date, first(date), units = "days")) / 365.25,
      
      CAGR.full.Equity = ifelse(years_elapsed > 0,
                                (full_equity / first_totalStockholdersEquity)^(1 / years_elapsed) - 1,
                                NA_real_), # Avoid divide-by-zero for the first quarter
      CAGR.full.Equity_noDebt = ifelse(years_elapsed > 0,
                                       (full_equity_noDebt / first_totalStockholdersEquity)^(1 / years_elapsed) - 1,
                                       NA_real_), # Avoid divide-by-zero for the first quarter
      CAGR.full.Equity_netIncome = ifelse(years_elapsed > 0,
                                          (full_equity_netIncome / first_totalStockholdersEquity)^(1 / years_elapsed) - 1,
                                          NA_real_) # Avoid divide-by-zero for the first quarter
      
    ) %>% 
    select(-totalDebt_interp) %>% 
    ungroup() %>% 
    arrange(desc(date))
  
  
  return(df_cagr)

}
# Negative FCF --------------------------------------------------
negative_FCF <- function(df){
  
  # 01 - Calculation quarters with negative FCF over total no. quarters -----------
  
  
  df <- df %>%
    group_by(Ticker) %>% 
    arrange(date) %>%  # Ensure data is sorted by date from oldest to latest
    mutate(
      # Cumulative count of negative free cash flow quarters
      cumulative_negative_fc_quarters = cumsum(freeCashFlow < 0),
      
      # Cumulative count of total quarters (1, 2, 3, ...)
      cumulative_total_quarters = row_number(),
      
      # Calculate the ratio of negative quarters to total quarters
      no.quarters.FCF_negative_ratio = cumulative_negative_fc_quarters / cumulative_total_quarters
    )  %>% 
    arrange(desc(date)) %>% 
    ungroup()
  
  # Prepare output
  df <- df %>% 
    select(
      Ticker, date, price,
      Earning.Power.per.Share.TTM,
      Owner.Earnings.Buffet.per.Share.TTM,
      Owner.Earnings.Buffet.IGVI.per.Share.TTM,
      Owner.Earnings.IGVI.per.Share.TTM,
      CAGR.full.Equity,
      no.quarters.FCF_negative_ratio,
      everything()
    )
  return(df)
}
# Multipliers ------------------------
multipliers <- function(df){
  
  # 01 - Calculation of multipliers -----------
  
  
  df <- df %>%
    mutate(
      Enterprise.Value.Op.Assets = marketCap_LocalFX + totalLiabilities - goodwillAndIntangibleAssets -
        excess_cash - 0.5* (otherCurrentAssets + otherNonCurrentAssets),
      
      EVops_EV = Enterprise.Value.Op.Assets / enterpriseValueTTM_LocalFX_KM_TTM,
      
      MktCap_EV = marketCap_LocalFX / Enterprise.Value.Op.Assets,
      
      Debt_EV = totalDebt / Enterprise.Value.Op.Assets,
      
      CurrentAssets_EV = totalCurrentAssets / Enterprise.Value.Op.Assets,
      
      NonCurrentAssets_EV = totalNonCurrentAssets / Enterprise.Value.Op.Assets,
      
      totalAssets_EV = totalAssets / Enterprise.Value.Op.Assets,
      
      ExcessCash_EV = excess_cash / Enterprise.Value.Op.Assets,
      
      FCF.4FQ = rollapply(freeCashFlow,
                          width = 4, FUN = sum, align = "left", fill = NA),
      
      Tangible_Equity_book = totalAssets - totalLiabilities - 
        goodwillAndIntangibleAssets,
      
      Equity_Net_Premium = marketCap_LocalFX - Tangible_Equity_book,
      
      Equity_Net_premiumToFCF= Equity_Net_Premium / FCF.4FQ,
      
      Net.Working.Capital = totalCurrentAssets - excess_cash
      - (totalCurrentLiabilities - shortTermDebt),
      
      NetWorkingCapital_EV = Net.Working.Capital / Enterprise.Value.Op.Assets
    ) 
  
  
  # Prepare output
  df <- df %>% 
    select(
      Ticker, date, price,
      Earning.Power.per.Share.TTM,
      Owner.Earnings.Buffet.per.Share.TTM,
      Owner.Earnings.Buffet.IGVI.per.Share.TTM,
      Owner.Earnings.IGVI.per.Share.TTM,
      CAGR.full.Equity,
      no.quarters.FCF_negative_ratio,
      EVops_EV,
      MktCap_EV,
      Debt_EV,
      totalAssets_EV,
      NetWorkingCapital_EV,
      Equity_Net_premiumToFCF,
      everything()
    )
  return(df)
}

# Capex vs Equity growth --------------------------------------------------

capex_equity_growth_plot <- function(fundamentals_df) {
  
  # Step 1: Calculate Capex ()
  fundamentals_df <- fundamentals_df %>%
    group_by(Ticker) %>% 
    arrange(date) %>%
    mutate(capex = capitalExpenditure,
           capex_TTM = rollapply(capex, width = 4, FUN = sum, fill = NA, align = "right")) %>% 
    ungroup()
  
  
  # Step 2: Calculate Equity Increases  2, 3, 4, 6 years prior
  fundamentals_df <- fundamentals_df %>%
    group_by(Ticker) %>% 
    arrange(desc(date)) %>% 
    mutate(
      equity_increase_annual = full_equity_netIncome - lead(full_equity_netIncome, 4),
      
      capex_TTM_2y = lead(capex_TTM,8),
      capex_TTM_3y = lead(capex_TTM,12),
      capex_TTM_4y = lead(capex_TTM,16),
      capex_TTM_8y = lead(capex_TTM,32)
      
    ) %>% 
    ungroup()
  
  
  # Step 3: Calculate Ratios using Capex 
  fundamentals_df <- fundamentals_df %>%
    group_by(Ticker) %>% 
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
  
  # Step 4: Replace outliers with NA for ratios
  fundamentals_df <- fundamentals_df %>%
    group_by(Ticker) %>%  # Apply outlier removal within each symbol group
    mutate(
      full_Equity_increase_capex_2y = replace_outliers_with_na(full_Equity_increase_capex_2y),
      full_Equity_increase_capex_3y = replace_outliers_with_na(full_Equity_increase_capex_3y),
      full_Equity_increase_capex_4y = replace_outliers_with_na(full_Equity_increase_capex_4y),
      full_Equity_increase_capex_8y = replace_outliers_with_na(full_Equity_increase_capex_8y)
    ) %>%
    ungroup()
  
  # Step 5: Calculate Correlations for Each Symbol
  correlation_results <- fundamentals_df %>%
    group_by(Ticker) %>%
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
    select(date, Ticker, full_Equity_increase_capex_2y, full_Equity_increase_capex_3y,
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
    facet_wrap(~ Ticker, scales = "fixed", ncol = 1)
  
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

# Seasonality --------------------------------------------------
seasonality <- function(fundamentals_df){
  library(tidyverse)
  library(lubridate)
  
  # Sample structure assumption
  # fundamentals_df <- data.frame(
  #   date = as.Date(c("2020-03-31", "2020-06-30", "2020-09-30", "2020-12-31", ...)),
  #   revenue = c(...),
  #   grossProfit = c(...),
  #   operatingIncome = c(...),
  #   netIncome = c(...)
  # )
  
  # Step 1: Add a 'quarter' column
  fundamentals_seasonality <- fundamentals_df %>%
    mutate(
      quarter = quarter(date, with_year = FALSE, fiscal_start = 1) %>% factor(levels = 1:4, labels = c("Q1", "Q2", "Q3", "Q4"))
    )
  
  # Step 2: Pivot longer for plotting
  fundamentals_long <- fundamentals_seasonality %>%
    pivot_longer(
      cols = c(revenue, grossProfit, operatingIncome, netIncome),
      names_to = "metric",
      values_to = "value"
    ) %>%
    mutate(
      value = value / 1e6,  # Convert to millions
      metric = factor(metric, levels = c("revenue", "grossProfit", "operatingIncome", "netIncome"))
    )
  
  # Step 3: Plot boxplots of each metric across quarters
  ggplot(fundamentals_long, aes(x = quarter, y = value)) +
    geom_boxplot(fill = "steelblue", color = "darkblue", outlier.color = "red", outlier.shape = 1) +
    facet_wrap(~ metric, scales = "free_y") +
    labs(
      title = "Seasonality of Financial Metrics by Quarter",
      x = "Quarter",
      y = "Value ($M)",
      caption = "Source: fundamentals_df"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      strip.background = element_rect(fill = "lightgrey", color = NA),
      strip.text = element_text(face = "bold")
    )
  
}


