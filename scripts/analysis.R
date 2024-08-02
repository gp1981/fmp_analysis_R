# Author: gp1981
# Date: 16 July 2024
# Purpose: Perform analysis
# Disclaimer: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.

# 01 - MF ranking   --------------------------------------------------------------
calculate_MF_ranking <- function(df){
  ## 01.1 - Calculation of 4FQ rolling sum of Op. Income
  df <- df %>% 
    group_by(symbol) %>%
    arrange(desc(date)) %>% 
    mutate(EBIT.4FQ = rollapply(operatingIncome,
                                width = 4, FUN = sum, align = "left", fill = NA)) %>% 
    
    mutate(Excess_Cash = cashAndCashEquivalents * 0.9 + shortTermInvestments) %>% 
    
    mutate(Net_Working_Capital = (totalCurrentAssets - Excess_Cash) - (totalCurrentLiabilities - shortTermDebt),
           
           Tangible_Capital_Employed = totalAssets - (goodwillAndIntangibleAssets 
                                                      + Excess_Cash + 
                                                        totalCurrentLiabilities - 
                                                        shortTermDebt)) %>% 
    
    mutate(Return_On_Capital_Employed = EBIT.4FQ / Tangible_Capital_Employed,
           Net_Interest_Bearing_Debt = totalDebt + capitalLeaseObligations) %>% 
    
    mutate(Enterprise_Value = mktCap + Net_Interest_Bearing_Debt + 
             minorityInterest + preferredStock) %>% 
    
    mutate(Earnings_Yield = EBIT.4FQ / Enterprise_Value) %>% 
    
    select(date,symbol, companyName, mktCap, Enterprise_Value, Earnings_Yield, Return_On_Capital_Employed,
           EBIT.4FQ, Tangible_Capital_Employed, Net_Working_Capital, Excess_Cash, everything()) %>% 
    ungroup()
  
  df <- EY_ROCE_ranking(df)
  
  return(df)
}

EY_ROCE_ranking <- function(df){
  df <- df %>% 
    mutate(aux.rank = "A") # Auxiliary variable
  
  df <- df %>%
    group_by(symbol) %>%
    arrange(desc(date)) %>%
    slice(1) %>%
    ungroup()
  
  df <- df %>% 
    filter(!is.na(Return_On_Capital_Employed) & is.finite(Return_On_Capital_Employed)) %>% 
    group_by(aux.rank) %>% 
    arrange(desc(Return_On_Capital_Employed)) %>% 
    mutate(Rank_Return_On_Capital_Employed = dplyr::row_number()) %>% 
    select(date,symbol, companyName, mktCap, Enterprise_Value, Earnings_Yield, Rank_Return_On_Capital_Employed,
           Return_On_Capital_Employed, EBIT.4FQ, Tangible_Capital_Employed, Net_Working_Capital, 
           Excess_Cash, everything()) %>% 
    ungroup()
  
  df <- df %>% 
    filter(!is.na(Earnings_Yield) & is.finite(Earnings_Yield) ) %>% 
    group_by(aux.rank) %>% 
    arrange(desc(Earnings_Yield)) %>% 
    mutate(Rank_Earnings_Yield = dplyr::row_number()) %>% 
    select(date,symbol, companyName, mktCap, Enterprise_Value, Rank_Earnings_Yield, 
           Earnings_Yield, Rank_Return_On_Capital_Employed, Return_On_Capital_Employed, 
           EBIT.4FQ, Tangible_Capital_Employed, Net_Working_Capital, Excess_Cash, 
           everything())%>% 
    ungroup()
  
  df <- df %>% 
    group_by(aux.rank) %>%
    mutate(Rank_EY_ROCE_absolute = Rank_Return_On_Capital_Employed + Rank_Earnings_Yield)%>% 
    ungroup()
  
  df <- df %>% 
    group_by(aux.rank) %>%
    arrange(Rank_EY_ROCE_absolute) %>% 
    mutate(Rank_EY_ROCE = dplyr::row_number()) %>%
    select(date,symbol, companyName, mktCap, Rank_EY_ROCE, Enterprise_Value, Rank_Earnings_Yield, 
           Earnings_Yield, Rank_Return_On_Capital_Employed, Return_On_Capital_Employed, 
           EBIT.4FQ, Tangible_Capital_Employed, Net_Working_Capital, Excess_Cash, 
           everything())%>% 
    ungroup()
  
  
  return(df)
}

