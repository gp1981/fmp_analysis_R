# Author: gp1981
# Date: 16 July 2024
# Purpose: Load all libraries and files
# Disclaimer: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.


# Packages and libraries -------------------------------------------------

packages <- c("httr","jsonlite","tidyverse", "openxlsx", "lubridate","tidyquant",
              "ggthemes","ggplot2","openxlsx","dplyr","zoo","ggpubr","foreach", 
              "progress", "ggplot2", "kableExtra", "knitr", "openxlsx", "zoo", 
              "rvest", "scales")

for (package in packages) {
  if (!(package %in% installed.packages())) {
    install.packages(package)
  }
  
  # Load the package
  library(package, character.only = TRUE)
}

# Utility functions -------------------------------------------------------
## API Token ---------------------------------------------------------------------

# Load API Key
API_Key = rstudioapi::askForSecret("API_FMP_KEY")

## Historical dates setting ---------------------------------------------------------------

# Get today's date
today_date <- as.Date(Sys.Date())

# Calculate the historical dates
historical_dates <- data.frame(
  date_25Y = (today_date - years(25)),
  date_20Y = (today_date - years(20)),
  date_10Y = (today_date - years(10)),
  date_5Y = (today_date - years(5)),
  date_3Y = (today_date - years(3)),
  date_1Y = (today_date - years(1)),
  date_6M = (today_date - months(6)),
  date_1M = (today_date - months(1)),
  date_1W = (today_date - weeks(1))
)

## Export data into xlsx table -------------------------------------------------

export_excel_data <- function(DF1, new_suffix = "") {
  
  ## Create workbook
  wb <- createWorkbook()
  
  ## Add worksheets
  addWorksheet(wb, "Data")
  
  ## Write DF1 to the worksheet
  writeDataTable(
    wb,
    "Data",
    x = as.data.frame(DF1),
    colNames = TRUE,
    tableStyle = "TableStyleLight9",
    tableName = "Data"
  )
  
  ## Construct the filename with the new_suffix
  file_name <- paste0("data/dataset_",new_suffix, format(Sys.Date(), "%Y%m%d"), ".xlsx")
  
  ## Save workbook
  saveWorkbook(wb, file = file_name, overwrite = TRUE)
  
  ## Check https://cran.r-project.org/web/packages/openxlsx/openxlsx.pdf
}

## Function to search for specific word in column names and retrieve matching columns ----

# Function to ensure columns have consistent types
ensure_consistent_types <- function(df_list) {
  all_colnames <- unique(unlist(lapply(df_list, colnames)))
  
  for (i in seq_along(df_list)) {
    for (col in all_colnames) {
      if (!col %in% colnames(df_list[[i]])) {
        df_list[[i]][[col]] <- NA
      }
    }
    df_list[[i]] <- df_list[[i]][, all_colnames]
  }
  
  return(df_list)
}

# Function to search for specific words in column names and retrieve matching columns
search_and_retrieve_columns <- function(df_list, words) {
  # Initialize an empty list to store the extracted data frames
  extracted_data <- list()
  
  # Iterate over each data frame in the list
  for (name in names(df_list)) {
    df <- df_list[[name]]
    
    # Find columns that contain any of the specified words
    matching_cols <- names(df)[str_detect(names(df), paste(words, collapse = "|"))]
    
    if (length(matching_cols) > 0) {
      # Select the matching columns and add to the list
      extracted_data[[name]] <- df %>% select(date, symbol, all_of(matching_cols))
    } else {
      # Select date and symbol, fill remaining columns with NAs
      extracted_data[[name]] <- df %>% select(date, symbol) %>%
        mutate(across(everything(), ~ NA_character_))
    }
  }
  
  # Ensure all dataframes have consistent column types
  extracted_data <- ensure_consistent_types(extracted_data)
  
  # Combine all the extracted data frames into a single data frame
  combined_df <- bind_rows(extracted_data, .id = "stock")
  
  return(combined_df)
}
## Extract specific variables from each data frame in a list -------------------
extract_specific_variables <- function(df_list, variables) {
  # Initialize an empty list to store the extracted data frames
  extracted_data <- list()
  
  # Iterate over each data frame in the list
  for (name in names(df_list)) {
    df <- df_list[[name]]
    
    # Check for missing variables and add them as NA if not present
    missing_vars <- setdiff(variables, names(df))
    if (length(missing_vars) > 0) {
      df[missing_vars] <- NA
    }
    
    # Select the specified variables and add to the list
    extracted_data[[name]] <- df %>% select(all_of(variables))
  }
  
  # Combine all the extracted data frames into a single data frame
  combined_df <- bind_rows(extracted_data, .id = "stock")
  
  return(combined_df)
}

# Reduce the list "FinancialsMetricsProfile" and remove columns with suffixes ".1", ".2", ".x", ".y" that are duplicates or wrong matches
reduce_financialsMetricsProfile <- function(FinancialsMetricsProfile) {
  
  # Helper function
  clean_joins <- function(df) {
    df %>%
      select(-matches("\\.y$")) %>%
      rename_with(~ gsub("\\.x$", "", .), matches("\\.x$"))
  }
  
  # Helper function to prioritize TTM values
  prioritize_TTM <- function(dynamic_value, ttm_value, row) {
    if_else(!is.na(ttm_value) & row == 1, ttm_value, dynamic_value)
  }
  
  # --- Convert dates properly ---
  FinancialsMetricsProfile <- lapply(FinancialsMetricsProfile, function(df) {
    if ("date" %in% names(df)) df$date <- as.Date(df$date)
    if ("filingDate" %in% names(df)) df$filingDate <- as.Date(df$filingDate)
    if ("acceptedDate" %in% names(df)) df$acceptedDate <- as.Date(df$acceptedDate)
    if ("fiscalYear" %in% names(df)) df$fiscalYear <- as.integer(df$fiscalYear)
    if ("ipoDate" %in% names(df)) df$ipoDate <- as.Date(df$ipoDate)
    return(df)
  })
  
  # --- Extract individual DataFrames ---
  DF_IS <- FinancialsMetricsProfile$IncomeStatement
  DF_BS <- FinancialsMetricsProfile$BalanceSheet
  DF_CF <- FinancialsMetricsProfile$CashFlow
  DF_KM_TTM <- FinancialsMetricsProfile$KeyMetrics_TTM
  DF_KM <- FinancialsMetricsProfile$KeyMetrics
  DF_Ratios_TTM <- FinancialsMetricsProfile$Ratios_TTM
  DF_Ratios <- FinancialsMetricsProfile$Ratios
  DF_Shares_Float <- FinancialsMetricsProfile$Shares_Float
  DF_EV <- FinancialsMetricsProfile$EV
  DF_Profile <- FinancialsMetricsProfile$Stock_List_data
  
  # --- Rename variables
  if (all(c("accountsReceivables", "inventory", "accountsPayables",
            "otherWorkingCapital","otherNonCashItems") %in% names(DF_CF))) {
    DF_CF <- DF_CF %>% rename(
      Change_accountsReceivables = accountsReceivables,
      Change_inventory = inventory,
      Change_accountsPayables = accountsPayables,
      Change_otherWorkingCapital = otherWorkingCapital,
      Change_otherNonCashItems = otherNonCashItems
    )
  }
  
  if ("marketCap" %in% names(DF_KM_TTM)) {
    DF_KM_TTM <- DF_KM_TTM %>% rename(
      marketCap_TTM = marketCap
    )
  }
  
  if ("marketCap" %in% names(DF_EV)) {
    DF_EV <- DF_EV %>% rename(
      marketCap_EV = marketCap,
      enterpriseValue_EV = enterpriseValue
    )
  }
  
  if ("date" %in% names(DF_Shares_Float)) {
    DF_Shares_Float <- DF_Shares_Float %>% rename(
      share_float_date = date
    )
  }
  
  if ("marketCap" %in% names(DF_Profile)) {
    DF_Profile <- DF_Profile %>% rename(
      marketCap_Profile = marketCap
    )
  }
  
  if ("marketCap" %in% names(DF_KM)) {
    DF_KM <- DF_KM %>% rename(
      marketCap_KM = marketCap
    )
  }
  
  if ("date" %in% names(DF_Shares_Float)) {
    DF_Shares_Float <- DF_Shares_Float %>% rename(
      share_float_date = date
    )
  }
  
 # --- Merge TTM values ---
  DF_TTM <- DF_Profile %>%
    left_join(DF_Ratios_TTM, by = intersect(names(DF_Ratios_TTM),names(DF_Profile)))
  
  DF_TTM <- DF_TTM %>% 
    left_join(DF_KM_TTM, by = intersect(names(DF_KM_TTM),names(DF_TTM)))
  
  # --- Merge historical data ---
  DF <- DF_BS %>% 
    left_join(DF_IS, by = intersect(names(DF_IS),names(DF_BS)))
  
  DF <- DF %>% 
    left_join(DF_CF, by = c("Ticker","date"))
  
  DF <- DF %>% 
    left_join(DF_KM, by = c("Ticker","date"))
  
  DF <- DF %>% 
    left_join(DF_EV, c("Ticker","date"))
  
  # --- Merge recent data ---
  DF <- DF %>% 
    left_join(DF_TTM, by = c("Ticker"))
  
  DF <- DF %>% 
    left_join(DF_Shares_Float, by = c("Ticker"))
  
  DF <- DF %>%
    left_join(DF_Ratios, by = c(intersect(names(DF),names(DF_Ratios))))
  
  # Replace the first row value
  DF <- DF %>%
    group_by(Ticker) %>% 
    arrange(desc(date)) %>% 
    mutate(
      marketCap = ifelse(row_number() == 1, marketCap_Profile, marketCap_TTM),
      enterpriseValue = ifelse(row_number() == 1, enterpriseValue_EV, enterpriseValueTTM)
    )
  
  
  # --- Clean up suffixes from joins ---
  clean_join_suffixes <- function(df) {
    df <- df %>% select(-matches("\\.y$"))
    
    cols_x <- names(df)[str_ends(names(df), "\\.x$")]
    clean_names <- str_remove(cols_x, "\\.x$")
    
    df <- df %>% select(-any_of(clean_names))
    df %>% rename_with(~ str_remove(., "\\.x$"), ends_with(".x"))
  }
  DF <- clean_join_suffixes(DF)
  
  
  # --- Standardize numeric types ---
  DF <- DF %>% 
    mutate(across(where(is.integer), as.numeric)) %>%
    mutate(outstandingShares = as.numeric(outstandingShares)) %>%
    mutate(outstandingShares = if_else(is.na(outstandingShares), weightedAverageShsOutDil, outstandingShares))
  return(DF)
}



## Extract TTM  fundamentals -----------------------------------------------
print_fundamentals_TTM <- function(df, Ticker){
  df <- df %>% 
    filter(symbol== Ticker) %>% select(date,symbol,
                                       revenue_TTM,
                                       costOfRevenue_TTM,
                                       sellingGeneralAndAdministrativeExpenses_TTM, 
                                       otherExpenses_TTM,
                                       researchAndDevelopmentExpenses_TTM,
                                       operatingIncome_TTM, 
                                       incomeTaxExpense_TTM,
                                       netIncome_TTM,
                                       operatingCashFlow_TTM,
                                       dividendsPaid_TTM,
                                       commonStockIssued_TTM,
                                       commonStockRepurchased_TTM)
  
  df <- df %>% mutate(across(.cols = where(is.numeric), .fns = ~ . /1e06))
  
  export_excel_data(df)
  return(df)
}

