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

export_excel_data <- function(DF1) {
  
  ## Create workbook
  wb <- createWorkbook()
  
  ## Add worksheets
  addWorksheet(wb, "Data")
  
  
  # Write DF1 and DF2 to worksheet if provided
  
  writeDataTable(
    wb,
    "Data",
    x = as.data.frame(DF1),
    colNames = TRUE,
    tableStyle = "TableStyleLight9",
    tableName = "Data_US_Stocks"
  )
  
  
  # Save workbook
  saveWorkbook(wb,
               file = paste0("data/dataset_", format(Sys.Date(), "%Y%m%d"), ".xlsx"), 
               overwrite = TRUE)
  # Check https://cran.r-project.org/web/packages/openxlsx/openxlsx.pdf
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
Reduce_FinancialsMetricsProfile <- function(FinancialsMetricsProfile) {

  FinancialsMetricsProfile$IncomeStatement <- FinancialsMetricsProfile$IncomeStatement %>% 
    mutate(
      date = as.Date(date),
      fillingDate = as.Date(fillingDate),
      acceptedDate = as.Date(acceptedDate),
      calendarYear = as.integer(calendarYear)
    )
  
  FinancialsMetricsProfile$BalanceSheet <- FinancialsMetricsProfile$BalanceSheet %>% 
    mutate(
      date = as.Date(date),
      fillingDate = as.Date(fillingDate),
      acceptedDate = as.Date(acceptedDate),
      calendarYear = as.integer(calendarYear)
    )
  
  FinancialsMetricsProfile$CashFlow <- FinancialsMetricsProfile$CashFlow %>% 
    mutate(
      date = as.Date(date),
      fillingDate = as.Date(fillingDate),
      acceptedDate = as.Date(acceptedDate),
      calendarYear = as.integer(calendarYear)
    )
  
  FinancialsMetricsProfile$KeyMetrics <- FinancialsMetricsProfile$KeyMetrics %>% 
    mutate(
      date = as.Date(date),
      calendarYear = as.integer(calendarYear)
    )
  
  FinancialsMetricsProfile$Ratios <- FinancialsMetricsProfile$Ratios %>% 
    mutate(
      date = as.Date(date),
      calendarYear = as.integer(calendarYear)
    )
  
  FinancialsMetricsProfile$Shares_Float <- FinancialsMetricsProfile$Shares_Float %>% 
    mutate(
      date = as.Date(date),
    )
  
  DF_IS <- FinancialsMetricsProfile$IncomeStatement
  DF_BS <- FinancialsMetricsProfile$BalanceSheet
  DF_CF <- FinancialsMetricsProfile$CashFlow
  DF_KM_TTM <- FinancialsMetricsProfile$KeyMetrics_TTM
  DF_KM <- FinancialsMetricsProfile$KeyMetrics
  DF_Ratios_TTM <- FinancialsMetricsProfile$Ratios_TTM
  DF_Ratios <- FinancialsMetricsProfile$Ratios
  DF_Shares_Float <- FinancialsMetricsProfile$Shares_Float
  DF_ST <- FinancialsMetricsProfile$symbols_df

  
  DF <- DF_ST %>%  
    left_join(DF_Ratios_TTM, by = c("symbol"))
  
  DF <- DF %>% 
    left_join(DF_KM_TTM, by = c("symbol")) %>% 
    select(-ends_with(".y")) %>%  # Remove .y columns
    rename_with(~ gsub("\\.x$", "", .), ends_with(".x"))  # Rename .x columns by removing the suffix
  
  DF <- DF %>%  
    left_join(DF_BS, by = c("symbol")) %>% 
    select(-ends_with(".y")) %>%  # Remove .y columns
    rename_with(~ gsub("\\.x$", "", .), ends_with(".x"))  # Rename .x columns by removing the suffix
  
  DF <- DF %>%
    left_join(DF_IS, by = c("date", "symbol")) %>% 
    select(-ends_with(".y")) %>%  # Remove .y columns
    rename_with(~ gsub("\\.x$", "", .), ends_with(".x"))  # Rename .x columns by removing the suffix
  
  DF <- DF %>%
    left_join(DF_CF,  by = c("date", "symbol")) %>% 
    filter(!is.na(freeCashFlow)) %>% 
    select(-ends_with(".y")) %>%  # Remove .y columns
    rename_with(~ gsub("\\.x$", "", .), ends_with(".x"))  # Rename .x columns by removing the suffix
  
  DF <- DF %>%
    left_join(DF_KM, by = c("date", "symbol")) %>% 
    select(-ends_with(".y")) %>%  # Remove .y columns
    rename_with(~ gsub("\\.x$", "", .), ends_with(".x"))  # Rename .x columns by removing the suffix
  
  DF <- DF %>%
    left_join(DF_Ratios, by = c("date", "symbol")) %>% 
    select(-ends_with(".y")) %>%  # Remove .y columns
    rename_with(~ gsub("\\.x$", "", .), ends_with(".x"))  # Rename .x columns by removing the suffix
  
  DF <- DF %>%
    left_join(DF_Shares_Float, by = c("date", "symbol")) %>% 
    select(-ends_with(".y")) %>%  # Remove .y columns
    rename_with(~ gsub("\\.x$", "", .), ends_with(".x"))  # Rename .x columns by removing the suffix
  
  DF <- DF %>% 
    mutate(across(where(is.integer), as.numeric))
  
  DF <- DF %>% 
    mutate(outstandingShares = as.numeric(outstandingShares))
  
  DF <- DF %>% 
    mutate(outstandingShares = ifelse(is.na(outstandingShares), weightedAverageShsOutDil, outstandingShares))
  
  return(DF)
}

