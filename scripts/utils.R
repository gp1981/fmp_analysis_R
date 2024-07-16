# Author: gp1981
# Date: 16 July 2024
# Purpose: Load all libraries and files
# Disclaimer: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.


# 01 - API, Packages and libraries -------------------------------------------------

packages <- c("httr","jsonlite","tidyverse", "openxlsx", "lubridate","tidyquant",
              "ggthemes","ggplot2","openxlsx","dplyr","zoo","ggpubr","foreach", 
              "progress", "ggplot2", "kableExtra", "openxlsx", "zoo")

for (package in packages) {
  if (!(package %in% installed.packages())) {
    install.packages(package)
  }
  
  # Load the package
  library(package, character.only = TRUE)
}

# 02 - Utility functions -------------------------------------------------------
## API ---------------------------------------------------------------------

# Load API Key
API_Key = rstudioapi::askForSecret("API_FMP_KEY")

## Dates setting ---------------------------------------------------------------

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
               file = paste0("data/dataset", ".xlsx"),
               overwrite = TRUE)
  # Check https://cran.r-project.org/web/packages/openxlsx/openxlsx.pdf
}

# 03 - Files to source ---------------------------------------------------------
source('scripts/data_retrieval.R')