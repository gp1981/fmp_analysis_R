# Author: gp1981
# Date: 16 July 2024
# Purpose: Retrieve data
# Disclaimer: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.

# 01 - Get data   --------------------------------------------------------------

## 01.1 - Get data from fmp ----------------------------------------
get_stock_data_df <- function(API_Key){
  # Prepare URL for accessing Symbol List API
  Symbol_List_API_path <- "https://financialmodelingprep.com/api/v3/stock/list?apikey="
  Symbol_List_API_path <- paste0(Symbol_List_API_path, API_Key)
  
  # Download stock list data via API
  Symbol_List <- fromJSON(Symbol_List_API_path)
  
  # Download financial statement symbol lists via API
  Financial_Statement_Symbol_List <- "https://financialmodelingprep.com/api/v3/financial-statement-symbol-lists?apikey="
  Financial_Statement_Symbol_List <- paste0(Financial_Statement_Symbol_List, API_Key)
  Financial_Statement_Symbol_List <- fromJSON(Financial_Statement_Symbol_List)
  
  
  # Download S&P500 constituent list via API
  SP500_Symbol_list <- "https://financialmodelingprep.com/api/v3/sp500_constituent?apikey="
  SP500_Symbol_list <- paste0(SP500_Symbol_list, API_Key)
  SP500_Symbol_list <- fromJSON(SP500_Symbol_list)
  
  # Download NASDAQ constituent list via API
  NASDAQ_Symbol_list <- "https://financialmodelingprep.com/api/v3/nasdaq_constituent?apikey="
  NASDAQ_Symbol_list <- paste0(NASDAQ_Symbol_list, API_Key)
  NASDAQ_Symbol_list <- fromJSON(NASDAQ_Symbol_list)
  
  # Download Dow Jones constituent list via API
  DOW_Symbol_list <- "https://financialmodelingprep.com/api/v3/dowjones_constituent?apikey="
  DOW_Symbol_list <- paste0(DOW_Symbol_list, API_Key)
  DOW_Symbol_list <- fromJSON(DOW_Symbol_list)
  
  # Convert JSON data into data frames
  symbol_df <- as.data.frame(Symbol_List)
  Financial_Statement_symbol_df <- as.data.frame(Financial_Statement_Symbol_List)
  SP500_symbol_df <- as.data.frame(SP500_Symbol_list)
  NASDAQ_symbol_df <- as.data.frame(NASDAQ_Symbol_list)
  DOW_symbol_df <- as.data.frame(DOW_Symbol_list)
  
  # Add a column to mark only stocks with financial statements
  Financial_Statement_symbol_df <- Financial_Statement_symbol_df %>% 
    mutate(Financial_statements = 'TRUE') %>% 
    rename(symbol = Financial_Statement_Symbol_List)
  
  # Add a column to mark only the S&P500, NASDAQ and Dow Jones stocks
  SP500_symbol_df <- SP500_symbol_df %>% 
    mutate(SP500 = 'S&P500') %>% 
    select(-name, -dateFirstAdded)
  
  NASDAQ_symbol_df <- NASDAQ_symbol_df %>% 
    mutate(NASDAQ = 'NASDAQ') %>% 
    select(-c("name","dateFirstAdded", "sector","subSector","headQuarter","cik", "founded"))
  
  DOW_symbol_df <- DOW_symbol_df %>% 
    mutate(Dow_Jones = 'DOW') %>% 
    select(-c("name","dateFirstAdded", "sector","subSector","headQuarter","cik", "founded"))
  
  # Merge stock data with financial statement symbol list and key indexes
  symbol_df  <- semi_join(symbol_df , Financial_Statement_symbol_df, by = "symbol")
  symbol_df  <- left_join(symbol_df , SP500_symbol_df, by = "symbol")
  symbol_df  <- left_join(symbol_df , NASDAQ_symbol_df, by = "symbol" )
  symbol_df  <- left_join(symbol_df , DOW_symbol_df, by = "symbol")
  
  return(symbol_df)
}

get_fundamentals_data_df <- function(symbols_df, period, limit, API_Key){
  
  # Create API URLs for various calls to collect Financial Statements
  API_IncomeStatement_path_base <- 'https://financialmodelingprep.com/api/v3/income-statement/'
  API_BalanceSheet_path_base <- 'https://financialmodelingprep.com/api/v3/balance-sheet-statement/'
  API_CashFlow_path_base <- 'https://financialmodelingprep.com/api/v3/cash-flow-statement/'
  API_Profile_path_base <- 'https://financialmodelingprep.com/api/v3/profile/'
  API_KeyMetrics_path_base <- 'https://financialmodelingprep.com/api/v3/key-metrics-ttm/'
  API_Ratio_path_base <- 'https://financialmodelingprep.com/api/v3/ratios-ttm/'
  
  if (period == "quarter") {
    API_IncomeStatement_path_suffix <- '?period=quarter'
    API_BalanceSheet_path_suffix <- '?period=quarter'
    API_CashFlow_path_suffix <- '?period=quarter'
  } else {
    API_IncomeStatement_path_suffix <- '?period=annual'
    API_BalanceSheet_path_suffix <- '?period=annual'
    API_CashFlow_path_suffix <- '?period=annual'
  }
  
  API_IncomeStatement_path <- paste0(API_IncomeStatement_path_base, symbols_df$symbol, API_IncomeStatement_path_suffix, '&limit=', limit, '&apikey=', API_Key)
  API_BalanceSheet_path <- paste0(API_BalanceSheet_path_base, symbols_df$symbol, API_BalanceSheet_path_suffix, '&limit=', limit, '&apikey=', API_Key)
  API_CashFlow_path <- paste0(API_CashFlow_path_base, symbols_df$symbol, API_CashFlow_path_suffix, '&limit=', limit, '&apikey=', API_Key)
  API_Profile_path <- paste0(API_Profile_path_base, symbols_df$symbol, '?apikey=', API_Key)
  API_KeyMetrics_path <- paste0(API_KeyMetrics_path_base, symbols_df$symbol, '?apikey=', API_Key)
  API_Ratio_path <- paste0(API_Ratio_path_base, symbols_df$symbol,'?apikey=', API_Key)
  
  # Progress bar
  total_symbols <- nrow(symbols_df) * 6 # Adjust the total to the number of different data
  pb <- progress_bar$new(
    format = "  [:bar] :percent in :elapsed",
    total = total_symbols, 
    width = 60
  )
  
  # Function to retrieve all statements, key metrics, profile and ratios from symbols_df
  fetch_fundamentals_data <- function(paths, type) {
    bind_rows(lapply(1:length(paths), function(x) {
      pb$tick()
      tryCatch({
        data <- fromJSON(paths[x])
        # Explicitly check for Key Metrics or Ratios to add the ticker
        if (type == "Key Metrics" || type == "Ratios") {
          if (length(data) == 0) {
            NULL
          } else {
            # Convert to dataframe and add the ticker symbol explicitly
            result <- data.frame(data)
            result$symbol <- symbols_df$symbol[x]  # Add ticker symbol
            return(result)
          }
        } else {
          if (length(data) == 0) {
            NULL
          } else {
            data.frame(data)
          }
        }
      }, error = function(cond) {
        message(paste("API provided an error for", type, "Ticker:", symbols_df$symbol[x]))
        message("Here's the original error message:")
        message(cond)
        return(NULL)
      }, warning = function(cond) {
        message(paste("API provided a warning for", type, "Ticker:", symbols_df$symbol[x]))
        message("Here's the original warning message:")
        message(cond)
        return(NULL)
      })
    }))
  }
  
  IS <- fetch_fundamentals_data(API_IncomeStatement_path, "Income Statement")
  BS <- fetch_fundamentals_data(API_BalanceSheet_path, "Balance Sheet")
  CF <- fetch_fundamentals_data(API_CashFlow_path, "Cash Flow")
  KeyMetrics <- fetch_fundamentals_data(API_KeyMetrics_path, "Key Metrics")
  Profile <- fetch_fundamentals_data(API_Profile_path, "Profile data")
  Ratios <- fetch_fundamentals_data(API_Ratio_path, "Ratios")
  
  # Formatting data
  IS <- IS %>% 
    mutate(across(c(date,fillingDate,acceptedDate), as.Date)) %>% 
    mutate_at(vars(calendarYear), as.integer) 
  
  BS <- BS %>% 
    mutate(across(c(date,fillingDate,acceptedDate), as.Date)) %>% 
    mutate_at(vars(calendarYear), as.integer)
  
  CF <- CF %>% 
    mutate(across(c(date,fillingDate,acceptedDate), as.Date)) %>% 
    mutate_at(vars(calendarYear), as.integer) %>% 
    rename(
      change_inventory = inventory,
      change_accountsReceivables = accountsReceivables,
      change_accountsPayables = accountsPayables,
      change_otherWorkingCapital = otherWorkingCapital,
      change_otherNonCashItems = otherNonCashItems
    )
  
  Profile <- Profile %>% 
    mutate_at(vars(ipoDate), as.Date) %>% 
    mutate(Statement = "Profile")
  
  # Combine all data into a dataframe or a single data frame, depending on your needs
  # Ensure the 'symbol' column exists and is consistent across all dataframes
  stopifnot("symbol" %in% colnames(symbols_df))
  stopifnot("symbol" %in% colnames(IS))
  stopifnot("symbol" %in% colnames(BS))
  stopifnot("symbol" %in% colnames(CF))

  
  # Check for NA values in the key columns
  sum(is.na(symbols_df$symbol))
  sum(is.na(IS$symbol))
  sum(is.na(BS$symbol))
  sum(is.na(CF$symbol))
  sum(is.na(Profile$symbol))
  
  # Perform joins step-by-step and inspect the results
  df <- Profile %>%
    left_join(Ratios, by = "symbol") %>%
    left_join(KeyMetrics, by = "symbol", suffix = c("_Ratios", "_KeyMetrics")) %>%
    
    # Resolve duplicate columns conditionally
    mutate(across(ends_with("_Ratios"), 
                  ~ coalesce(.x, get(sub("_Ratios", "_KeyMetrics", cur_column()))), 
                  .names = "{.col}_final")) %>%
    
    # Clean up column names: retain only the resolved values
    select(-ends_with("_Ratios"), -ends_with("_KeyMetrics")) %>%
    rename_with(~ sub("_Ratios_final", "", .), ends_with("_final"))

  fundamentals <- df %>% 
    left_join(CF, by = c("symbol")) %>% 
    select(-ends_with(".x"), -ends_with(".y"))
  
  fundamentals <- fundamentals %>% 
    left_join(IS, by = c("symbol","date")) %>% 
    select(-ends_with(".x"), -ends_with(".y"))
  
  fundamentals <- fundamentals %>% 
    left_join(BS, by = c("symbol","date")) %>% 
    select(-ends_with(".x"), -ends_with(".y"))

  ## Prepare output ----------
  
  # Prepare dataframe with necessary columns
  symbols_df <- symbols_df %>%
    select(-any_of(c("name", "price")))
  
  # Combine dataframes
  fundamentals <- fundamentals %>% 
    left_join(symbols_df, by = "symbol")
  
  # Get the names of the columns in the combined dataframe
  column_names <- names(fundamentals)
  
  # Find columns that end with ".y"
  y_columns <- grep("\\.y$", column_names, value = TRUE)
  
  # Remove columns with ".y" suffix
  fundamentals <- fundamentals %>% select(-all_of(y_columns))
  
  # Find columns that end with ".x"
  x_columns <- grep("\\.x$", column_names, value = TRUE)
  
  # Function to remove the ".x" suffix
  remove_x_suffix <- function(name) {
    sub("\\.x$", "", name)
  }
  
  # Rename columns with ".x" suffix to remove the suffix
  fundamentals <- fundamentals %>%
    rename_with(remove_x_suffix, all_of(x_columns))
  
  return(fundamentals)
}

get_financial_statements_as_reported_list <- function(symbols_df, period, limit, API_Key) {
  
  # Base URL for the API
  API_financial_as_reported_base <- 'https://financialmodelingprep.com/api/v3/financial-statement-full-as-reported/'
  
  # URL suffix based on the period
  API_financial_as_reported_suffix <- ifelse(period == "quarter", '?period=quarter', '?period=annual')
  
  # Construct full API URLs
  API_paths <- paste0(API_financial_as_reported_base, symbols_df$symbol, API_financial_as_reported_suffix, '&limit=', limit, '&apikey=', API_Key)
  
  # Progress bar
  total_symbols <- nrow(symbols_df)
  pb <- progress::progress_bar$new(
    format = "  [:bar] :percent in :elapsed",
    total = total_symbols,
    width = 60
  )
  
  # Function to fetch data from JSON URLs
  fetch_fundamentals_as_reported <- function(paths) {
    data_list <- lapply(1:length(paths), function(i) {
      pb$tick()
      tryCatch({
        data <- fromJSON(paths[i])
        if (length(data) > 0) {
          data_frame <- data.frame(data)
          data_frame$date <- data_frame$date %>% as.Date()
          return(data_frame)
        } else {
          return(NULL)
        }
      }, error = function(cond) {
        message(paste("API provided an error for Ticker:", symbols_df$symbol[i]))
        message("Error message:", cond)
        return(NULL)
      }, warning = function(cond) {
        message(paste("API provided a warning for Ticker:", symbols_df$symbol[i]))
        message("Warning message:", cond)
        return(NULL)
      })
    })
    names(data_list) <- symbols_df$symbol
    return(data_list)
  }
  
  # Fetch data
  data_list <- fetch_fundamentals_as_reported(API_paths)
  
  # Combine all data frames into a list
  df_list <- lapply(data_list, function(df) {
    if (!is.null(df)) {
      return(df)
    } else {
      return(data.frame())
    }
  })
  
  
  # Return the list of data frames
  return(df_list)
}

get_price_history_data_df <- function(symbols_df, startDate, endDate , API_Key){
  
  # Create API URLs for various calls to collect historical price
  API_historical_price_path_base <- 'https://financialmodelingprep.com/api/v3/historical-price-full/'
  API_historical_price_path <- paste0(API_historical_price_path_base, symbols_df$symbol, '?from=', startDate, '&to=', endDate,  '&apikey=', API_Key)
  
  # Progress bar
  total_symbols <- nrow(symbols_df) # Adjust the total to the number of different data
  pb <- progress_bar$new(
    format = "  [:bar] :percent in :elapsed",
    total = total_symbols, 
    width = 60
  )
  
  # Function to retrieve all statements, key metrics, profile and ratios from symbols_df
  fetch_historical_price_data <- function(paths) {
    bind_rows(lapply(1:length(paths), function(x) {
      pb$tick()
      tryCatch({
        data <- fromJSON(paths[x])
        if (length(data) == 0) {
          NULL
        } else {
          data.frame(data)
        }
      }, error = function(cond) {
        message(paste("API provided an error for", type, "Ticker:", symbols_df$symbol[x]))
        message("Here's the original error message:")
        message(cond)
        return(NULL)
      }, warning = function(cond) {
        message(paste("API provided a warning for", type, "Ticker:", symbols_df$symbol[x]))
        message("Here's the original warning message:")
        message(cond)
        return(NULL)
      })
    }))
  }
  
  historical_price_df <- fetch_historical_price_data(API_historical_price_path) %>% as.data.frame()
  
}

get_hist_index_df <- function(index, API_Key) {
  
  get_hist_index_data <- function(api_hist_path, api_path) {
    # Connect to FMP data and create data frames
    df_Hist <- fromJSON(api_hist_path)
    df_Current <- fromJSON(api_path)
    
    # Convert JSON data into data frames
    df_Hist <- as.data.frame(df_Hist)
    df_Current <- as.data.frame(df_Current)
    
    # Convert date formats
    df_Hist <- df_Hist %>%
      mutate(dateAdded = mdy(dateAdded))
    
    df_Current <- df_Current %>%
      mutate(dateFirstAdded = as.Date(dateFirstAdded, format = "%Y-%m-%d"),
             founded = as.Date(founded, format = "%Y"))
    
    # Split historical data into two data frames by filtering empty values
    df_Hist_Added <- df_Hist %>% 
      filter(removedTicker == "")
    
    df_Hist_Removed <- df_Hist %>% 
      filter(addedSecurity == "")
    
    # Add today's date
    df_Current <- df_Current %>% 
      mutate(date = as.Date(Sys.Date())) %>% 
      select(date, everything())
    
    # Create a vector of dates of changes
    dateseq <- df_Hist %>% 
      select(dateAdded) %>% 
      mutate(date = as.Date(dateAdded, format = "%Y-%m-%d")) %>% 
      select(-dateAdded) %>% 
      distinct(date)
    
    # Initialize the full historical constituents
    df_Full <- df_Current
    
    # # Progress bar
    pb <- progress::progress_bar$new(
      format = "  [:bar] :percent in :elapsed",
      total = nrow(dateseq), 
      width = 60
    )
    
    # Loop through each date in dateseq
    for (i in 1:nrow(dateseq)) {
      pb$tick()
      date_n <- dateseq$date[i]
      
      # Identify securities that have been removed and added
      removed <- df_Hist_Removed %>% 
        filter(date == date_n) %>% 
        mutate(date = as.Date(date, format = "%Y-%m-%d"))
      
      added <- df_Hist_Added %>% 
        filter(date == date_n) %>% 
        mutate(date = as.Date(date, format = "%Y-%m-%d"))
      
      # Recreate constituents for previous period
      df_n_1 <- df_Full %>% 
        filter(date == min(date)) %>% 
        select(-date) %>% 
        mutate(date = date_n) %>% 
        select(date, everything()) %>% 
        anti_join(added, by = "symbol") %>% 
        bind_rows(removed) %>% 
        select(-(8:14))
      
      df_Full <- df_Full %>% 
        bind_rows(df_n_1)
    }
    
    return(df_Full)
  }
  
  # Define API paths for each index
  if (index == "SP500") {
    API_Hist_path <- paste0('https://financialmodelingprep.com/api/v3/historical/sp500_constituent?apikey=', API_Key)
    API_Current_path <- paste0('https://financialmodelingprep.com/api/v3/sp500_constituent?apikey=', API_Key)
    df <- get_hist_index_data(API_Hist_path, API_Current_path)
  } else if (index == "NASDAQ") {
    API_Hist_path <- paste0('https://financialmodelingprep.com/api/v3/historical/nasdaq_constituent?apikey=', API_Key)
    API_Current_path <- paste0('https://financialmodelingprep.com/api/v3/nasdaq_constituent?apikey=', API_Key)
    df <- get_hist_index_data(API_Hist_path, API_Current_path)
  } else if (index == "DOW") {
    API_Hist_path <- paste0('https://financialmodelingprep.com/api/v3/historical/dowjones_constituent?apikey=', API_Key)
    API_Current_path <- paste0('https://financialmodelingprep.com/api/v3/dowjones_constituent?apikey=', API_Key)
    df <- get_hist_index_data(API_Hist_path, API_Current_path)
  } else if (index == "ALL") {
    API_SP500_Hist_path <- paste0('https://financialmodelingprep.com/api/v3/historical/sp500_constituent?apikey=', API_Key)
    API_SP500_Current_path <- paste0('https://financialmodelingprep.com/api/v3/sp500_constituent?apikey=', API_Key)
    
    API_NASDAQ_Hist_path <- paste0('https://financialmodelingprep.com/api/v3/historical/nasdaq_constituent?apikey=', API_Key)
    API_NASDAQ_Current_path <- paste0('https://financialmodelingprep.com/api/v3/nasdaq_constituent?apikey=', API_Key)
    
    API_DOW_Hist_path <- paste0('https://financialmodelingprep.com/api/v3/historical/dowjones_constituent?apikey=', API_Key)
    API_DOW_Current_path <- paste0('https://financialmodelingprep.com/api/v3/dowjones_constituent?apikey=', API_Key)
    
    df_SP500 <- get_hist_index_data(API_SP500_Hist_path, API_SP500_Current_path)
    df_NASDAQ <- get_hist_index_data(API_NASDAQ_Hist_path, API_NASDAQ_Current_path)
    df_DOW <- get_hist_index_data(API_DOW_Hist_path, API_DOW_Current_path)
    
    df <- list(SP500 = df_SP500, NASDAQ = df_NASDAQ, DOW = df_DOW)
  } else {
    stop("Invalid index provided. Please choose 'SP500', 'NASDAQ', 'DOW', or 'ALL'.")
  }
  
  return(df)
}

# 02 - Get data from www.magicformulainvesting.com  -----------------------------
get_MF_data_df <- function(mktCap_limit_lower_M, mktCap_limit_upper_M, mktCap_step_M){
  
  # 1 - Login and open session ----------------------------------------------
  
  # Ask for username and password
  username <- rstudioapi::askForSecret("Import Magic Formula Data - Enter your username:")
  password <- rstudioapi::askForSecret("Import Magic Formula Data - Enter your password:")
  
  # Create a session
  session <- session("https://www.magicformulainvesting.com/Account/LogOn")
  
  # Submit login form
  form <- html_form(session)[[1]]
  filled_form <- html_form_set(form, "Email" = username, "Password" = password)
  session <- session_submit(session, filled_form)
  
  # Check if login was successful
  response <- session$response
  if (response$status_code == 200 && session$url == "https://www.magicformulainvesting.com/Screening/StockScreening") {
    print("Login successful!")
  } else {
    stop("Login failed. Please check your username and password.")
  }
  
  # Reinitialize form with the session redirected to the screening page
  form <- html_form(session)[[1]]
  
  # Initialize an empty data frame to store the final results
  company_data <- data.frame()
  
  # Initialize progress bar
  pb <- progress_bar$new(format = "[:bar] :percent :elapsed", 
                         total = length(seq(mktCap_limit_lower_M, 
                                            mktCap_limit_upper_M, 
                                            by = mktCap_step_M)
                         )
  )
  
  # 2 - Extract data --------------------------------------------------------
  
  # Loop through market cap thresholds
  for (MinimumMarketCap in seq(mktCap_limit_lower_M, mktCap_limit_upper_M, by = mktCap_step_M)) {
    # # Increment progress bar
    pb$tick()
    
    # Submit form to get top 30
    filled_form_top30 <- html_form_set(form, "Select30" = "true", "MinimumMarketCap" = MinimumMarketCap)
    session <- session_submit(session, filled_form_top30)
    
    # Extract top 30 data
    company_data_top30 <- extract_company_data(session, MinimumMarketCap, "Top30")
    
    # Submit form to get top 50
    filled_form_top50 <- html_form_set(form, "Select30" = "false", "MinimumMarketCap" = MinimumMarketCap)
    session <- session_submit(session, filled_form_top50)
    
    # Extract top 50 data
    company_data_top50 <- extract_company_data(session, MinimumMarketCap, "Top50")
    
    # Merge company_data_top50 into company_data_top30 preserving the records in company_data_top30
    company_data_merged <- merge(company_data_top30, company_data_top50, by = c("Company_Name", "Ticker", "Market_Cap_Millions", "Price_From", "Most_Recent_Quarter_Data", "threshold_mktCap", "TopGreenblatt"), all = TRUE)
    
    company_data_merged_unique <- company_data_merged %>%
      group_by(Company_Name) %>% 
      arrange(TopGreenblatt) %>% 
      distinct(Company_Name, .keep_all = TRUE) %>% 
      ungroup()
    
    # Append to company_data 
    company_data <- rbind(company_data, company_data_merged_unique)
    
    # Pause for 0.1 second to avoid overloading the server
    Sys.sleep(0.1)
  }
  # 
  # Filter duplicate at higher threshold market cap
  company_data <- company_data %>%
    rename(name = Company_Name,
           symbol = Ticker) %>% 
    group_by(name) %>%
    arrange(threshold_mktCap) %>%
    distinct(name, .keep_all = TRUE) %>%
    ungroup()
  
  
  # Checking the final data
  print(company_data)
  
  #  3 - Formatting data -----------------------------------------------------
  
  # Convert Market_Cap_Millions column to numeric
  company_data$Market_Cap_Millions <- as.numeric(company_data$Market_Cap_Millions)
  
  # Convert Date columns to date format
  year <- format(today(), "%Y")
  company_data$Price_From <- as.Date(paste0(year, "-", substr(company_data$Price_From, 1, 2), "-", substr(company_data$Price_From, 4, 5)))
  company_data$Most_Recent_Quarter_Data <- as.Date(paste0(year, "-", substr(company_data$Most_Recent_Quarter_Data, 1, 2), "-", substr(company_data$Most_Recent_Quarter_Data, 4, 5)))
  
  # # Adjust names of the variables
  # prefix = "MF_"
  # colnames(company_data) <- ifelse(names(company_data) == "Ticker", "symbol", paste0(prefix,names(company_data)))
  # 
  return(company_data)
}

## 02.1 - Function to extract company data from session  -----------------------
extract_company_data <- function(session, MinimumMarketCap, TopGreenblatt) {
  # year <- format(last_business_date, "%Y")
  
  company_data <- session %>%
    html_nodes("table.screeningdata tbody tr") %>%
    map_df(~{
      tds <- html_nodes(.x, "td")
      data.frame(
        Company_Name = html_text(tds[1]),
        Ticker = html_text(tds[2]),
        Market_Cap_Millions = gsub(",", "", html_text(tds[3]), fixed = TRUE), # Remove commas
        Price_From = html_text(tds[4]),
        Most_Recent_Quarter_Data = html_text(tds[5]),
        threshold_mktCap = MinimumMarketCap,
        TopGreenblatt = TopGreenblatt,
        stringsAsFactors = FALSE
      )
    })
}

get_quote_data_df <- function(symbols_df, API_Key){
  
  # Create API URLs for various calls to collect full quote
  API_quote_path_base <- 'https://financialmodelingprep.com/api/v3/quote/'
  API_quote_path <- paste0(API_quote_path_base, symbols_df$symbol, '?apikey=', API_Key)
  
  # Progress bar
  total_symbols <- nrow(symbols_df) # Adjust the total to the number of different data
  pb <- progress_bar$new(
    format = "  [:bar] :percent in :elapsed",
    total = total_symbols, 
    width = 60
  )
  
  # Function to retrieve all statements, key metrics, profile and ratios from symbols_df
  fetch_quote_data <- function(paths) {
    bind_rows(lapply(1:length(paths), function(x) {
      pb$tick()
      tryCatch({
        data <- fromJSON(paths[x])
        if (length(data) == 0) {
          NULL
        } else {
          data.frame(data)
        }
      }, error = function(cond) {
        message(paste("API provided an error for", type, "Ticker:", symbols_df$symbol[x]))
        message("Here's the original error message:")
        message(cond)
        return(NULL)
      }, warning = function(cond) {
        message(paste("API provided a warning for", type, "Ticker:", symbols_df$symbol[x]))
        message("Here's the original warning message:")
        message(cond)
        return(NULL)
      })
    }))
  }
  
  quote_df <- fetch_quote_data(API_quote_path) %>% as.data.frame()
  
}

