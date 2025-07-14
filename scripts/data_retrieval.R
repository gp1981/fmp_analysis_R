# Author: gp1981
# Date: 16 July 2024
# Purpose: Retrieve data
# Disclaimer: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.

# 01 - Get data   --------------------------------------------------------------

## 01.1 - Get data from fmp ----------------------------------------
get_stock_data_df <- function(API_Key){
  # Download financial statement symbol lists via API
  Financial_Statement_Symbol_List <- "https://financialmodelingprep.com/stable/financial-statement-symbol-list?apikey="
  Financial_Statement_Symbol_List <- paste0(Financial_Statement_Symbol_List, API_Key)
  Financial_Statement_Symbol_List <- fromJSON(Financial_Statement_Symbol_List)
  Financial_Statement_Symbol_List <- as.data.frame(Financial_Statement_Symbol_List)
  Stock_List_data <- Financial_Statement_Symbol_List 
  rm(Financial_Statement_Symbol_List)
  
  # Rename column names
  names(Stock_List_data)[1] <- "Ticker"
  names(Stock_List_data)[2] <- "CompanyName"
  
  return(Stock_List_data)
}

API_Profile <- function(Stock_List_data, API_Key, countries) {
  
  # Base URL for API calls
  API_Profile_path_base <- 'https://financialmodelingprep.com/stable/profile?symbol='
  
  # Initialize a list to store profile data
  Profile_list <- list()
  
  total_stocks <- length(Stock_List_data$Ticker)
  i <- 1
  
  # Define a function to process each ticker
  process_ticker <- function(ticker) {
    cat("Processing Profile", ticker, "-", round(i / total_stocks * 100, 1), "% complete\n")
    
    # Construct API URL for the current ticker
    API_Profile_path <- paste0(API_Profile_path_base, ticker, '&apikey=', API_Key)
    
    result <- list(
      Profile = NULL
    )
    
    tryCatch({
      # Retrieve Profile
      Stock_Profile_temp <- fromJSON(API_Profile_path)
      if (length(Stock_Profile_temp) > 0) {
        result$Profile <- data.frame(Stock_Profile_temp)
      }
    }, error = function(cond) {
      message(paste("API provided an error for this Ticker:", ticker))
      message("Here's the original error message:")
      message(cond)
    }, warning = function(cond) {
      message(paste("API provided a warning for this Ticker:", ticker))
      message("Here's the original warning message:")
      message(cond)
    })
    
    i <<- i + 1  # Update i in the global environment
    
    return(result)
  }
  
  # Use lapply to process all tickers
  results <- lapply(Stock_List_data$Ticker, process_ticker)
  
  # Flatten the list of dataframes and combine them into one
  flattened_results <- lapply(results, function(x) x$Profile)
  combined_df <- bind_rows(flattened_results)
  
  # Rename column "symbol" to "Ticker" for consistency
  if ("symbol" %in% colnames(combined_df)) {
    combined_df <- combined_df %>% rename(Ticker = symbol)
  }
  
  return(combined_df)
}

get_fundamentals_data_df <- function(Stock_List_data, API_Key, period, period_limit) {
  
  # Create API URLs for various calls to collect Financial Statements
  API_IncomeStatement_path_base <- 'https://financialmodelingprep.com/stable/income-statement?symbol='
  API_BalanceSheet_path_base <- 'https://financialmodelingprep.com/stable/balance-sheet-statement?symbol='
  API_CashFlow_path_base <- 'https://financialmodelingprep.com/stable/cash-flow-statement?symbol='
  API_KeyMetrics_TTM_path_base <- 'https://financialmodelingprep.com/stable/key-metrics-ttm?symbol='
  API_KeyMetrics_path_base <- 'https://financialmodelingprep.com/stable/key-metrics?symbol='
  API_Ratios_TTM_path_base <- 'https://financialmodelingprep.com/stable/ratios-ttm?symbol='
  API_Ratios_path_base <- 'https://financialmodelingprep.com/stable/ratios?symbol='
  API_Shares_Float <- 'https://financialmodelingprep.com/stable/shares-float?symbol='
  API_EV_base <- 'https://financialmodelingprep.com/stable/enterprise-values?symbol='
  
  if (period == "quarter") {
    API_IncomeStatement_path_suffix <- '&period='
    API_BalanceSheet_path_suffix <- '&period='
    API_CashFlow_path_suffix <- '&period='
    API_EV_suffix <- "&period="
  } else {
    API_IncomeStatement_path_suffix <- ''
    API_BalanceSheet_path_suffix <- ''
    API_CashFlow_path_suffix <- ''
    API_EV_suffix <- ''
  }
  
  # Initialize lists to store data
  IS_list <- list()
  BS_list <- list()
  CF_list <- list()
  KeyMetrics_list_TTM <- list()
  KeyMetrics_list <- list()
  Ratios_TTM <- list()
  Ratios <- list()
  Shares_Float <- list()
  EV<-list()
  
  total_stocks <- length(Stock_List_data$Ticker)
  i <- 1
  
  # Define a function to process each ticker
  process_ticker <- function(ticker) {
    cat("Processing", ticker, "-", round(i / total_stocks * 100, 1), "% complete\n")
    
    # Construct API URLs for the current ticker
    API_IncomeStatement_path <- paste0(API_IncomeStatement_path_base, ticker, API_IncomeStatement_path_suffix, period,  '&limit=', period_limit, '&apikey=', API_Key)
    API_BalanceSheet_path <- paste0(API_BalanceSheet_path_base, ticker, API_BalanceSheet_path_suffix, period, '&limit=', period_limit, '&apikey=', API_Key)
    API_CashFlow_path <- paste0(API_CashFlow_path_base, ticker, API_CashFlow_path_suffix, period, '&limit=', period_limit, '&apikey=', API_Key)
    API_KeyMetrics_TTM_path <- paste0(API_KeyMetrics_TTM_path_base, ticker, '&apikey=', API_Key)
    API_KeyMetrics_path <- paste0(API_KeyMetrics_path_base, ticker, '&limit=', period_limit, '&apikey=', API_Key)
    API_Ratios_TTM_path <- paste0(API_Ratios_TTM_path_base, ticker, '&apikey=', API_Key)
    API_Ratios_path <- paste0(API_Ratios_path_base, ticker, '&limit=', period_limit, '&apikey=', API_Key)
    API_Shares_Float_path <- paste0(API_Shares_Float, ticker, '&apikey=', API_Key)
    API_EV_path <- paste0(API_EV_base, ticker, API_EV_suffix, period,'&limit=', period_limit, '&apikey=', API_Key)
    
    
    result <- list(
      IS = NULL,
      BS = NULL,
      CF = NULL,
      KM_TTM = NULL,
      KM = NULL,
      Ratios_TTM = NULL,
      Ratios = NULL,
      Shares_Float = NULL
    )
    
    tryCatch({
      # Retrieve Income Statement
      Stock_IncomeStatement_temp <- fromJSON(API_IncomeStatement_path)
      if (length(Stock_IncomeStatement_temp) > 0) {
        result$IS <- data.frame(Stock_IncomeStatement_temp)
      }
      
      # Retrieve Balance Sheet
      Stock_BalanceSheet_temp <- fromJSON(API_BalanceSheet_path)
      if (length(Stock_BalanceSheet_temp) > 0) {
        result$BS <- data.frame(Stock_BalanceSheet_temp)
      }
      
      # Retrieve Cash Flow Statement
      Stock_CashFlow_temp <- fromJSON(API_CashFlow_path)
      if (length(Stock_CashFlow_temp) > 0) {
        result$CF <- data.frame(Stock_CashFlow_temp)
      }
      
      # Retrieve Key Metrics TTM
      Stock_KeyMetrics_temp_TTM <- fromJSON(API_KeyMetrics_TTM_path)
      if (length(Stock_KeyMetrics_temp_TTM) > 0) {
        result$KM_TTM <- data.frame(Stock_KeyMetrics_temp_TTM)
      }
      
      # Retrieve Key Metrics
      Stock_KeyMetrics_temp <- fromJSON(API_KeyMetrics_path)
      if (length(Stock_KeyMetrics_temp) > 0) {
        result$KM <- data.frame(Stock_KeyMetrics_temp)
      }
      
      # Retrieve Ratios TTM
      Stock_Ratios_temp_TTM <- fromJSON(API_Ratios_TTM_path)
      if (length(Stock_Ratios_temp_TTM) > 0) {
        result$Ratios_TTM <- data.frame(Stock_Ratios_temp_TTM)
      }
      
      # Retrieve Ratios
      Stock_Ratios_temp <- fromJSON(API_Ratios_path)
      if (length(Stock_Ratios_temp) > 0) {
        result$Ratios <- data.frame(Stock_Ratios_temp)
      }
      
      # Retrieve Shares Float
      Shares_Float_temp <- fromJSON(API_Shares_Float_path)
      if (length(Shares_Float_temp) > 0) {
        result$Shares_Float <- data.frame(Shares_Float_temp)
      }
      
      # Retrieve EV
      EV_temp <- fromJSON(API_EV_path)
      if (length(EV_temp) > 0) {
        result$EV <- data.frame(EV_temp)
      }
      
    }, error = function(cond) {
      message(paste("API provided an error for this Ticker:", ticker))  # Use ticker instead of Ticker
      message("Here's the original error message:")
      message(cond)
    }, warning = function(cond) {
      message(paste("API provided a warning for this Ticker:", ticker))  # Use ticker instead of Ticker
      message("Here's the original warning message:")
      message(cond)
    })
    
    i <<- i + 1
    
    return(result)
  }
  
  # Use lapply to process all tickers
  results <- lapply(Stock_List_data$Ticker, process_ticker)
  
  # Combine all dataframes
  IS <- bind_rows(lapply(results, function(x) x$IS))
  BS <- bind_rows(lapply(results, function(x) x$BS))
  CF <- bind_rows(lapply(results, function(x) x$CF))
  
  KeyMetrics_TTM <- bind_rows(lapply(results, function(x) x$KM_TTM))
  KeyMetrics_TTM <- KeyMetrics_TTM %>% rename(marketCap_LocalFX_KM_TTM = marketCap)
  KeyMetrics_TTM <- KeyMetrics_TTM %>% rename(enterpriseValueTTM_LocalFX_KM_TTM = enterpriseValueTTM)
  
  KeyMetrics <- bind_rows(lapply(results, function(x) x$KM)) 
  KeyMetrics <- KeyMetrics %>% rename(marketCap_LocalFX_KM = marketCap)
  KeyMetrics <- KeyMetrics %>% rename(enterpriseValueTTM_LocalFX_KM = enterpriseValue)
  
  Ratios_TTM  <- bind_rows(lapply(results, function(x) x$Ratios_TTM))
  Ratios  <- bind_rows(lapply(results, function(x) x$Ratios))
  Shares_Float  <- bind_rows(lapply(results, function(x) x$Shares_Float))
  
  EV  <- bind_rows(lapply(results, function(x) x$EV))
  EV <- EV %>% rename(marketCapitalization_EV_LocalFX_EV = marketCapitalization)
  EV <- EV %>% rename(enterpriseValue_EV_LocalFX_EV = enterpriseValue)
  
  # Rename column "symbol" to "Ticker" for consistency
  if ("symbol" %in% colnames(IS)) {
    IS <- IS %>% rename(Ticker = symbol)
  }
  
  if ("symbol" %in% colnames(BS)) {
    BS <- BS %>% rename(Ticker = symbol)
  }
  
  if ("symbol" %in% colnames(CF)) {
    CF <- CF %>% rename(Ticker = symbol)
  }
  
  if ("symbol" %in% colnames(KeyMetrics_TTM)) {
    KeyMetrics_TTM <- KeyMetrics_TTM %>% rename(Ticker = symbol)
  } 
  
  if ("symbol" %in% colnames(KeyMetrics)) {
    KeyMetrics <- KeyMetrics %>% rename(Ticker = symbol)
  }
  
  if ("symbol" %in% colnames(Ratios_TTM)) {
    Ratios_TTM <- Ratios_TTM %>% rename(Ticker = symbol)
  }
  
  if ("symbol" %in% colnames(Ratios)) {
    Ratios <- Ratios %>% rename(Ticker = symbol)
  }
  
  if ("symbol" %in% colnames(Shares_Float)) {
    Shares_Float <- Shares_Float %>% rename(Ticker = symbol)
  }
  
  if ("symbol" %in% colnames(EV)) {
    EV <- EV %>% rename(Ticker = symbol)
  }
  
  FinancialsMetricsProfile <- list(
    IncomeStatement = IS,
    BalanceSheet = BS,
    CashFlow = CF,
    KeyMetrics_TTM = KeyMetrics_TTM,
    KeyMetrics = KeyMetrics,
    Ratios_TTM = Ratios_TTM,
    Ratios = Ratios,
    Shares_Float = Shares_Float,
    EV = EV,
    Stock_List_data = Stock_List_data
  )
  
  return(FinancialsMetricsProfile)
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
    rename(name = Company_Name) %>% 
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

get_quote_data_df <- function(Stock_List_data, API_Key){
  
  # Create API URLs for various calls to collect full quote
  API_quote_path_base <- 'https://financialmodelingprep.com/stable/aftermarket-trade?symbol='
  API_quote_path <- paste0(API_quote_path_base, Stock_List_data$Ticker, '&apikey=', API_Key)
  
  # Progress bar
  total_symbols <- nrow(Stock_List_data) # Adjust the total to the number of different data
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
  quote_df <- quote_df %>%
    mutate(
      timestamp = as.Date(format(as.POSIXct(timestamp / 1000, origin = "1970-01-01", 
                                            tz = "UTC"), "%Y-%m-%d"))
    )
}

# 04 - Get FX data  -----------------------------
API_FX_rate <- function(API_Key) {
  
  # Base URLs
  FX_list_url_path_base <- 'https://financialmodelingprep.com/stable/forex-list?apikey='
  FX_base <- 'https://financialmodelingprep.com/stable/quote-short?symbol='
  
  # Retrieve List of currencies & FX symbols
  FX_list_url <- paste0(FX_list_url_path_base,API_Key)
  FX_list_df <- fromJSON(FX_list_url)
  
  # USD Converted Currencies
  FX_list_df_USD <- FX_list_df %>% filter(toCurrency =="USD")
  total_cur <- length(FX_list_df_USD$symbol)
  
  
  # Fetch FX rates
  fx_rates <- list()
  for (i in seq_along(FX_list_df_USD$symbol)) {
    cur <- FX_list_df_USD$symbol[i]
    cat("Processing FX", cur, "-", round(i / total_cur * 100, 1), "% complete\n")
    fx_url <- paste0(FX_base, cur,'&apikey=', API_Key)
    
    tryCatch({
      fx_data <- fromJSON(fx_url)
      if (length(fx_data) > 0) {
        fx_rates[[cur]] <- as.numeric(fx_data$price)
      } else {
        fx_rates[[cur]] <- NA
      }
    }, error = function(e) {
      message(paste("Error fetching FX for", cur, ":", e$message))
      fx_rates[[cur]] <- NA
    })
    
  }
  
  # Create FX_rates dataframe
  fx_rates_df <- tibble(
    fx_symbol = names(fx_rates),
    value = unlist(fx_rates)
  )
  
  # Add 1.0 for USD
  fx_rates_df <- fx_rates_df %>% 
    add_row(fx_symbol = "USDUSD", value = 1.0)
  
  return(fx_rates_df)
}
