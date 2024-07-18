***Disclaimer**: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.*

# Fundamental Analysis for Value Investors

This repository contains R code to perform fundamental analysis for value investors. It leverages the `dplyr` and `tidyverse` libraries along with `fmpcloudr` to retrieve and analyze financial data from [Financialmodelingprep API (fmp)](https://financialmodelingprep.com/developer/docs/).

## Features

- **Analysis**: Analyze key financials of a company.
- **Benchmarking**: Compare a company’s financial metrics with other companies
- **Visualization**: Produce graphs and charts to perform analysis.


## Requirements

- R (version >= 4.0.0)
- RStudio (optional, but recommended)
- Packages: `dplyr`, `tidyverse`, `fmpcloudr`, `ggplot2`, `shiny` (for interactive visualizations)

## Installation

1. Set up your API Token on : https://www.financialmodelingprep.com


2. Clone the repository:
    ```bash
    git clone https://github.com/gp1981/fmp_analysis.git
    cd fmp_analysis
    ```
    
3. Run the script on run_analysis.R:

## Usage

1. **Run the Analysis**:
    ```R
    source('scripts/main.R')
    ```

2. **Select the companies**:
    Modify the `main.R` script to include the ticker of the company and its competitors.

## Repository Structure

The folder `scripts/` includes the following files:

1. **main.R**: the main script to perform the analysis

2. **utils.R**: script of utility functions

3. **data_retrieval.R**: script to retrieve data from fmp

An up-to-date description of the functions in each file is reported in the table below. 
*Note*: the suffix of the function name indicates the type of output e.g. _df (dataframe), _list (list)

| file             | Function     | Status    | Description            |
|:-----------------|:-------------|:----------|:-----------------------|
| data_retrieval.R | `get_stock_data_df` | Completed | Retrieve stock list data from fmp |
| data_retrieval.R | `get_fundamentals_data_df` | Completed | Retrieve fundamentals data from fmp |
| data_retrieval.R | `get_financial_statements_as_reported_list` | Completed | Retrieve financial statements as reported |
| data_retrieval.R | `get_price_history_data_df` | Completed | Retrieve the historical price data |
| data_retrieval.R | `get_quote_data_df` | Completed | Retrieve the full quote data |
| data_retrieval.R | `history_total_equity_book_value_df` | To Start | Calculate the Total Equity Book Value (incl. past dividends, treasury, net issuance of shares, etc.) (see issue #1)|
| utils.R | `export_excel_data` | Completed | Export dataframe into a table in excel |
| utils.R | `ensure_consistent_types` | Completed | Ensure columns of a dataframes nested in a list |     
| utils.R | `search_and_retrieve_columns` | Completed | Search specific words in the heading of the dataframes nested in a list |
| utils.R | `extract_specific_variables` | Completed | Extract specific variables from each dataframe in a list | 

## Contributing

Contributions are welcome! Please create a pull request with a detailed description of your changes.

## License

This project is licensed under the MIT License.
