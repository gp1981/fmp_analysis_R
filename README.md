***Disclaimer**: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.*

# Fundamental Analysis for Value Investors

This repository contains R code to perform fundamental analysis for value investors. It leverages the `dplyr` and `tidyverse` libraries along with `fmpcloudr` to retrieve and analyze financial data from [Financialmodelingprep API (fmp)](https://financialmodelingprep.com/developer/docs/). Also, the code retrieves data from [Greenblatt's magic formula website](https://www.magicformulainvesting.com/) which is used for further analysis.

## Features

-   **Analysis**: Analyze key financials of a company.
-   **Benchmarking**: Compare a company’s financial metrics with other companies
-   **Visualization**: Produce graphs and charts to perform analysis.

## Requirements

-   R (version \>= 4.0.0)
-   RStudio (optional, but recommended)
-   Packages: `dplyr`, `tidyverse`, `fmpcloudr`, `ggplot2`, `shiny` (for interactive visualizations)

## Installation

1.  Set up your API Token on : <https://www.financialmodelingprep.com>

2.  Clone the repository: `bash     git clone https://github.com/gp1981/fmp_analysis.git     cd fmp_analysis`

3.  Run the script on run_analysis.R:

## Usage

1.  **Run the Analysis**: `R     source('scripts/main.R')`

2.  **Select the companies**: Modify the `main.R` script to include the ticker of the company and its competitors.

## Repository Structure

The folder `scripts/` includes the following files:

1.  **main.R**: the main script to perform the analysis

2.  **utils.R**: script of utility functions

3.  **data_retrieval.R**: script to retrieve data from fmp

An up-to-date description of the functions in each file is reported in the project [https://github.com/users/gp1981/projects/14/views/1](fmp_analysis_project). 



## Contributing

Contributions are welcome! Please create a pull request with a detailed description of your changes.

## License

This project is licensed under the MIT License.
