#### Preamble ####
# Purpose: Test data from The Real Effects of Monetary Expansions: Evidence from a Large-scale Historical Experiment
# Author: Rae Zhang / Jenny Shen / Sarah Zhang
# Data: 11 February 2023 [...UPDATE THIS...]
# Contact: junwei.zhang@mail.utoronto.ca 
# License: MIT
# Pre-requisites: Have access to the paper 
# Any other information needed? N/A



#### Workspace setup ####
# Loading in datasets
library("tidyverse")
library("haven")

datadescriptive <- read_dta("inputs/data/datadescriptive.dta")
liquidity <- read_dta("inputs/data/liquidity.dta")

# Test datasets

# Test numbers of rows in datadescriptive.dta
nrow(datadescriptive) == 324

# Test numbers of countires in liquidity.dta
liquidity$country |>
  unique() == c(
    "England/GB",
    "Italy",
    "Holland",
    "Portugal",
    "Spain",
    "Germany"
  )

# Test min and max years in liquidity.dta
liquidity$year |> 
  min() == 1531

liquidity$year |> 
  max() == 1790