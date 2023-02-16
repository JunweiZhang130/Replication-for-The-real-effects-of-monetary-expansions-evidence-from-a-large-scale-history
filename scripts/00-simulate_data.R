#### Preamble ####
# Purpose: Simulates... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Data: 11 February 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]

#### Workspace setup ####
library(tidyverse)
library(haven)

datadescriptive <- read_dta("inputs/data/datadescriptive.dta")
head(datadescriptive)

liquidity <- read_dta("inputs/data/liquidity.dta")
head(liquidity)

#### Simulate data of Figure 4 ####
set.seed(853)

# create a data frame with all combinations of years and countries
years <- 1531:1790
countries <- c("England/GB", 
               "Germany", 
               "Holland", 
               "Italy", 
               "Portugal", 
               "Spain"
               )

simulated_data <- 
  expand_grid(Years = years, Countries = countries)

# group by year and sample one country per group
simulated_data <- 
  simulated_data |>
  group_by(Years) |>
  sample_n(size = 6) |>
  ungroup() |>
  mutate(nominal_gdp = round(runif(n(), 
                                      min = 1.662095, 
                                      max = 5.343219), 
                                digits = 6))

head(simulated_data)

# simulate the graphs
library(ggplot2)

# plot
ggplot(simulated_data, aes(x = Years, y = nominal_gdp, color = Countries)) +
  geom_line() +
  labs(x = "Year", 
       y = "Nominal GDP", 
       title = "Nominal GDP for 6 Countries") +
  theme_minimal() +
  facet_wrap(~ Countries, ncol = 2)
