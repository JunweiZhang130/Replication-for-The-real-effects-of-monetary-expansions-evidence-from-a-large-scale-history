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
# Generate nominal GDP values
simulated_data <- simulated_data %>%
  mutate(nominal_gdp = rlnorm(n(), meanlog = 6.8, sdlog = 0.55)) %>%
  group_by(Countries) %>%
  mutate(nominal_gdp = nominal_gdp / (nominal_gdp[Years == 1700] / 100)) %>%
  ungroup()

# Create a list of colors for the countries
colors <- c("#7F3C8D", 
            "#11A579", 
            "#3969AC", 
            "#F2B701", 
            "#E73F74", 
            "#80BA5A")

# Plot the data
ggplot(simulated_data, aes(x = Years, 
                           y = nominal_gdp, 
                           color = Countries)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = colors) +
  scale_y_continuous(limits = c(0, 300), 
                     expand = c(0, 0)) +

  labs(x = "Year", 
       y = "Nominal GDP", 
       title = "Nominal GDP for 6 Countries") +
  theme_minimal() +
  facet_wrap(~ Countries, ncol = 2)

#### Simulate data of Figure 6 ####
set.seed(858)

# create a data frame with all combinations of years and countries
years <- 1531:1790
countries <- c("England/GB", 
               "Germany", 
               "Holland", 
               "Italy", 
               "Portugal", 
               "Spain"
)

simulated_real_gdp <- 
  expand_grid(Years = years, Countries = countries)

# group by year and sample one country per group
simulated_real_gdp <- 
  simulated_real_gdp |>
  group_by(Years) |>
  sample_n(size = 6) |>
  ungroup() |>
  mutate(nominal_gdp = round(runif(n(), 
                                   min = 1.662095, 
                                   max = 5.343219), 
                             digits = 6))

head(simulated_real_gdp)

# simulate the graphs
library(ggplot2)

# plot
# Generate nominal GDP values
simulated_real_gdp <- simulated_real_gdp %>%
  mutate(real_gdp = rlnorm(n(), meanlog = 6.8, sdlog = 0.55)) %>%
  group_by(Countries) %>%
  mutate(real_gdp = real_gdp / (real_gdp[Years == 1700] / 100)) %>%
  ungroup()

# Create a list of colors for the countries
colors <- c("#7F3C8D", 
            "#11A579", 
            "#3969AC", 
            "#F2B701", 
            "#E73F74", 
            "#80BA5A")

# Plot the data
ggplot(simulated_real_gdp, aes(x = Years, 
                           y = real_gdp, 
                           color = Countries)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = colors) +
  scale_y_continuous(limits = c(0, 300), 
                     expand = c(0, 0)) +
  
  labs(x = "Year", 
       y = "Real GDP", 
       title = "Real GDP for 6 Countries") +
  theme_minimal() +
  facet_wrap(~ Countries, ncol = 2)

#### Workspace setup ####
library(tidyverse)
library(haven)
library(AER)

datadescriptive <- read_dta("inputs/data/datadescriptive.dta")
head(datadescriptive)

liquidity <- read_dta("inputs/data/liquidity.dta")
head(liquidity)
#### Simulate data of Figure 13 ####
set.seed(853)
years <- 1559:1700
in_mint_adj <- runif(length(years), -5.3, -3.0)
metal_prod <- runif(length(years), -5.3, -3.0)

simulated_england_first_stage_variables <- 
  expand_grid(Years = years, 
              English_Mint = in_mint_adj, 
              Precious_Metals = metal_prod) |>
  group_by(Years) |>
  sample_n(size = 2) |>
  ungroup() |>
  mutate(English_Mint = round(runif(n(), 
                                    min = -8, 
                                    max = -2.0), 
                              digits = 6)) %>%
  mutate(Precious_Metals = round(runif(n(), 
                                       min = -5.3, 
                                       max = -3.0), 
                                 digits = 6))

head(simulated_england_first_stage_variables)

# Plot the results
ggplot(simulated_england_first_stage_variables, aes(x = Years)) +
  geom_line(aes(y = English_Mint), color = "blue") +
  scale_y_continuous(name = "English Mint Output", 
                     limits = c(-5, -3)) +
  geom_line(aes(y = Precious_Metals), 
            color = "red", 
            linetype = "dashed") +
  scale_y_continuous(name = "Precious Metals", 
                     sec.axis = sec_axis(~., name = "Precious Metals", 
                                         breaks = seq(-8, -2, by = 1), 
                                         labels = seq(-8, -2, by = 1))) +
  labs(title = "Simulated England First-Stage Variables")