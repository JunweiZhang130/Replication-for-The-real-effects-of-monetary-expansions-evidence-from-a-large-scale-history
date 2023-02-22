#### Preamble ####
# Purpose: Downloads and saves the data from The Real Effects of Monetary Expansions: Evidence from a Large-scale Historical Experiment
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
library("ggplot2")

datadescriptive <- read_dta("inputs/data/datadescriptive.dta")
liquidity <- read_dta("inputs/data/liquidity.dta")
write_dta(datadescriptive,"scripts/datadescriptive.dta")
write_dta(liquidity,"scripts/liquidity.dta")
liquidity <- read_dta(here::here("scripts/liquidity.dta"))
datadescriptive <- read_dta(here::here("scripts/datadescriptive.dta"))

# Replication of Figure 4
color = c("#7F3C8D", 
           "#11A579", 
           "#3969AC", 
           "#F2B701", 
           "#E73F74", 
           "#80BA5A")

NominalGDP <- liquidity |>
  ggplot(aes(x = year, 
             y = Nominal_GDPindex1700100, 
             color = country)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = colors) +
  scale_y_continuous(limits = c(0, 300), 
                     expand = c(0, 0)) +
  labs(x = "Year", 
       y = "Nominal GDP, index(1700=100)", 
       title = "Nominal GDP for 6 Countries") +
  theme_minimal() +
  facet_wrap(~ country, ncol = 2)

NominalGDP

# Replication of Figure 6
RealGDP <- liquidity |>
  ggplot(aes(x = year, 
             y = realGDPindex1700100, 
             color = country)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = colors) +
  scale_y_continuous(limits = c(0, 300), 
                     expand = c(0, 0)) +
  
  labs(x = "Year", 
       y = "Real GDP, index(1700=100)", 
       title = "Real GDP for 6 Countries") +
  theme_minimal() +
  facet_wrap(~ country, ncol = 2)

RealGDP

# Replication of Figure 7
# Set color palette
mypal <- c("#1b9e77", "#d95f02")

TotalGoldShare <- datadescriptive |>
  ggplot(aes(x = year, y = goldshare, fill = "Total Gold Share")) +
  geom_bar(stat = "identity") +
  geom_line(aes(x = year, y = goldshare, color = "Total Gold Share"), size = 1.5) +
  labs(x = "Year", 
       y = "Total Gold Share", 
       title = "Gold share") +
  scale_x_continuous(limits = c(1530, 1790), breaks = seq(1530, 1790, by = 10)) +
  scale_y_continuous(limits = c(0, 100))+
  scale_fill_manual(values = mypal, guide = FALSE) +
  scale_color_manual(values = mypal, guide = FALSE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
TotalGoldShare

# Replication of Figure 12
EnglandMintOutput <- datadescriptive |>
  ggplot(aes(x = year, y = englandmintmillions)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", 
       y = "England Mint Output (in millions)", 
       title = "Simulated England Mint Output from 1531 to 1790") +
  scale_x_continuous(limits = c(1531, 1790))
EnglandMintOutput