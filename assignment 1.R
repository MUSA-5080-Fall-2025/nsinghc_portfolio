---
title: "Assignment 1: Census Data Quality for Policy Decisions"
subtitle: "Evaluating Data Reliability for Algorithmic Decision-Making"
author: "Nina Carlsen"
date: today
format: 
  html:
  code-fold: false
toc: true
toc-location: left
theme: cosmo
execute:
  warning: false
message: false
---

library(tidyverse)
library(tidycensus)
library(knitr)

census_api_key(Sys.getenv("e195d3b46a99d2c4f9250dbc870202be7aaf8a2b"))
install = TRUE

my_state = "New Jersey"

acs_vars_2022 <- load_variables(2022, "acs5", cache = TRUE)

nj_data <- get_acs(
  geography = "county",
  variables = c(
    total_pop = "B01003_001",
    median_income = "B19013_001"
  ),
  state = "NJ",
  year = 2022,
  output = "wide"  
)

head(nj_data)

nj_clean <- nj_data %>%
  mutate(
    county_name = str_remove(NAME, ", New Jersey"),
    county_name = str_remove(county_name, " County")
  )

head(nj_clean)

nj_reliability <- nj_clean %>%
  mutate(
    moe_percentage = round((median_incomeM / median_incomeE) * 100, 2),
    
    reliability = case_when(
      moe_percentage < 5 ~ "High Confidence",
      moe_percentage >= 5 & moe_percentage <= 10 ~ "Moderate",
      moe_percentage > 10 ~ "Low Confidence"
    )
  )

count(nj_reliability, reliability)

library(dplyr)
library(knitr)

nj_reliability %>%
  mutate(
    moe_pct = 100 * median_incomeM / median_incomeE
  ) %>%
  arrange(desc(moe_pct)) %>%
  slice_head(n = 5) %>%
  select(
    county_name,
    median_income = median_incomeE,
    margin_of_error = median_incomeM,
    moe_pct,
    reliability
  ) %>%
  mutate(`MOE %` = paste0(round(moe_pct, 2), "%")) %>%
  select(-moe_pct) %>%        
  knitr::kable(
    caption = "Top 5 NJ Counties with Highest MOE Percentage",
    col.names = c("County", "Median Income", "Margin of Error", "MOE %", "Reliability Category")
  )

selected_counties <- nj_reliability %>%
  filter(county_name %in% c(
    "Somerset",
    "Essex",
    "Middlesex"
  )) %>%
  mutate(moe_pct = 100 * median_incomeM / median_incomeE) %>%
  select(
    county_name,
    median_income = median_incomeE,
    moe_pct,
    reliability
  ) %>%
  mutate(`MOE %` = paste0(round(moe_pct, 2), "%")) %>%
  select(-moe_pct)

knitr::kable(
  selected_counties,
  caption   = "Selected Counties for Tract-Level Study",
  col.names = c("County", "Median Income", "MOE %", "Reliability Category")
)

# Middlesex, where I'm from, has the lowest percentage


  