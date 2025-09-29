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

my_state <- "New Jersey"
my_year  <- 2022

race_vars <- c(
  total    = "B03002_001",
  white    = "B03002_003",
  black    = "B03002_004",
  hispanic = "B03002_012"
)

tract_demo <- get_acs(
  geography = "tract",
  state     = my_state,
  county    = c("035","013","023"),
  year      = my_year,
  variables = race_vars,
  output    = "wide"
)

nj_counties <- tidycensus::fips_codes %>%
  filter(state_code == "34") %>%                # NJ = 34
  distinct(county_code, county) %>%
  rename(county_name = county)

tract_demo <- tract_demo %>%
  mutate(county_code = substr(GEOID, 3, 5)) %>% 
  left_join(nj_counties, by = "county_code") %>%
  select(-county_code)

tract_demo <- tract_demo %>%
  mutate(
    pct_white    = if_else(totalE > 0, 100 * whiteE    / totalE, NA_real_),
    pct_black    = if_else(totalE > 0, 100 * blackE    / totalE, NA_real_),
    pct_hispanic = if_else(totalE > 0, 100 * hispanicE / totalE, NA_real_)
  )

top_hispanic_tract <- tract_demo %>%
  filter(!is.na(pct_hispanic)) %>%
  arrange(desc(pct_hispanic)) %>%
  slice_head(n = 1) %>%
  select(NAME, pct_hispanic) %>%
  mutate(`Hispanic %` = sprintf("%.2f%%", pct_hispanic))

knitr::kable(
  top_hispanic_tract,
  caption   = "Tract with Highest Percent Hispanic/Latino (Selected NJ Counties)",
  col.names = c("Tract", "County", "Hispanic %")
)

county_avgs <- tract_demo %>%
  group_by(county_name) %>%
  summarise(
    n_tracts     = n(),
    avg_white    = mean(pct_white,    na.rm = TRUE),
    avg_black    = mean(pct_black,    na.rm = TRUE),
    avg_hispanic = mean(pct_hispanic, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_hispanic))

county_avgs_fmt <- county_avgs %>%
  mutate(
    `Avg White %`    = sprintf("%.2f%%", avg_white),
    `Avg Black %`    = sprintf("%.2f%%", avg_black),
    `Avg Hispanic %` = sprintf("%.2f%%", avg_hispanic)
  ) %>%
  select(county_name, n_tracts, `Avg White %`, `Avg Black %`, `Avg Hispanic %`)

knitr::kable(
  county_avgs_fmt,
  caption   = "Average Tract Demographics by County (Percent of Total Population)",
  col.names = c("County", "# Tracts", "Avg White %", "Avg Black %", "Avg Hispanic %")
)

demographic_moe <- tract_demo %>%
  mutate(
    white_moe_pct    = if_else(whiteE    > 0, 100 * whiteM    / whiteE, NA_real_),
    black_moe_pct    = if_else(blackE    > 0, 100 * blackM    / blackE, NA_real_),
    hispanic_moe_pct = if_else(hispanicE > 0, 100 * hispanicM / hispanicE, NA_real_),
    high_moe_flag    = (white_moe_pct > 15) | (black_moe_pct > 15) | (hispanic_moe_pct > 15)
  )

demographic_moe <- tract_demo %>%
  mutate(
    white_moe_pct    = if_else(whiteE    > 0, 100 * whiteM    / whiteE, NA_real_),
    black_moe_pct    = if_else(blackE    > 0, 100 * blackM    / blackE, NA_real_),
    hispanic_moe_pct = if_else(hispanicE > 0, 100 * hispanicM / hispanicE, NA_real_),

    any_high_moe = pmax(white_moe_pct, black_moe_pct, hispanic_moe_pct, na.rm = TRUE) > 15
  )

overall_moe_summary <- demographic_moe %>%
  summarise(
    total_tracts     = n(),
    tracts_high_moe  = sum(any_high_moe, na.rm = TRUE),
    pct_high_moe     = 100 * tracts_high_moe / total_tracts,
    avg_white_moe    = mean(white_moe_pct,    na.rm = TRUE),
    avg_black_moe    = mean(black_moe_pct,    na.rm = TRUE),
    avg_hispanic_moe = mean(hispanic_moe_pct, na.rm = TRUE)
  )

knitr::kable(
  overall_moe_summary,
  caption = "Overall MOE Summary (Tract-Level)",
  col.names = c("Total Tracts", "Tracts with High MOE (>15%)", "% High MOE",
                "Avg White MOE %", "Avg Black MOE %", "Avg Hispanic MOE %")
)

by_county_moe_summary <- demographic_moe %>%
  group_by(county_name) %>%
  summarise(
    n_tracts         = n(),
    tracts_high_moe  = sum(any_high_moe, na.rm = TRUE),
    pct_high_moe     = 100 * tracts_high_moe / n_tracts,
    avg_white_moe    = mean(white_moe_pct,    na.rm = TRUE),
    avg_black_moe    = mean(black_moe_pct,    na.rm = TRUE),
    avg_hispanic_moe = mean(hispanic_moe_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_high_moe))

knitr::kable(
  by_county_moe_summary,
  caption = "MOE Summary by County (Tract-Level)",
  col.names = c("County", "# Tracts", "# High MOE Tracts", "% High MOE",
                "Avg White MOE %", "Avg Black MOE %", "Avg Hispanic MOE %")
)

pattern_summary <- demographic_moe %>%
  group_by(any_high_moe) %>%  
  summarise(
    n_tracts      = n(),
    avg_population = mean(totalE, na.rm = TRUE),
    avg_white_pct  = mean(pct_white,    na.rm = TRUE),
    avg_black_pct  = mean(pct_black,    na.rm = TRUE),
    avg_hisp_pct   = mean(pct_hispanic, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = if_else(any_high_moe, "High MOE (≥15% in any group)", "Low MOE (<15%)")
  ) %>%
  select(group, n_tracts, avg_population, avg_white_pct, avg_black_pct, avg_hisp_pct)

knitr::kable(
  pattern_summary,
  caption = "Comparison of Tracts by Data Quality (High vs. Low MOE)",
  col.names = c("MOE Group", "Number of Tracts",
                "Avg Population", "Avg White %", "Avg Black %", "Avg Hispanic %"),
  format.args = list(big.mark = ",", digits = 2)
)