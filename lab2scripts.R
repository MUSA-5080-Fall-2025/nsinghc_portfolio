---
  title: "Algorithmic Decision Making & Census Data"
subtitle: "Week 2: MUSA 5080"
author: "Dr. Elizabeth Delmelle"
date: "September 15, 2025"
format: 
  revealjs:
  theme: simple
slide-number: true
scrollable: true
chalkboard: true
code-line-numbers: true
incremental: false
---
  
  # Today's Agenda
  
  ## What We'll Cover
  
  **Part 1: Algorithmic Decision Making** 
  
  - What are algorithms in public policy?
  - When algorithmic decision making goes wrong
- Current policy responses

**Part 2: Active Learning**  
  
  - Small group scenarios: designing ethical algorithms
- Discussion and reflection

**Part 3: Census Data Foundations** 
  
  - Understanding census data for policy analysis
- Geography and data availability

**Part 4: Hands-On Census Data with R** 
  
  - Live demonstration of key functions
- Practice exercises

---
  
  # Part 1: Algorithmic Decision Making
  
  ## Opening Question
  
  **Discuss with your table (1 minutes):**
  
  What is an algorithm?
  
  *Think beyond just computer code - how do you make decisions in daily life?*
  
  ## What Is An Algorithm?
  
  **Definition:** A set of rules or instructions for solving a problem or completing a task

**Examples:**
  
  - Recipe for cooking
- Directions to get somewhere  
- Decision tree for hiring

- **Computer program that processes data to make predictions**
  
  ## Algorithmic Decision Making in Government
  
  **Systems used to assist or replace human decision-makers**
  
  Based on predictions from models that process historical data containing:
  
  - **Inputs** ("features", "predictors", "independent variables", "x")
- **Outputs** ("labels", "outcome", "dependent variable", "y")

## Real-World Examples

:::: {.columns}

::: {.column width="30%"}

**Criminal Justice**
  Recidivism risk scores for bail and sentencing decisions
:::
  
  ::: {.column width="30%"}

**Housing & Finance**  
  Mortgage lending and tenant screening algorithms
:::
  
  ::: {.column width="30%"}

**Healthcare**
  Patient care prioritization and resource allocation
:::
  
  ::::
  
  ## Clarifying Key Terms
  
  **Data Science** → Computer science/engineering focus on algorithms and methods

**Data Analytics** → Application of data science methods to other disciplines  

**Machine Learning** → Algorithms for classification & prediction that learn from data

**AI** → Algorithms that adjust and improve across iterations (neural networks, etc.)

## Public Sector Context

**Long history of government data collection:**
  
  - Civic registration systems  
- Census data
- Administrative records
- Operations research (post-WWII)

**What's new?**

- More data (official and "accidental")
- Focus on **prediction** rather than explanation
- **Harder to interpret** and explain

## Why Government Uses Algorithms

**Governments have limited budgets and need to serve everyone**

Algorithmic decision making is especially appealing because it promises:

- **Efficiency** - process more cases faster
- **Consistency** - same rules applied to everyone  
- **Objectivity** - removes human bias
- **Cost savings** - fewer staff needed

**But does it deliver on these promises?**

---

# When Algorithms Go Wrong

## Remember: Data Analytics Is Subjective

**Every step involves human choices:**

- Data cleaning decisions
- Data coding or classification  
- Data collection - use of imperfect proxies
- How you interpret results
- What variables you put in the model

**These choices embed human values and biases**

## {.smaller}
**Healthcare Algorithm Bias**

**The Problem:**

Algorithm used to identify high-risk patients for additional care **systematically discriminated against Black patients**

**What Went Wrong:**

- Algorithm used **healthcare costs as a proxy for need**
- Black patients typically incur lower costs due to **systemic inequities in access**  
- Result: Black patients under-prioritized despite **equivalent levels of illness**

**Scale:** Used by hospitals and insurers for **over 200 million people** annually

*Source: Obermeyer et al. (2019), Science*

## Criminal Justice Algorithm Bias  

**COMPAS Recidivism Prediction:**

**The Problem:**

- Algorithm **2x as likely** to falsely flag Black defendants as high risk
- White defendants often rated low risk **even when they do reoffend**  

**Why This Happens:**

- Historical arrest data reflects **biased policing patterns**
- Socioeconomic proxies correlate with race

- **"Objective" data contains subjective human decisions**

*Source: ProPublica investigation*

## Dutch Welfare Fraud Detection

**The Problem:**

- **"Black box" system** operated in secrecy
- Impossible for individuals to understand or challenge decisions
- **Disproportionately targeted** vulnerable populations

**Court Ruling:**

- Breached privacy rights under European Convention on Human Rights
- Highlighted **unfair profiling and discrimination**
- System eventually **shut down**


# Part 2: Active Learning Exercise

## Small Group Challenge (10 Minutes! We keep a tight ship around here.)

At your table, pick **one** scenario and answer **three** prompts.

**Prompts (plain English, no tech):**

1. **Proxy:** What would you *use* to stand in for what you *want*?
2. **Blind spot:** What data gap or historical bias could skew results?
3. **Harm + Guardrail:** Who could be harmed, and one simple safeguard?


## Pick one scenario

1) **Emergency response prioritization** (natural disasters)  
2) **School enrollment assignment**  
3) **Automated traffic enforcement** (red-light cameras)  
4) **Housing assistance allocation**  
5) **Predictive policing**

## Example (so you see the level)

**Scenario:** Emergency response  

- **Proxy:** 911 call volume → stand-in for “need”  
- **Blind spot:** Under-calling where trust/connectivity is low  
- **Harm + Guardrail:** Wealthier areas over-prioritized → add a **vulnerability boost** (age/disability) and a **minimum-service floor** per zone

## Discuss at your table (8 minutes)

**Answer these out loud and on one device or notepad:**

- **Proxy →** “We’d use ____ as a stand-in for ____.”
- **Blind spot →** “This could miss/undercount ____ because ____.”
- **Harm + Guardrail →** “Group ____ could be hurt by ____. We’d add ____ (one safeguard).”

**Choose ONE guardrail type:**

- Prioritize vulnerable groups  
- Cap disparities across areas (simple rule)  
- Human review + appeals for edge cases  
- Replace a bad proxy (collect the right thing)  
- Publish criteria & run a periodic bias check

## Lightning shares (2–3 tables)

In **≤20 seconds**, say:

- **Scenario**, **one proxy**, **one harm**, **one guardrail**

Class quick poll: *Would that guardrail help?* 

- 👍 Green light 
- 👎 Red light


---

# Part 3: Census Data Foundations

## Why Census Data Matters

**Census data is the foundation for:**

- Understanding community demographics
- Allocating government resources  
- Tracking neighborhood change

- **Designing fair algorithms** (like those we just discussed)

**Connection:** The same demographic data used in census goes into many of the algorithms we analyzed

## Census vs. American Community Survey

:::: {.columns}

::: {.column width="50%"}
**Decennial Census (2020)**

- **Everyone** counted every 10 years
- **9 basic questions:** age, race, sex, housing
- **Constitutional requirement**
- Determines political representation
:::

::: {.column width="50%"}  
**American Community Survey (ACS)**

- **3% of households** surveyed annually
- **Detailed questions:** income, education, employment, housing costs
- **Replaced** the old "long form" in 2005
- **A big source of data we'll use** this semester
:::
  
  ::::
  
  ## ACS Estimates: What You Need to Know
  
  **1-Year Estimates** (areas > 65,000 people)

- Most current data, smallest sample

**5-Year Estimates** (all areas including census tracts)  

- Most reliable data, largest sample
- **What you'll use most often**

**Key Point:** All ACS data comes with **margins of error** - we'll learn to work with uncertainty

## Census Geography Hierarchy

```
Nation
├── Regions  
├── States
│   ├── Counties
│   │   ├── Census Tracts (1,500-8,000 people)
│   │   │   ├── Block Groups (600-3,000 people)  
│   │   │   │   └── Blocks (≈85 people, Decennial only)
```

**Most policy analysis happens at:**
  
  - **County level** - state and regional planning
- **Census tract level** - neighborhood analysis 
- **Block group level** - very local analysis (tempting, but big MOEs)

## 2020 Census Innovation: Differential Privacy

**The Challenge:** Modern computing can "re-identify" individuals from census data

**The Solution:** Add mathematical "noise" to protect privacy while preserving overall patterns

**The Controversy:** Some places now show populations living "underwater" or other impossible results

**Why This Matters:** Even "objective" data involves **subjective choices** about privacy vs. accuracy. Also, the errors.

## Accessing Census Data in R

**Traditional approach:** Download CSV files from Census website

**Modern approach:** Use R packages to access data directly

**Benefits of programmatic access:**
  
  - Always get latest data
- Reproducible workflows  
- Automatic geographic boundaries
- Built-in error handling

**We'll use the `tidycensus` package starting in Lab 1**

## Understanding ACS Data Structure

**Data organized in tables:**

- **B19013** - Median Household Income
- **B25003** - Housing Tenure (Own/Rent)  
- **B15003** - Educational Attainment
- **B08301** - Commuting to Work

**Each table has multiple variables:**

- B19013_001E = Median household income (estimate)
- B19013_001M = Median household income (margin of error)

**You'll learn to find the right variables for your research questions**
  
  ## Working with Margins of Error
  
  **Every ACS estimate comes with uncertainty**
  
  **Rule of thumb:**
  
  - Large MOE relative to estimate = **less reliable**
  - Small MOE relative to estimate = **more reliable**
  
  **In your analysis:**
  
  - Always report MOE alongside estimates
- Be cautious comparing estimates with overlapping error margins
- Consider using 5-year estimates for greater reliability

## Two Types of Census Data

:::: {.columns}

::: {.column width="50%"}
**Summary Tables** (what we'll use mostly)

- Pre-calculated statistics by geography
- Median income, percent college-educated, etc.
- **Good for:** Mapping, comparing places
:::

::: {.column width="50%"}  
**PUMS - Individual Records** 

- Anonymous individual/household responses
- **Good for:** Custom analysis, regression models
- More complex but more flexible
:::

::::

## When New Data Comes Out

**ACS 1-year estimates:** Released in September (previous year's data)

**ACS 5-year estimates:** Released in December  

**Decennial Census:** Released on rolling schedule over 2-3 years

**For Lab 1:** We'll use 2018-2022 ACS 5-year estimates (latest available)

## Data Sources You'll Use

**TIGER/Line Files** 
  
  - Geographic boundaries (shapefiles)
- Census tracts, counties, states
- Now released as shapefiles (easier to use!)

**Historical Data Sources:**
  
  - NHGIS (nhgis.org) - Historical census data
- Neighborhood Change Database 
- Longitudinal Tract Database - Track changes over time

---
  
  # Part 4: Hands-On Census Data with R
  
  ## Live Demo Setup
  
  Let's see tidycensus in action with some basic examples:

```{r demo-setup}
library(tidycensus)
library(tidyverse)
library(knitr)
# Set API key (you'll get yours for Lab 1)
census_api_key("42bf8a20a3df1def380f330cf7edad0dd5842ce6")
```

**Follow along:** We'll work through examples together, then you'll practice in Lab 1

## Basic get_acs() Function

**Most important function you'll use:**

```{r demo-get-acs}
# Get state-level population data
state_pop <- get_acs(
  geography = "state",
  variables = "B01003_001",  # Total population
  year = 2022,
  survey = "acs5"
)

glimpse(state_pop)
```

**Key parameters:** geography, variables, year, survey

## Understanding the Output

**Every ACS result includes:**

- `GEOID` - Geographic identifier
- `NAME` - Human-readable location name  
- `variable` - Census variable code
- `estimate` - The actual value
- `moe` - Margin of error

**This is the foundation for all your analysis**

## Working with Multiple Variables

```{r demo-multiple-vars}
# Get income and population for Pennsylvania counties
pa_data <- get_acs(
  geography = "county",
  variables = c(
    total_pop = "B01003_001",
    median_income = "B19013_001"
  ),
  state = "PA",
  year = 2022,
  output = "wide"  # Makes analysis easier
)

head(pa_data)
```

**Note:** `output = "wide"` gives you one row per place, multiple columns for variables

## Data Cleaning Essentials

**Clean up messy geographic names:**

```{r demo-cleaning}
pa_clean <- pa_data %>%
  mutate(
    # Remove state name from county names
    county_name = str_remove(NAME, ", Pennsylvania"),
    # Remove "County" word
    county_name = str_remove(county_name, " County")
  )

# Compare before and after
select(pa_clean, NAME, county_name)
```

**Functions you'll use:** `str_remove()`, `str_extract()`, `str_replace()`

## Calculating Data Reliability

**This is crucial for policy work:**
  
  ```{r demo-moe}
pa_reliability <- pa_clean %>%
  mutate(
    # Calculate MOE as percentage of estimate
    moe_percentage = round((median_incomeM / median_incomeE) * 100, 2),
    
    # Create reliability categories
    reliability = case_when(
      moe_percentage < 5 ~ "High Confidence",
      moe_percentage >= 5 & moe_percentage <= 10 ~ "Moderate",
      moe_percentage > 10 ~ "Low Confidence"
    )
  )

count(pa_reliability, reliability)
```

**Key functions:** `case_when()` for categories, MOE calculations

## Finding Patterns with dplyr

- Find counties with highest uncertainty
- Summarize by reliability category

```{r demo-analysis}
# Find counties with highest uncertainty
high_uncertainty <- pa_reliability %>%
  filter(moe_percentage > 8) %>%
  arrange(desc(moe_percentage)) %>%
  select(county_name, median_incomeE, moe_percentage)

# Summary by reliability category  
reliability_summary <- pa_reliability %>%
  group_by(reliability) %>%
  summarize(
    counties = n(),
    avg_income = round(mean(median_incomeE, na.rm = TRUE), 0)
  )
```

## Professional Tables

**Making results presentation-ready:**
  
  ```{r demo-tables}
# Create formatted table
kable(high_uncertainty,
      col.names = c("County", "Median Income", "MOE %"),
      caption = "Counties with Highest Income Data Uncertainty",
      format.args = list(big.mark = ","))
```

**Key points:**
  
  - Use `kable()` for professional formatting
- Add descriptive column names and captions  
- Format numbers appropriately

## Quick Practice

**Try this with a neighbor (5 minutes):**
  
  Using the `pa_reliability` data we just created:
  
  1. **Filter** for counties with "High Confidence" data
2. **Arrange** by median income (highest first)  
3. **Select** county name and median income
4. **Slice** the top 3 counties

**We'll share answers before moving on**

## Policy Connection

**Why this matters:**

- **Algorithmic fairness:** Unreliable data can bias automated decisions
- **Resource allocation:** Know which areas need extra attention
- **Equity analysis:** Some communities may be systematically under-counted
- **Professional credibility:** Always assess your data quality

**This connects directly to our algorithmic bias discussion**

---

# Connecting the Dots

## From Algorithms to Analysis

**Today's key connections:**
  
  **Algorithmic Decision Making** → Understanding why your analysis matters for real policy decisions

**Data Subjectivity** → Why we emphasize **transparent, reproducible methods** in this class

**Census Data** → The foundation for most urban planning and policy analysis

**R Skills** → The tools to do this work professionally and ethically

## Questions for Reflection

**As you work with data this semester, ask:**
  
  1. **What assumptions am I making** in my data choices?
  2. **Who might be excluded** from my analysis?
  3. **How could my findings be misused** if taken out of context?
  4. **What would I want policymakers to understand** about my methods?
  
  These questions will make you a **more thoughtful analyst** and **better future policymaker**
  
  ---
  
  # Next Steps
  
  ## Before Next Class
  
  1. **Complete Lab 0** if you haven't finished
2. **Post your weekly notes** - reflect on today's discussion
3. **Start Lab 1** - census data exploration (begins today!)


## Questions?
