# IMPORT CLEAN DATA ----

# Goal
# Import data to build regional economic dashboard of

# Set up

# Core
library(tidyverse)
library(tidyquant)

# Time series
library(timetk)

# Interactive visualization
library(plotly)


# Import data ----

## * US recession dates ----
Recession_Dates_tbl    <- read_rds("00_data/wrangled_data//Recession_Dates_tbl.rds")

## * Nonfarm Payroll ----
# monthly, not seasonally adjusted
jobs_nh_nsa_tbl <- tq_get("NHNAN", get = "economic.data", from = "1939-01-01")

# monthly, seasonally adjusted
jobs_nh_sa_tbl  <- tq_get("NHNA", get = "economic.data", from = "1990-01-01")


## * Building Permits ----
# monthly, not seasonally adjusted, all structures
permits_nh_all_nsa_tbl <- tq_get("NHBPPRIV", get = "economic.data", from = "1988-01-01")

# monthly, seasonally adjusted, all structures
permits_nh_all_sa_tbl  <- tq_get("NHBPPRIVSA", get = "economic.data", from = "1988-01-01")

# monthly, not seasonally adjusted, 1-unit structures
permits_nh_1unit_nsa_tbl <- tq_get("NHBP1FH", get = "economic.data", from = "1988-01-01")

# monthly, seasonally adjusted, 1-unit structures
permits_nh_1unit_sa_tbl  <- tq_get("NHBP1FHSA", get = "economic.data", from = "1988-01-01")


indicators_tbl <- bind_rows(jobs_nh_nsa_tbl,
                            permits_nh_1unit_nsa_tbl) %>%

    # recode
    mutate(indicator = symbol %>%
               as_factor() %>%
               fct_recode("Jobs" = "NHNAN",
                          "Permits_1unit" = "NHBP1FH")) %>%

    # Calculate year-over-year change
    group_by(indicator) %>%
    mutate(price = (price/lag(price, n = 12)-1)*100) %>%
    ungroup() %>%

    select(indicator, date, value = price) %>%

    # Remove data before 1989
    filter(date >= "1969-01-01")


# Make plot ----
# Necessary to put variables into the facet labels
indicator_desc <- as_labeller(
    c(`Jobs` = "All Employees: Total Nonfarm in New Hampshire",
      `Permits_1unit` = "New Private Housing Units Authorized by Building Permits: 1-Unit Structures for New Hampshire"))


nh_indicators_fig <- indicators_tbl %>%

    mutate(value = round(value, 1)) %>%

    ggplot(aes(date, value)) +
    geom_line(show.legend = FALSE) +

    facet_wrap(~indicator, scales = "free_y", ncol = 1, labeller = indicator_desc) +

    # geom_rect doesn't show up in plotly
    geom_vline(data = Recession_Dates_tbl %>% filter(USREC == 1),
               aes(xintercept = as.numeric(date)), color = "grey70", size = 0.7, alpha = 0.2) +
    geom_hline(yintercept = 0, color = "#18BC9C", size = 1, alpha = 0.5) +

    labs(y = "Year Over Year Percent Change",
         x = NULL) +

    theme_tq() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank())

ggplotly(nh_indicators_fig)

write_rds(nh_indicators_fig, "00_data/fig/nh_indicators_fig.rds")

