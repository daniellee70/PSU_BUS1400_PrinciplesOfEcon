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

# symbols
symbols <- c("GDPC1", # real gdp
             "DMOTRX1Q020SBEA", # Groppone
             "DIFSRX1Q020SBEA", # Bank of New Hampshire
             "Y033RX1Q020SBEA") # Comptus

# Download
data <- tq_get(symbols,
               get = "economic.data",
               from = "1947-01-01") %>%

    # Recode
    mutate(symbol = case_when(
        symbol == "GDPC1"           ~ "GDP",
        symbol == "DMOTRX1Q020SBEA" ~ "Graponne",
        symbol == "DIFSRX1Q020SBEA" ~ "BNH",
        symbol == "Y033RX1Q020SBEA" ~ "Comptus",
        TRUE                        ~ symbol
    ))
# Recession data
Recession_Dates_tbl <- read_rds("00_data/wrangled_data/Recession_Dates_tbl.rds")


# Transform data ----

data_clean <- data %>%

    group_by(symbol) %>%

    # Calculate percent change
    mutate(change = (price / lag(price,4))-1) %>%

    # Remove GDP?
    # filter(indicator != "GDP") %>%

    # Remove data prior to 2004
    filter(date > "2004-01-01") %>%

    # Rearrange variables
    select(date, indicator = symbol, value = change) %>%

    # Reorder levels
    mutate(indicator = factor(indicator,
                              levels = c("GDP","BNH","Comptus","Graponne")))

data_clean



# Make plot ----

# Necessary to put variables into the facet labels
indicator_desc <- as_labeller(
    c(`Graponne` = "Real personal consumption expenditures: Durable goods: Motor vehicles and parts",
      `BNH` = "Real personal consumption expenditures: Financial services and insurance",
      `Comptus` = "Real Gross Private Domestic Investment: Fixed Investment: Nonresidential: Equipment",
      `GDP` = "Real Gross Domestic Product"))


g <- data_clean %>%

    mutate(value = round(value, 3)) %>%

    ggplot(aes(date, value)) +
    geom_line(show.legend = FALSE) +

    facet_wrap(~indicator, scales = "free", ncol = 1, labeller = indicator_desc) +

    # geom_rect doesn't show up in plotly
    geom_vline(data = Recession_Dates_tbl %>% filter(USREC == 1),
               aes(xintercept = as.numeric(date)), color = "grey70", size = 0.7, alpha = 0.2) +
    geom_hline(yintercept = 0, color = "#18BC9C", size = 1, alpha = 0.5) +

    labs(title = "Year Over Year Percent Change",
         y = NULL,
         x = NULL) +

    theme_tq() +
    scale_y_continuous(labels = scales::percent_format()) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank())

ggplotly(g)


write_rds(g, "00_data/fig/industry_indicators_fig.rds")

