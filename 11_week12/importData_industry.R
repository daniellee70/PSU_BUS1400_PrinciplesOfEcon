# IMPORT CLEAN DATA ----

# Goal
# Import industry data and visualize

# Set up

# Load packages
library(tidyverse)
library(tidyquant)
library(lubridate)
library(plotly)

# Source function
# source("00_scripts/plot_econ_series.R")


# Import data ----

# symbols
symbols <- c("GDPC1", # real gdp
             "DMOTRX1Q020SBEA", # Groppone
             "DIFSRX1Q020SBEA", # Bank of New Hampshire
             "Y033RX1Q020SBEA") # Comptus

# Download
data <- tq_get(symbols,
               get = "economic.data",
               from = "1947-01-01")

# Recession data
Recession_Dates_tbl <- read_rds("00_data/wrangled_data/Recession_Dates_tbl.rds")


# Transform data
data_clean <- data %>%

    group_by(symbol) %>%

    # Calculate percent change
    mutate(change = (price / lag(price, 4))-1)  %>%
    mutate(text = str_glue("{date %>% quarter(type = 'year.quarter')},
                           Growth: {change %>% scales::percent(accuracy = 0.1)}")) %>%

    filter(date > "2000-01-01")

data_clean

g <- data_clean %>%

    # Calculate percent change
    filter(symbol %in% c("GDPC1",
                         "DMOTRX1Q020SBEA")) %>%

    mutate(symbol = fct_recode(symbol,
                               "GDP" = "GDPC1",
                               "Industry" = "DMOTRX1Q020SBEA")) %>%

    # Plot
    ggplot(aes(date, change)) +
    geom_line(aes(color = symbol)) +
    geom_point(aes(text = text), size = 0.1, alpha = 0) +
    theme_tq() +
    theme(legend.position = "bottom",
          strip.text = element_text(size = 8),
          panel.grid.major.y = element_blank()) +

    geom_hline(yintercept = 0, color = "#18BC9C", size = 0.5, alpha = 0.7) +

    # Shade for recession
    geom_vline(data = Recession_Dates_tbl %>% filter(USREC == 1),
               aes(xintercept = as.numeric(date)), color = "#4876FF", alpha = 0.3) +

    labs(subtitle = "US Recessions in Blue Shade",
         y = "Percent",
         x = NULL,
         color = NULL)

plotly::ggplotly(g, tooltip = "text")















# Create a custom function to plot ----

plot_econ_series <- function(data_clean, var = "PRFIC1") {

    g <- data_clean %>%

        # Calculate percent change
        filter(symbol %in% c("GDPC1",
                             var)) %>%

        mutate(symbol = fct_recode(symbol,
                                   "GDP" = "GDPC1",
                                   "Industry" = var)) %>%

        mutate(symbol = factor(symbol, levels = c("GDP", "Industry"))) %>%

        # Plot
        ggplot(aes(date, change)) +
        geom_line(aes(color = symbol)) +
        geom_point(aes(text = text), size = 0.1, alpha = 0) +
        theme_tq() +
        theme(legend.position = "bottom",
              strip.text = element_text(size = 8),
              panel.grid.major.y = element_blank()) +

        geom_hline(yintercept = 0, color = "#18BC9C", size = 0.5, alpha = 0.7) +

        # Shade for recession
        geom_vline(data = Recession_Dates_tbl %>% filter(USREC == 1),
                   aes(xintercept = as.numeric(date)), color = "#4876FF", alpha = 0.3) +

        labs(subtitle = "US Recessions in Blue Shade",
             y = "Year-Over-Year Percent Change",
             x = NULL,
             color = NULL)

    # Make the plot interactive
    plotly::ggplotly(g, tooltip = "text")

}

dump(list = "plot_econ_series",
     file = "00_scripts/plot_econ_series.R")


# Plot ----

Groppone_fig <-
    data_clean %>% plot_econ_series(var = "DMOTRX1Q020SBEA")

BNH_fig <-
    data_clean %>% plot_econ_series(var = "DIFSRX1Q020SBEA")

Comptus_fig <-
    data_clean %>% plot_econ_series(var = "Y033RX1Q020SBEA")


ggplotly(Groppone_fig)
ggplotly(BNH_fig)
ggplotly(Comptus_fig)

write_rds(Groppone_fig, "00_data/fig/Groppone_fig.rds")
write_rds(BNH_fig, "00_data/fig/BNH_fig.rds")
write_rds(Comptus_fig, "00_data/fig/Comptus_fig.rds")
