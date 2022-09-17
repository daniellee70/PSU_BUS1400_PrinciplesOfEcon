# VISUALIZE DATA ----

# Goal
# Experiment data visualization for economic dashboard
# Create data visualizing functions for fred time series with us recessions
# Use geom_vline() for recessions. geom_rec() doesn't work for plotly

# https://rpubs.com/jkang09/620791

# Set up

# Core
library(tidyverse)
library(tidyquant)

# Time series
library(timetk)
library(lubridate)

# Interactive visualization
library(plotly)

# Import Data
# Market Indicators
VIX_tbl                <- read_rds("00_data/wrangled_data//VIX_tbl.rds")
Treasury10_Yield_tbl   <- read_rds("00_data/wrangled_data//Treasury10_Yield_tbl.rds")
Yield_Spread_tbl       <- read_rds("00_data/wrangled_data//Yield_Spread_tbl.rds")
Home_Price_tbl         <- read_rds("00_data/wrangled_data//Home_Price_tbl.rds")

# Economic Indicators - US
Recession_Dates_tbl    <- read_rds("00_data/wrangled_data//Recession_Dates_tbl.rds")
Inflation_tbl          <- read_rds("00_data/wrangled_data//Inflation_tbl.rds")
Unemployment_Rate_tbl  <- read_rds("00_data/wrangled_data//Unemployment_Rate_tbl.rds")
GDP_Growth_tbl         <- read_rds("00_data/wrangled_data//GDP_Growth_tbl.rds")
Consumer_Sentiment_tbl <- read_rds("00_data/wrangled_data//Consumer_Sentiment_tbl.rds")


# 1 Market and Economic Indicators ----

## 1.1 Plot all in one ----

market_indicators_tbl <- list(Yield_Spread_tbl %>% rename(Yield_Spread = value),
                              VIX_tbl %>% rename(VIX = value),
                              Home_Price_tbl %>%

                                  # Calculate year-over-year change
                                  mutate(value = (value/lag(value, n = 12)-1)*100) %>%
                                  rename(Home_Price = value)) %>%

    # Aggregate
    reduce(left_join) %>%

    # Transform data to long form
    pivot_longer(cols = -date, names_to = "indicator", values_to = "value")


# Necessary to put variables into the facet labels
indicator_desc <- as_labeller(
    c(`Home_Price` = "S&P/Case-Shiller 20-City Composite Home Price Index (year-over-year percent change)",
      `VIX` = "The Chicago Board Options Exchange Volatility Index",
      `Yield_Spread` = "Treasury Yield Spread between the 10 Year Note and the 3 Month Bill"))


market_indicators_fig <- market_indicators_tbl %>%

    mutate(value = round(value, 1)) %>%

    ggplot(aes(date, value)) +
    geom_line(show.legend = FALSE) +

    facet_wrap(~indicator, scales = "free", ncol = 1, labeller = indicator_desc) +

    # geom_rect doesn't show up in plotly
    geom_vline(data = Recession_Dates_tbl %>% filter(USREC == 1),
               aes(xintercept = as.numeric(date)), color = "grey70", size = 0.7, alpha = 0.2) +
    geom_hline(yintercept = 0, color = "#18BC9C", size = 1, alpha = 0.5) +

    labs(y = NULL,
         x = NULL) +

    theme_tq() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank())

ggplotly(market_indicators_fig)

write_rds(market_indicators_fig, "fig/market_indicators_fig.rds")


economic_indicators_tbl <- list(Inflation_tbl %>% rename(Inflation = value),
                                Unemployment_Rate_tbl %>% rename(Unemployment_Rate = value),
                                Consumer_Sentiment_tbl %>%

                                    # Calculate year-over-year change
                                    mutate(value = (value/lag(value, n = 12)-1)*100) %>%
                                    rename(Consumer_Sentiment = value)) %>%

    # Aggregate
    reduce(left_join) %>%

    # Transform data to long form
    pivot_longer(cols = -date, names_to = "indicator", values_to = "value")


# Necessary to put variables into the facet labels
indicator_desc <- as_labeller(
    c(`Consumer_Sentiment` = "Consumer Sentiment Index (year-over-year percent change)",
      `Inflation` = "Consumer Price Index (year-over-year percent change)",
      `Unemployment_Rate` = "Unemployment Rate (percent)"))


economic_indicators_fig <- economic_indicators_tbl %>%

    mutate(value = round(value, 1)) %>%

    ggplot(aes(date, value)) +
    geom_line(show.legend = FALSE) +

    facet_wrap(~indicator, scales = "free", ncol = 1, labeller = indicator_desc) +

    # geom_rect doesn't show up in plotly
    geom_vline(data = Recession_Dates_tbl %>% filter(USREC == 1),
               aes(xintercept = as.numeric(date)), color = "grey70", size = 0.7, alpha = 0.2) +
    geom_hline(yintercept = 0, color = "#18BC9C", size = 1, alpha = 0.5) +

    labs(y = NULL,
         x = NULL,
         caption = "Source: FRED at St. Louis Fed") +

    theme_tq()  +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank() )


ggplotly(economic_indicators_fig)

write_rds(economic_indicators_fig, "fig/economic_indicators_fig.rds")


## 1.2 Plot each series separate ----

g <- Treasury10_Yield_tbl %>%

    ggplot(aes(date, value)) +
    geom_line() +

    # geom_rect doesn't show up in plotly
    geom_vline(data = Recession_Dates_tbl %>% filter(USREC == 1),
               aes(xintercept = as.numeric(date)), color = "#4876FF", alpha = 0.3) +

    labs(title = "Market Yield on U.S. Treasury Securities at 10-Year Constant Maturity",
         subtitle = "US Recessions in Blue Shade",
         y = "Percent",
         x = NULL)


ggplotly(g)
