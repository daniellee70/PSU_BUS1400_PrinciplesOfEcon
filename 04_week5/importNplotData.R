# IMPORT CLEAN AND PLOT DATA ----

# Goal
# Import inflation data from inputs and outputs for client businesses

# Grappone
# output price: Consumer Price Index for All Urban Consumers: New Vehicles in U.S. City Average (CUSR0000SETA01)
# input prices: Producer Price Index by Industry: New Car Dealers: New Vehicle Sales (PCU441110441110101)

# Bank of New Hampshire
# Output price (long term rate): Market Yield on U.S. Treasury Securities at 10-Year Constant Maturity, Quoted on an Investment Basis (GS10)
# Input price (short term rate): Market Yield on U.S. Treasury Securities at 3-Month Constant Maturity, Quoted on an Investment Basis (GS3M)
# Use the Yield Spread chart from Week 4

# Comptus
# Output price: Producer Price Index by Commodity: Machinery and Equipment: Miscellaneous Instruments (WPS118)
# Input price:  Producer Price Index by Commodity: Metals and Metal Products: Nonferrous Metals (WPS102)

# Set up

# Core
library(tidyverse)
library(tidyquant)

# Interactive visualization
library(plotly)

# Time series
library(timetk)

# Grappone ----

## Import data ----

symbols <- c("CUSR0000SETA01", "PCU441110441110101")

grappone_rawData <- tq_get(x   = symbols,
                             get = "economic.data")

## Transform data ----

grappone_tbl <- grappone_rawData %>%

    # Change values in symbol column
    mutate(symbol = case_when(symbol == "CUSR0000SETA01" ~ "outputPrice", symbol == "PCU441110441110101" ~ "inputPrice")) %>%

    # Calculate year-over-year change
    mutate(value = (price/lag(price, n = 12)-1)*100) %>%

    # Remove data before 2013 for odd behavior
    filter(date > "2013-01-01") %>%

    mutate(value = round(value, 1))


## Plot data ----

grappone_fig <- grappone_tbl %>%

    ggplot(aes(date, value, color = symbol)) +
    geom_line() +

    labs(caption = "outputPrice = Consumer Price Index for All Urban Consumers: New Vehicles in U.S. City Average
         inputPrice = Producer Price Index by Industry: New Car Dealers: New Vehicle Sales",
         x = NULL,
         y = "Year-Over-Year Growth Rate")

ggplotly(grappone_fig)

write_rds(grappone_fig, "04_week5/fig/grappone_fig.rds")

# Comptus ----

## Import data ----

symbols <- c("WPS118", "WPS102")

comptus_rawData <- tq_get(x   = symbols,
                          get = "economic.data")

## Transform data ----

comptus_tbl <- comptus_rawData %>%

    # Change values in symbol column
    mutate(symbol = case_when(symbol == "WPS118" ~ "outputPrice", symbol == "WPS102" ~ "inputPrice")) %>%

    # Calculate year-over-year change
    mutate(value = (price/lag(price, n = 12)-1)*100) %>%

    # Remove data before 2013 for odd behavior
    filter(date > "2013-01-01") %>%

    mutate(value = round(value, 1))


## Plot data ----

comptus_fig <- comptus_tbl %>%

    ggplot(aes(date, value, color = symbol)) +
    geom_line() +

    labs(caption = "outputPrice = Producer Price Index by Commodity: Machinery and Equipment: Miscellaneous Instruments
         inputPrice = Producer Price Index by Commodity: Metals and Metal Products: Nonferrous Metals",
         x = NULL,
         y = "Year-Over-Year Growth Rate")

ggplotly(comptus_fig)

write_rds(comptus_fig, "04_week5/fig/comptus_fig.rds")



# Bank of New Hampshire ----

## Import data ----

symbols <- c("GS10", "GS3M")

bnh_rawData <- tq_get(x   = symbols,
                          get = "economic.data")

## Transform data ----

bnh_tbl <- bnh_rawData %>%

    # Change values in symbol column
    mutate(symbol = case_when(symbol == "GS10" ~ "outputPrice", symbol == "GS3M" ~ "inputPrice")) %>%

    # Calculate year-over-year change
    # mutate(value = (price - lag(price, n = 12))) %>%
    rename(value = price) %>%

    # Remove data before 2013 for odd behavior
    filter(date > "2013-01-01") %>%

    mutate(value = round(value, 1))


## Plot data ----

bnh_fig <- bnh_tbl %>%

    ggplot(aes(date, value, color = symbol)) +
    geom_line() +

    scale_y_continuous(labels = scales::percent_format(scale = 1))+

    labs(caption = "outputPrice = Market Yield on U.S. Treasury Securities at 10-Year Constant Maturity
         inputPrice = Market Yield on U.S. Treasury Securities at 3-Month Constant Maturity",
         x = NULL,
         y = "Yield")

ggplotly(bnh_fig)

write_rds(bnh_fig, "04_week5/fig/bnh_fig.rds")
