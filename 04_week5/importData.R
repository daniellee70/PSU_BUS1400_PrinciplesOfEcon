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

symbols <- c("CUSR0000SETA01", "PCU441110441110101")

grappone_rawData <- tq_get(x   = symbols,
                             get = "economic.data")

## Transform data ----

grappone_tbl <- grappone_rawData %>%

    # Change values in symbol column
    mutate(symbol = case_when(symbol == "CUSR0000SETA01" ~ "outputPrice", symbol == "PCU441110441110101" ~ "inputPrice")) %>%

    # Calculate year-over-year change
    mutate(value = (price/lag(price, n = 12)-1)*100) %>%
    select(-price) %>%

    # Remove data before 2013 for odd behavior
    filter(date > "2013-01-01") %>%

    mutate(value = round(value, 1)) %>%
    rename(grappone = value)


# Comptus ----

symbols <- c("WPS118", "WPS102")

comptus_rawData <- tq_get(x   = symbols,
                          get = "economic.data")

## Transform data ----

comptus_tbl <- comptus_rawData %>%

    # Change values in symbol column
    mutate(symbol = case_when(symbol == "WPS118" ~ "outputPrice", symbol == "WPS102" ~ "inputPrice")) %>%

    # Calculate year-over-year change
    mutate(value = (price/lag(price, n = 12)-1)*100) %>%
    select(-price) %>%

    # Remove data before 2013 for odd behavior
    filter(date > "2013-01-01") %>%

    mutate(value = round(value, 1)) %>%
    rename(comptus = value)


# Bank of New Hampshire ----

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

    mutate(value = round(value, 1)) %>%
    rename(bnh = value)


# Join three companies ----

all_companies_tbl <- list(bnh_tbl, comptus_tbl, grappone_tbl) %>%

    reduce(left_join) %>%

    # transform to long
    pivot_longer(cols = bnh:grappone,
                 names_to = "companies",
                 values_to = "value")

all_companies_tbl

## Plot data ----

labeller_desc <- as_labeller(c(bnh      = "Bank of New Hampshire",
                               comptus  = "Comptus",
                               grappone = "Grappone"))

input_output_price_fig <- all_companies_tbl %>%

    ggplot(aes(date, value, color = symbol)) +
    geom_line() +
    facet_wrap(~companies, scales = "free",
               ncol = 1, labeller = labeller_desc) +
    geom_hline(yintercept = 0, color = "#18BC9C",
               linewidth = 1, alpha = 0.5) +

    # Formatting

    theme_tq() +
    scale_color_tq() +
    theme(legend.position = "bottom") +

    labs(title = "Input and Output Price Changes, YOY %",
         caption = "outputPrice of Grappone = Consumer Price Index for All Urban Consumers: New Vehicles in U.S. City Average
         inputPrice of Grappone = Producer Price Index by Industry: New Car Dealers: New Vehicle Sales
         outputPrice of Comptus = Producer Price Index by Commodity: Machinery and Equipment: Miscellaneous Instruments
         inputPrice of Comptus = Producer Price Index by Commodity: Metals and Metal Products: Nonferrous Metals
         outputPrice of BNH = Consumer Price Index for All Urban Consumers: New Vehicles in U.S. City Average
         inputPrice of BNH = Producer Price Index by Industry: New Car Dealers: New Vehicle Sales",
         x = NULL,
         y = NULL,
         color = NULL)

ggplotly(input_output_price_fig)

write_rds(input_output_price_fig, "../00_data/fig/input_output_price_fig.rds")




