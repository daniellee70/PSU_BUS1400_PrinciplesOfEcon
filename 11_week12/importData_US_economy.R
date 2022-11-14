# IMPORT CLEAN DATA ----

# Goal
# Import data to build economic dashboard of Russell Investment
# https://russellinvestments.com/us/resources/financial-professionals/economic-indicators-dashboard

# Set up

# Core
library(tidyverse)
library(tidyquant)

# FRED data
library(eFRED)

# BEA data
library(bea.R)

# Time series
library(timetk)


api_key <- "11c92c9630cc2c5c83eb5da3b01a59f0"
set_fred_key(api_key)



# 1 Market Indicators ----

## * Market Volatility ----

download_and_clean_VIX <- function() {

    search_results <- fred_search(text = "CBOE VIX", key = api_key)

    VIX_tbl <- search_results %>%

        # Get id
        filter(title == "CBOE Volatility Index: VIX") %>%
        pull(id) %>%

        # Download data
        fred(key = api_key) %>%
        set_names(c("date", "value")) %>%

        # Convert to monthly
        summarise_by_time(.date_var = date, value = mean(value, na.rm = T), .by = "month") %>%

        # Move date to the last day of the month
        mutate(date = date %+time% "1 month" %-time% "1 day")

}

VIX_tbl <- download_and_clean_VIX()

write_rds(VIX_tbl, "00_data/wrangled_data/VIX_tbl.rds")

## * 10 Year US Treasury Yield ----

download_and_clean_Treasury10_Yield <- function() {

    search_results <- fred_search(text = "Treasury", key = api_key)

    Treasury10_Yield_tbl <- search_results %>%

        # Get id (Inflation-Indexed measure is also available in FRED)
        filter(title == "Market Yield on U.S. Treasury Securities at 10-Year Constant Maturity, Quoted on an Investment Basis") %>%
        pull(id) %>%

        # Download
        fred(key = api_key) %>%
        select(date, DGS10) %>%
        set_names(c("date", "value")) %>%

        # Remove NAs before 1962
        filter(date >= ymd("1962-01-01")) %>%

        # Convert to monthly
        summarise_by_time(.date_var = date, value = last(value), .by = "month") %>%

        # Move date to the last day of the month
        mutate(date = date %+time% "1 month" %-time% "1 day")

}

Treasury10_Yield_tbl <- download_and_clean_Treasury10_Yield()

write_rds(Treasury10_Yield_tbl, "00_data/wrangled_data/Treasury10_Yield_tbl.rds")

## * Yield Spread ----

download_and_clean_Yield_Spread <- function() {

    search_results <- fred_search(text = "Treasury", key = api_key)

    Yield_Spread_tbl <- search_results %>%

        # Get id
        filter(title == "10-Year Treasury Constant Maturity Minus 3-Month Treasury Constant Maturity") %>%
        filter(frequency == "Daily") %>%
        pull(id) %>%

        # Download
        fred(key = api_key) %>%
        set_names(c("date", "value")) %>%

        # Convert to monthly
        summarise_by_time(.date_var = date, value = last(value), .by = "month") %>%

        # Move date to the last day of the month
        mutate(date = date %+time% "1 month" %-time% "1 day")

}

Yield_Spread_tbl <- download_and_clean_Yield_Spread()

write_rds(Yield_Spread_tbl, "00_data/wrangled_data/Yield_Spread_tbl.rds")


## * Home Prices ----

download_and_clean_Home_Price <- function() {

    search_results <- fred_search(text = "Home Price", key = api_key)

    Home_Price_tbl <- search_results %>%

        # Get id
        filter(title == "S&P/Case-Shiller 20-City Composite Home Price Index") %>%
        filter(seasonal_adjustment_short == "NSA") %>%
        pull(id) %>%

        # Download
        fred(key = api_key) %>%
        set_names(c("date", "value")) %>%

        # Move date to the last day of the month
        mutate(date = date %+time% "1 month" %-time% "1 day") %>%

        filter(!is.na(value))

}

Home_Price_tbl <- download_and_clean_Home_Price()

write_rds(Home_Price_tbl, "00_data/wrangled_data/Home_Price_tbl.rds")



# 2 Economic Indicators: US ----

## * Recession Dates ----

download_and_clean_Recession_Dates <- function() {

    search_results <- fred_search(text = "recession", key = api_key)

    Recession_Dates_tbl <- search_results %>%
        filter(id == "USREC") %>%
        pull(id) %>%
        fred(key = api_key) %>%

        # Move date to the last day of the month
        mutate(date = date %+time% "1 month" %-time% "1 day")

}

Recession_Dates_tbl <- download_and_clean_Recession_Dates()

write_rds(Recession_Dates_tbl, "00_data/wrangled_data/Recession_Dates_tbl.rds")

## * Inflation ----

download_and_clean_Inflation <- function() {

    search_results <- fred_search(text = "consumer price index", key = api_key)

    Inflation_tbl <- search_results %>%

        # Get id
        filter(title == "Consumer Price Index: Total All Items for the United States") %>%
        filter(frequency_short == "M") %>%
        filter(units == "Growth rate same period previous year") %>%
        pull(id) %>%

        # Download
        fred(key = api_key) %>%
        set_names(c("date", "value")) %>%

        # Move date to the last day of the month
        mutate(date = date %+time% "1 month" %-time% "1 day") %>%

        filter(!is.na(value))

}

Inflation_tbl <- download_and_clean_Inflation()

write_rds(Inflation_tbl, "00_data/wrangled_data/Inflation_tbl.rds")

## * Unemployment ----

download_and_clean_Unemployment_Rate <- function() {

    search_results <- fred_search(text = "unemployment", key = api_key)

    Unemployment_Rate_tbl <- search_results %>%

        # Get id
        filter(title == "Unemployment Rate") %>%
        filter(seasonal_adjustment_short == "SA") %>%
        pull(id) %>%

        # Download
        fred(key = api_key) %>%
        set_names(c("date", "value")) %>%

        # Move date to the last day of the month
        mutate(date = date %+time% "1 month" %-time% "1 day")

}

Unemployment_Rate_tbl <- download_and_clean_Unemployment_Rate()

write_rds(Unemployment_Rate_tbl, "00_data/wrangled_data/Unemployment_Rate_tbl.rds")

## * Economic Expansion ----

download_and_clean_GDP_Growth <- function() {

    search_results <- fred_search(text = "gross domestic product", key = api_key)

    GDP_Growth_tbl <- search_results %>%

        # Get id
        filter(title == "Real Gross Domestic Product") %>%
        filter(seasonal_adjustment_short == "SAAR") %>%
        filter(units == "Percent Change from Preceding Period") %>%
        pull(id) %>%

        # Download
        fred(key = api_key) %>%
        set_names(c("date", "value")) %>%

        # Move date to the last day of the quarter
        mutate(date = date %+time% "3 months" %-time% "1 day")

}

GDP_Growth_tbl <- download_and_clean_GDP_Growth()

write_rds(GDP_Growth_tbl, "00_data/wrangled_data/GDP_Growth_tbl.rds")

## * Consumer Sentiment ----

download_and_clean_Consumer_Sentiment <- function() {

    search_results <- fred_search(text = "Consumer Sentiment", key = api_key)

    Consumer_Sentiment_tbl <- search_results %>%

        # Get id
        filter(title == "University of Michigan: Consumer Sentiment") %>%
        pull(id) %>%

        # Download
        fred(key = api_key) %>%
        set_names(c("date", "value")) %>%

        # Move date to the last day of the month
        mutate(date = date %+time% "1 month" %-time% "1 day")

}

Consumer_Sentiment_tbl <- download_and_clean_Consumer_Sentiment()

write_rds(Consumer_Sentiment_tbl, "00_data/wrangled_data/Consumer_Sentiment_tbl.rds")
