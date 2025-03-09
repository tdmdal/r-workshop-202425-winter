# A simple script to collect crypto-currency data from the CoinGecko API
# We are using its free API to get the data
# For certain premium data, one need to pay for the premium API

# note that there is a R library called "geckor" that can be used to interact
# with the CoinGecko API to retrieve data. However, it only supports a few 
# endpoints (i.e., with limited data retrieval scope).

# I will use the httr2 library to interact with the CoinGecko API.
# This is a more general approach and can be used to interact with any web API. 

# load libraries
library(httr2) # for working with web APIs
library(tidyverse)
library(purrr) # for functional programming
library(glue) # for string interpolation
library(lubridate) # for date manipulation
library(scales) # for formatting numbers
library(patchwork) # for combining plots


# 1 import/collect data

# get a list of all supported coins and store them in a dataframe/tibble
# api document on getting the coin list: https://docs.coingecko.com/reference/coins-list
coin_df <- request("https://api.coingecko.com/api/v3/coins/list") |> 
  req_headers("Accept" = "application/json") |>
  req_perform() |>
  resp_body_json() |>
  map_dfr(as_tibble_row)

# take a quick look at all the crazy coins available
View(coin_df)

# let's just retrieve the bitcoin daily data from the past 1 year
# api document: # https://docs.coingecko.com/reference/coins-id-market-chart
# endpoint with parameters:
# https://api.coingecko.com/api/v3/coins/bitcoin/market_chart?vs_currency=usd&days=365&interval=daily
bitcoin_id <- "bitcoin"
chart_api_url <- glue("https://api.coingecko.com/api/v3/coins/{bitcoin_id}/market_chart")

bitcoin_list <- request(chart_api_url) |>
  req_url_query(vs_currency = "usd", days = 365, interval = "daily") |>
  req_headers("Accept" = "application/json") |>
  req_perform() |>
  resp_body_json()

# store retrieved data in a dataframe/tibble
bitcoin_df_price <- bitcoin_list$prices |> 
  map_dfr(function(x) {
    tibble(timestamp = x[[1]], price = x[[2]])
  })

bitcoin_df_market_caps <- bitcoin_list$market_caps |> 
  map_dfr(function(x) {
    tibble(market_caps = x[[2]])
  })

bitcoin_df_volumes <- bitcoin_list$total_volumes |> 
  map_dfr(function(x) {
    tibble(volumes = x[[2]])
  })

bitcoin_df <- bind_cols(bitcoin_df_price, 
                        bitcoin_df_market_caps, 
                        bitcoin_df_volumes) |>
  mutate(timestamp = as_datetime(timestamp / 1000)) # convert UNIX timestamp to datetime

View(bitcoin_df)

# 2 model/analyze data
# omitted for this example

# 3 visualize data
# 3.1 plot the price data
bitcoin_df |> 
  ggplot(aes(x = timestamp, y = price / 1000)) +
  geom_line() +
  labs(title = "Bitcoin Price in the Past 1 Year",
       x = "Date",
       y = "Price (Thousands USD)")

# 3.2 plot price and volumes data in one plot
# Price Plot (Top)
price_plot <- bitcoin_df |>
  ggplot(aes(x = timestamp, y = price / 1000)) +
  geom_line(color = "blue") +
  labs(title = "Bitcoin Price in the Past 1 Year",
       x = NULL,  # Remove x-axis to align with lower plot
       y = "Price (Thousand USD)") +
  theme_minimal()

# Volume Plot (Bottom)
volume_plot <- bitcoin_df |>
  ggplot(aes(x = timestamp, y = volumes / 1e9)) +
  geom_bar(stat = "identity", fill = "gray") +
  labs(title = "Bitcoin Trading Volumes",
       x = "Date",
       y = "Volume (Billion USD)") +
  theme_minimal()

# Volume Plot (Bottom) - Alternative (in terms of number of coins traded)
# volume_plot <- bitcoin_df |>
#   ggplot(aes(x = timestamp, y = volumes / price / 1e6)) +
#   geom_bar(stat = "identity", fill = "gray") +
#   labs(title = "Bitcoin Trading Volumes",
#        x = "Date",
#        y = "Volume (Million Coin)") +
#   theme_minimal()

# Combine Plots (Stacked)
price_plot / volume_plot  # Using patchwork to stack them

