# a taste of time series plot using tidyquant and dygraph

# ref1, https://business-science.github.io/tidyquant/index.html
# ref2, https://rstudio.github.io/dygraphs/index.html

# loading libraries
library(tidyverse)
library(tidyquant)
library(timetk)
library(dygraphs)

# Get AAPL and AMZN Stock Prices
AAPL <- tq_get("AAPL", get = "stock.prices", from = "2015-09-01", to = "2016-12-31")

# tidyquant: geom_candlestick()
AAPL %>%
  filter(date < as.Date("2015-12-31")) %>%
  ggplot(aes(x = date, y = close)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
  labs(title = "AAPL Candlestick Chart", y = "Closing Price", x = "") +
  theme_tq()

# tidyquant: geom_candlestick() with gemo_ma() (simple moving average)
AAPL %>%
  ggplot(aes(x = date, y = close)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
  geom_ma(ma_fun = SMA, n = 30, linetype = 5, size = 1.25) +
  geom_ma(ma_fun = SMA, n = 90, color = "red", size = 1.25) + 
  labs(title = "AAPL Candlestick Chart", 
       subtitle = "50 and 200-Day SMA", 
       y = "Closing Price", x = "") +
  theme_tq()

# dygraph: interactive candlestick
dateWindow <- c("2015-09-01", "2015-12-31")
AAPL %>% 
  select(date, open, high, low, close) %>%
  tk_xts() %>%      # convert tibble to xts
  dygraph() %>% 
  dyCandlestick() %>%
  dyRangeSelector(dateWindow = dateWindow)