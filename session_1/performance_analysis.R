# portfolio performance analysis
# evaluate the returns of several technology stocks against the SPDR Technology ETF (XLK)
# source: https://cran.r-project.org/web/packages/tidyquant/vignettes/TQ05-performance-analysis-with-tidyquant.html

# 0) load packages needed for this analysis
library(tidyverse)
library(tidyquant)
library(huxtable)

# 1) import data: download data from Internet source
# three tech stocks
stock_prices <- c("AAPL", "GOOG", "NFLX") %>%
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31")

# a baseline tech ETF
baseline_prices <- "XLK" %>%
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to = "2015-12-31")

# 2) transform data
# 2.1) get monthly returns
stock_returns_monthly <- stock_prices %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted, 
               mutate_fun = periodReturn, 
               period = "monthly", 
               col_rename = "Ra")

baseline_returns_monthly <- baseline_prices %>%
  tq_transmute(select = adjusted, 
               mutate_fun = periodReturn, 
               period = "monthly", 
               col_rename = "Rb")

# 2.2) form a portfolio and get its monthly return
wts_map <- tibble(
  symbols = c("AAPL", "NFLX"),
  weights = c(0.5, 0.5)
)

portfolio_returns_monthly <- stock_returns_monthly %>%
  tq_portfolio(assets_col = symbol, 
               returns_col = Ra, 
               weights = wts_map, 
               col_rename = "Ra")

# 2.3) combine portfolio and baseline return for portfolio performance analysis
RaRb_single_portfolio <- left_join(portfolio_returns_monthly, 
                                   baseline_returns_monthly,
                                   by = "date")

# 3) model & report: compute portfolio performance measures
# 3.1) annualized return
RaRb_single_portfolio %>%
  tq_performance(Ra = Ra, 
                 Rb = NULL,
                 Rf = 0.03 / 12,
                 performance_fun = table.AnnualizedReturns) %>%
  as_hux() %>% # start to draw report table
  set_bold(1, everywhere) %>%
  set_bottom_border(1, everywhere) %>%
  set_number_format(2) %>%
  print_screen(colnames = FALSE)

# 3.2) CAPM measures
RaRb_single_portfolio %>%
  tq_performance(Ra = Ra, 
                 Rb = Rb,
                 Rf = 0.03 / 12,
                 performance_fun = table.CAPM) %>%
  select(ActivePremium, Alpha, AnnualizedAlpha, Beta) %>%
  as_hux() %>% # start to draw report table
  set_bold(1, everywhere) %>%
  set_bottom_border(1, everywhere) %>%
  set_number_format(2) %>%
  print_screen(colnames = FALSE)
  
# 3.3) plot portfolio monthly growth graph
# 3.3.1) calculate monthly growth
portfolio_growth_monthly <- stock_returns_monthly %>%
  tq_portfolio(assets_col   = symbol, 
               returns_col  = Ra, 
               weights      = wts_map, 
               col_rename   = "investment_growth",
               wealth.index = TRUE) %>%
  mutate(investment_growth = investment_growth * 10000)

# 3.3.2) plot monthly growth
portfolio_growth_monthly %>%
  ggplot(aes(x = date, y = investment_growth)) +
  geom_line() +
  labs(title = "Portfolio Growth",
       subtitle = "50% AAPL, 0% GOOG, and 50% NFLX",
       caption = "* Portfolio starts with $10,000",
       x = "", y = "Portfolio Value") +
  geom_smooth(method = "loess") +
  scale_y_continuous(labels = scales::dollar)

# Note: run the below to see performance analysis functions available
# tq_performance_fun_options()
  
