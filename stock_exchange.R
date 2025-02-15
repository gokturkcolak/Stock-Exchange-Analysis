library(tidyverse)
library(zoo)

stock = read_csv('indexData.csv', show_col_types = FALSE)

summary(stock)
head(stock)
stock$Close <- as.numeric(stock$Close)

# Computation of returns
stock <- stock %>% 
  group_by(Index) %>% 
  arrange(Date) %>%
  mutate(daily_return = (Close - lag(Close)) / lag(Close))

# Computation of average returns for each index
average_returns = stock %>% 
  group_by(Index) %>% 
  summarise(avg_return = mean(daily_return, na.rm = TRUE))

# Annualizing the returns
average_returns <- average_returns %>%
  mutate(annualized_return = (1 + avg_return)^252 - 1)

# Identifying the index with the highest annualized return
highest_annual_return <- average_returns %>%
  filter(annualized_return == max(annualized_return))

print(highest_annual_return)

# Visualization of the annualized returns
ggplot(average_returns, aes(x = Index, y = annualized_return, fill = Index)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Annualized Returns by Index",
       x = "Index",
       y = "Annualized Return") +
  theme_minimal()


## Filter the data for IXIC
ixic_data <- stock %>% 
  filter(Index == "IXIC") %>% 
  arrange(Date)


## Computation of the 30-day moving average
ixic_data <- ixic_data %>% 
  mutate(moving_avg_30 = rollapply(Close, width = 30, FUN = mean, fill = NA, align = "right"))

## Plot of the 30-day moving average
ggplot(ixic_data, aes(x = Date)) +
  geom_line(aes(y = Close, color = "Daily Close Price"), linewidth = 0.7) +
  geom_line(aes(y = moving_avg_30, color = "30-Day Moving Average"), linewidth = 1.2) +
  labs(title = "30-Day Moving Average for IXIC (NASDAQ Composite)",
       x = "Date",
       y = "Price",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Daily Close Price" = "blue", "30-Day Moving Average" = "red"))


### Computation of volatility (standard deviation of daily returns)
volatility <- stock %>%
  group_by(Index) %>%
  summarise(volatility = sd(daily_return, na.rm = TRUE))

### Printing the volatility results
print(volatility)

### Visualization of the volatilities
ggplot(volatility, aes(x = Index, y = volatility, fill = Index)) +
  geom_bar(stat = "identity") +
  labs(title = "Volatility Comparison of Indexes",
       x = "Index",
       y = "Volatility (Standard Deviation of Daily Returns)") +
  theme_minimal()


