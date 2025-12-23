# asset return in 2025
setwd("/Users/takayukitamura/Documents/R_Computing/risk_assets_return")
library(tidyverse)
library(patchwork)
library(ggtext)
library(glue)
library(scales)
library(plotly)

return <- read_csv("asset_return_2025.csv")
#return <- read_csv("asset_return_2025.csv", col_names = c("date", "S.P500", "Nasdaq", "Google", "Tesla", "Amazon", "Microsoft", "Meta", "Apple", "Nvidia", "JP.Morgan", "Citigroup", "Goldman.Sachs", "Gold", "Oil", "Bitcoin"))

# return <- return[-236,]
# return <- return[-c(126, 127), ]
# return[return$date == "2025-11-01", "date"] <- "2025-11-03"
# return <- within(return, date[date == "2025-11-01"] <- "2025-11-03")
# return <- return %>% mutate(return = if_else(date == "2025-10-07", "2025-10-08", date))

# return %>% slice_max(date)


head(return)
tail(return)

# colnames(return)

return$date <- as.Date(return$date, format = "%Y-%m-%d")

daily <- "0.169	0.213	0.635	0.210	0.041	0.150	0.130	0.082	0.368	0.348	0.678	0.570	0.686	-0.199	-0.055"
daily <- gsub("\\s+", ",", daily)
daily

updates <- tribble(~date, ~`S&P500`, ~"NASDAQ", ~"Google",  ~"Tesla", ~"Amazon", ~"Microsoft",  ~"Meta",  ~"Apple", ~"NVIDIA", ~`JP Morgan`, ~"Citigroup", ~`Goldman Sachs`,  ~"Gold", ~"Oil", ~"Bitcoin",
                   "2025-12-22",0.169,0.213,0.635,0.210,0.041,0.150,0.130,0.082,0.368,0.348,0.678,0.570,0.686,-0.199,-0.055)

updates$date <- as.Date(updates$date, format = "%Y-%m-%d")
# 

return <- rbind(return, updates)

write_csv(return, "asset_return_2025.csv")

names(return)

asset_names_cols <- return %>% 
  select(-date) %>% 
  names()

for (col in asset_names_cols){
  return[[col]] <- as.numeric(return[[col]])
}

sapply(return, class)

return$date <- as.Date(return$date, "%Y-%m-%d")

return_long <- return %>% 
  pivot_longer(cols = - date, names_to = "asset", values_to = "price") 


latest_date <- max(return_long$date)

latest_return <-  aggregate(price ~ asset, data = return_long[return_long$date == 
                                                                as.Date(latest_date),], max)

return_long$asset <- factor(return_long$asset, levels = latest_return[order(latest_return$price,
                                                                            decreasing = TRUE), "asset"])

p <- return_long %>% 
  ggplot(aes(x = date, y = price, color = asset)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  theme_classic() +
  labs(title = "Major Financial Asset Return 2025",
       subtitle = "The prices indicating we have digested the significant risk on Tariffs and Geopolitical landscape",
       x = NULL, y = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(),
    plot.subtitle = element_textbox_simple(),
    panel.grid.major = element_line(colour = "gray80")
  )

ggplotly(p)


###
stock_data <- data.frame(
  stock1 = c("123.45", "125.67", "128.90"),
  stock2 = c("98.76", "100.00", "102.34"),
  stock3 = c("200.50", "205.75", "210.00"),
  stock4 = c("50.00","51.01","52.02"),
  stock5 = c("10.01","11.02","12.03"),
  stock6 = c("300.01","301.02","302.03"),
  stock7 = c("400.01","401.02","402.03"),
  stock8 = c("500.01","501.02","502.03"),
  stock9 = c("600.01","601.02","602.03"),
  stock10 = c("700.01","701.02","702.03")
) 

stock_data %>% print()

stock_price_columns <- names(stock_data)

names(stock_data)
for (col in stock_price_columns){
  stock_data[[col]] <- as.numeric(stock_data[[col]])
}

sapply(stock_data, class)

library(ggplot2)
library(dplyr)

# Example dataset
df <- data.frame(
  date = rep(seq(as.Date("2024-01-01"), as.Date("2024-01-10"), by="days"), 10),
  stock = rep(c("AAPL", "MSFT", "TSLA", "GOOGL", "AMZN", "NVDA", "META", "BRK.B", "V", "JPM"), each = 10),
  price = runif(100, 100, 500)  # Random prices
)

# Get latest prices to order the legend
latest_prices <- df %>%
  filter(date == max(date)) %>%
  arrange(desc(price)) %>%
  pull(stock)

# Plot with reordered legend
ggplot(df, aes(x = date, y = price, color = factor(stock, levels = latest_prices))) +
  geom_line() +
  labs(color = "Stock") +
  theme_minimal()



