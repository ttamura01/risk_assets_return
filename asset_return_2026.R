# asset return in 2026
setwd("/Users/takayukitamura/Documents/R_Computing/risk_assets_return")
library(tidyverse)
library(patchwork)
library(ggtext)
library(glue)
library(scales)
library(plotly)

return <- read_csv("asset_return_2026.csv")
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

daily <- "2026-02-19	0.002	-0.024	-0.033	-0.085	-0.112	-0.176	-0.023	-0.041	0.008	-0.044	-0.010	0.043	0.160	0.161	-0.226
2026-02-20	0.009	-0.015	0.004	-0.084	-0.090	-0.179	-0.007	-0.027	0.018	-0.035	-0.006	0.049	0.182	0.161	-0.236"
daily <- gsub("\\s+", ",", daily)
daily

updates <- tribble(~date, ~`S&P500`, ~"NASDAQ", ~"Google",  ~"Tesla", ~"Amazon", ~"Microsoft",  ~"Meta",  ~"Apple", ~"NVIDIA", ~`JP Morgan`, ~"Citigroup", ~`Goldman Sachs`,  ~"Gold", ~"WTI", ~"Bitcoin",
                   "2026-02-19",0.002,-0.024,-0.033,-0.085,-0.112,-0.176,-0.023,-0.041,0.008,-0.044,-0.010,0.043,0.160,0.161,-0.226,
                   "2026-02-20",0.009,-0.015,0.004,-0.084,-0.090,-0.179,-0.007,-0.027,0.018,-0.035,-0.006,0.049,0.182,0.161,-0.236)

updates$date <- as.Date(updates$date, format = "%Y-%m-%d")
# 

return <- rbind(return, updates)

write_csv(return, "asset_return_2026.csv")

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
