# asset return in 2025 vs. 2024
setwd("/Users/takayukitamura/Documents/R_Computing/risk_assets_return")

library(tidyverse)
library(showtext)
library(ggtext)

asset_return <- tribble(
  ~Asset, ~"2024", ~"2025",
  "S&P 500", 24.01, 16.30,
  "NASDAQ", 28.64, 22.86,
  "Google",	35.13,	47.98,
  "Tesla",	62.52,	13.05,
  "Meta",	65.42,	10.73,
  "Apple",	30.07,	7.97,
  "NVIDIA",	171.18,	50.79,
  "JP Morgan",	40.92,	29.79,
  "Citigroup",	40.92,	43.81,
  "Goldman Sachs",	48.44,	37.85,
  "Gold", 	26.66,	52.03,
  "WTI",	0.77,	-16.61,
  "Bitcoin",	119.61,	18.31)
  
order_2025 <- asset_return %>%
    arrange(`2025`) %>%
    pull(Asset)

pad <- 2
y_offset <- 0.25  # how much to stagger vertically

asset_return %>%
  pivot_longer(-Asset, names_to = "year", values_to = "return") %>%
  mutate(
    Asset = factor(Asset, levels = order_2025),
    year  = factor(year, levels = c("2025", "2024")),
    x_lab = if_else(return >= 0, return + pad, return - pad),
    h_just = if_else(return >= 0, 0, 1),
    # manually compute y offset
    y_lab = as.numeric(Asset) + if_else(year == "2025", y_offset, -y_offset),
    lab = sprintf("%.2f%%", return)
  ) %>%
  ggplot(aes(x = return, y = Asset, fill = year)) +
  geom_col(position = position_dodge2(preserve = "single", reverse = TRUE)) +
  geom_text(
    aes(x = x_lab, y = y_lab, label = lab, hjust = h_just, color = year),
    size = 3.5,
    show.legend = FALSE
  ) +
  scale_fill_discrete(limits = c("2025", "2024")) +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(.05, .1))) +
  coord_cartesian(clip = "off", expand = TRUE, xlim = c(-35, 185)) +
  labs(
    title = "Asset Returns: 2024 vs 2025",
    x = "Return (%)", y = NULL, fill = "Year"
  ) +
  theme(
    # legend.position = "top",
    legend.position = "top",
    legend.title = element_text(face = "bold", hjust = 0.3),
    legend.title.position = "top",
    legend.key.size = unit(30, "pt"),
    # legend.key.spacing.x = unit(3, "pt"),
    legend.box.spacing = unit(5, "pt"),
    axis.text.y = element_text(face = "bold"),
    panel.background = element_blank(),
    axis.ticks = element_blank()
  )

ggsave("asset_return_2025.png", width = 5.75, height = 6.25)
