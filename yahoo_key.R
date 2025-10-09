library(tidyverse)
library(showtext)
library(quantmod)
library(ggtext)

sandp <- getSymbols("^GSPC", auto.assign = FALSE) %>% 
  as_tibble(rownames = "date") %>% 
  select(date, close = GSPC.Close) 

wti <- getSymbols("CL=F", auto.assign = FALSE) %>%
  as_tibble(rownames = "date") %>%
  select(date, close = `CL=F.Close`)
tail(wti) 

library(quantmod)
library(tidyverse)

# Download daily Bitcoin prices (USD)
btc <- getSymbols("BTC-USD", auto.assign = FALSE) %>%
  as_tibble(rownames = "date") %>%
  select(date, close = `BTC-USD.Close`)

# Preview data
head(btc)
tail(btc)

wti_btc <- wti %>% 
  inner_join(., btc, by = "date") %>% 
  filter(date >= "2025-01-02")

getwd()
