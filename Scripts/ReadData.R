require(pacman)
pacman::p_load(tidyverse, lubridate, zoo)

read_homeownership <- function() {
  homeownership <- read_csv("../Data/Homeownership Rate-1.csv", skip = 6, show_col_types = FALSE) %>%
      drop_na()
  homeownership$Date <- as.Date(as.yearqtr(homeownership$Period, format = "Q%q-%Y"))
  homeownership <- homeownership %>% select(-Period)
  homeownership <- cbind(homeownership, Quarter = as.factor(quarters(homeownership$Date)))
  homeownership <- homeownership[,c('Date', 'Quarter', 'Value')]
  
  return(homeownership)
}

read_interest_rate <- function() {
  interest_rate <- read_csv("../Data/Interest Rate.csv", show_col_types = FALSE) %>% drop_na()
  colnames(interest_rate) <- c("Date", "Rate")
  interest_rate <- cbind(interest_rate, Month = as.factor(month(interest_rate$Date)))
  
  return(interest_rate)
}

read_gdp <- function() {
  gdp <- read_csv("Data/Real GDP by Quarter.csv", show_col_types = FALSE) %>% drop_na()
  colnames(gdp) <- c("Date", "GDP")
  gdp <- cbind(gdp, Quarter = as.factor(quarters(gdp$Date)))
  
  return(gpd)
}

read_sales_price <- function() {
  sales_price <- read_csv("Data/Median Sales Price of Houses Sold by Quarter.csv", show_col_types = FALSE) %>% drop_na()
  colnames(sales_price) <- c("Date", "Price")
  sales_price <- cbind(sales_price, Quarter = as.factor(quarters(sales_price$Date)))
  
  return(sales_price)
}