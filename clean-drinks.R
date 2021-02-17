library(tidyverse)

d <- read.csv("~/Downloads/wise-bartender.csv")

#Get list of products sold
product <- d %>%
  select(ProductName)

#Select unique products sold
unique <- product %>%
  distinct(ProductName) 

unique <- unique %>%
  
