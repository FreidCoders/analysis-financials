library(tidyverse)

financials <- read_csv("data/raw/Financials.csv")

# Grouper des donnés financiers par segment
financials %>% group_by(Segment) %>% summarise(n=n())

#Changer le nom de certains colonnes

#