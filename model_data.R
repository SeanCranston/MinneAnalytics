library(tidyverse)
library(readr)
source("data_cleaning.R",echo = FALSE) #to load the Games DF
SeasonStats1993_2021 <- read_csv("SeasonStats1993-2021.csv")
View(SeasonStats1993_2021)

#I can't see if this code works bc the games df is not right yet

#team 0 - team 1. if team 0 then it return 0. if team 1 wins then 1 is returned
key <- sapply(x = 1:dim(games)[1], ifelse(games[x,3]-games[x,4]>0,0,1)) %>% as_tibble()
values <-  filter(SeasonStats1993_2021,School == games$`team 0`)-filter(SeasonStats1993_2021,school == games$`team 1`)

Model_df <- cbind(key,values)

