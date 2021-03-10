library(tidyverse)
library(readxl)

#if someone can think of better var name dont be afraid to use it

#Cleaning season 2020-2021
Year0 <- read_excel("NCAA Statistics.xlsx",sheet = "2020-2021")
#the w-L column didn't come in right
View(Year0)


#cleaning season 2019-2020
Year1 <- read_excel("NCAA Statistics.xlsx", sheet = "2019-2020")
#split the W-L column into two columns 
a <- str_split(Year1$`W-L`,"-")
for (i in 1:dim(Year1)[1]) {
    Year1$W[i] <- a[[i]][1] %>% as.numeric()
    Year1$L[i] <- a[[i]][2] %>% as.numeric()
}
Year1 <- Year1 %>% mutate('W-L'=NULL) #removed W-L column



#Cleaning season 2018-2019
Year2 <- read_excel("NCAA Statistics.xlsx", sheet = "2018-2019")
#split the W-L column into two columns 
b <- str_split(Year2$`W-L`,"-")
for (i in 1:dim(Year2)[1]) {
    Year2$W[i] <- b[[i]][1] %>% as.numeric()
    Year2$L[i] <- b[[i]][2] %>% as.numeric()
}
Year2 <- Year2 %>% mutate('W-L'=NULL) #removed W-L column
