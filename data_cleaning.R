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





-------------------------------------------------------------------------------
#from https://www.ncaa.com/scoreboard/basketball-men/d1/2021/03/10/all-conf
# data for validating model
url1 <- "https://www.ncaa.com/scoreboard/basketball-men/d1/2021/03/0"#note that their is a zero there so this url only works for days before the 10th  
end <- "/all-conf"
url <- c()
lis <- c()
df <- c()
for (i in 1:2) { #only going through 1:2 to debug
    
    url[i] <- paste0(url1,i,end)
    lis[[i]] <- url[i] %>%
        read_html() %>%
        html_nodes(".gamePod.gamePod-type-game.status-final") %>%
        html_text()
    
    df <- c(df,lis[[i]]) 
    #we don't want the data in a list, and it would get to messy me to concentatnate the lis variable in the loop (I think anyways)
}

# this jsut gets rid of that nasty html stuff (I think that's what it is anyway)
df <- gsub("\n","",df)
df <- gsub("  ","",df) 
df <- gsub("FINAL","",df)
df <- as_tibble(df)

# making the data more user friendly
scores <- c()
teams <- c()
for (i in 1:dim(df)[1]) {
    #uses regular expression, reference: https://rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf
    scores[i] <- regmatches(df[i,1], gregexpr("\\d+",df[i,1])) #get the connected numbers
    teams[i] <- regmatches(df[i,1],gregexpr("\\D+",df[i,1])) # get the strings of words
}
# this script fails above. extra numbers in both score and team we need to remove
#down below turns the list into a data frame
games_score <- as_tibble(do.call(rbind,scores))
games_team <- as_tibble(do.call(rbind,teams)) 

#make the colnames more user friendly
colnames(games_score) <- c("score 1","score 2")
colnames(games_team) <- c("team 1","team 2")

#combine teams and score 
games <- cbind(games_team,games_score) %>% as_tibble()





---------------------------------------------------------------------------------
#this is the same data Nicole found
# from http://web1.ncaa.org/stats/StatsSrv/rankings

WL_per <- read_csv("rankings.csv",skip = 11,n_max = 354)
WL_per <- WL_per[-c(351),]
WL_per$Rank <- c(seq(1,350),rep("NR",3))
View(WL_per)

Iscor_Offense <- read_csv("rankings.csv",skip = 372,n_max = 354,
                          col_types = cols(
                              Rank = col_character(), #bc we have some unranked teams
                              Name = col_character(),
                              GM = col_double(),
                              `W-L` = col_character(),
                              PTS = col_double(),
                              PPG = col_double()
                          ))
Iscor_Offense <- Iscor_Offense[-c(351),]
Iscor_Offense$Rank <- c(seq(1,350),rep("NR",3))
view(Iscor_Offense)
# PTS ~ cumulative points in season
# PPG ~ average points per game

Iscor_Defense <- 




