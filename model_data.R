#this is just Simeons code
library(tidyverse)
library(rvest)
library(readxl)


################################################################################
    
Excel_File <- c("March_Madness2021.xlsx")

Teams_in_Tournament <- read_excel(Excel_File, sheet = "TeamIndex",
                                  col_types = c("numeric", "text"))

TeamStats <- read_excel(Excel_File, sheet = "TeamStats",
                        col_types = c("numeric", "numeric", "text", "text", "text", 
                                      "numeric", "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric"))

kenPom <- read_excel(Excel_File, sheet = "KenPom",
                     col_types = "numeric")

Previous_Matchups <- read_excel(Excel_File, sheet = "PreviousGames",
                                col_types = c("numeric", "text", "numeric", "text", 
                                              "numeric", "numeric", "numeric"))
################################################################################
    
#simeon's Function   
    
    
# returns season stats and kenPom asjusted rankings:
team_stats <- function(team_index){
        
        team_2021 <- TeamStats %>% 
            filter(TeamIndex == team_index) %>%
            merge(kenPom, by = "TeamIndex")
        
        #print(team_2021)
        return(team_2021)
    }


#returns the average score difference between two teams or 0 if they haven't faced this year yet.
teams_played_this_year <- function(team1, team2){
    # Rows where team 1 and team 2 have faced
    
    # Team 1 is first team in the Row
    team1_is_t1 <- Previous_Matchups[which((Previous_Matchups$Team1Index == team1) & 
                                               (Previous_Matchups$Team2Index == team2)), ]
    # Team 1 is second team in the Row
    team1_is_t2 <- Previous_Matchups[which((Previous_Matchups$Team1Index == team2) & 
                                               (Previous_Matchups$Team2Index == team1)), ]
    
    #print(team1_is_t1)
    #print(team1_is_t2)
    # if they have faced already this year:
    if ((nrow(team1_is_t1) >= 1 | nrow(team1_is_t2) >= 1)){
        # print("-----FACED EACH OTHER EARLIER IN THE SEASON-----")
        num_matchups = nrow(team1_is_t1) + nrow(team1_is_t2)
        
        avg_ScoreDiff = (sum(team1_is_t1$ScoreDiff) - sum(team1_is_t2$ScoreDiff)) / num_matchups
        
        return(avg_ScoreDiff)
        
    } else {
        
        return(0) # No Score differences because they haven't faced this year.
    }
}
    

#takes in the two teams in a given matchup and returns the winner.    
predict_winner <- function(team1, team2){
    t1 <- team_stats(team1)
    t2 <- team_stats(team2)
    
    # 0 if teams haven't faced, + if t1 scored more points on avg., - if t2 scored more on average
    AvgScoreDiff <- teams_played_this_year(team1, team2)
    
    # How many points a team scores/gives up on average + difference fro prev. matchups
    PredictedScoreDiff = (t1$PPG.For + AvgScoreDiff - t1$PPG.Against) - (t2$PPG.For - t2$PPG.Against)
    
    # If predicted scorediff is +, t1 is predicted to win, - is t2, 0 would be tie needing another else if ()
    if (PredictedScoreDiff > 0) {
        
        return(team1)
        
    } else {
        return(team2)
        
    }
}

predictedScoreDiff <- function(team1, team2) {
    t1 <- team_stats(team1)
    t2 <- team_stats(team2)
    
    # 0 if teams haven't faced, + if t1 scored more points on avg., - if t2 scored more on average
    AvgScoreDiff <- teams_played_this_year(team1, team2)
    
    # How many points a team scores/gives up on average + difference fro prev. matchups
    PredictedScoreDiff =abs((t1$PPG.For + AvgScoreDiff - t1$PPG.Against) - (t2$PPG.For - t2$PPG.Against))
    return(PredictedScoreDiff)
}    
    

#will print the results (from above) in a meaningful way.
print_results <- function(winner, loser){
    win = TeamStats %>% filter(TeamIndex == winner)
    lose = TeamStats %>% filter(TeamIndex == loser)
    
    print(paste0(win$Seed," ", win$School, " (", win$Region, ") beats ", 
                 lose$Seed, " ", lose$School, " (", lose$Region, ") by ", round(predictedScoreDiff(winner,loser), 2), " points.",
                 collapse = "\n"))
    
    return(NULL)
}    
    
    
# The following functions take in bracket format and moves winners along.
# replace predicted Winner with result from model above: compare_teams()
types <- c("numeric", "numeric", "numeric", "text", "numeric", "numeric")

FirstRound <- read_xlsx(Excel_File, sheet = "FirstRound", col_types = types) # Type in Winners from FirstFour
SecondRound <- read_xlsx(Excel_File, sheet = "SecondRound", col_types = types)
Sweet16 <- read_xlsx(Excel_File, sheet = "Sweet16", col_types = types)
Elite8 <- read_xlsx(Excel_File, sheet = "Elite8", col_types = types)
FinalFour <- read_xlsx(Excel_File, sheet = "FinalFour", col_types = types)
Championship <- read_xlsx(Excel_File, sheet = "Championship", col_types = types)

# head(FirstRound)



#helps build the model, using the predict_winner() function to update the 
#PredictedWinner column and the team indices for following rounds. 
#It also calls upon the print_result() function to print the predictions.
bracket_builder <- function(CurrentRound, NextRound){
    
    for (game in 1:nrow(CurrentRound)){
        winner <- predict_winner(as.list(CurrentRound[game, "Team1Index"]), as.list(CurrentRound[game, "Team2Index"]))
        
        CurrentRound[game, "PredictedWinner"] <- winner
        
        if(CurrentRound[game, "Team1Index"] == winner){
            print_results(winner, as.list(CurrentRound[game, "Team2Index"]))
            
        }else{
            print_results(winner, as.list(CurrentRound[game, "Team1Index"]))
        }
    }
    
    for (i in 1:nrow(NextRound)){
        NextRound[i, "Team1Index"] <- CurrentRound %>% 
            filter(NextGameIndex == as.list(NextRound[i, "GameIndex"])) %>% 
            slice_head(n=1) %>% select(PredictedWinner)
        
        NextRound[i, "Team2Index"] <- CurrentRound %>% 
            filter(NextGameIndex == as.list(NextRound[i, "GameIndex"])) %>% 
            slice_tail(n=1) %>% select(PredictedWinner)
        
    }
    
    return(NextRound)
}

