library(dplyr)
library(rvest)

# Refer to: https://www.sports-reference.com/cbb/seasons/

# Years with NCAA March Madness Tournament and Have Stats not separated by conference
# There are many years on the website that have season stats but they are separated by conference into many tables
years <- c(1993:2021)

for (yr in years){
    site <- paste0("https://www.sports-reference.com/cbb/seasons/", yr ,"-school-stats.html")
    site_selector <- site %>% read_html() %>% html_nodes("table") %>% html_table()

    temp_tbl <- site_selector[[1]]
    
    temp_tbl <- temp_tbl[-c(seq(22,374, by = 22), seq(23,374, by = 22)), ] #removed unnecessary rows
    names(temp_tbl) <- paste(names(temp_tbl), temp_tbl[1, ]) #concatenate the colnames with the first row
    temp_tbl <- temp_tbl[-c(1), ]   #remove 1st row that contains all colnames
    
    # remove NA columns
    temp_tbl <- temp_tbl[, !duplicated(colnames(temp_tbl))]
    temp_tbl <- temp_tbl[, - c(9)]
    
    assign(paste0("stats",yr), temp_tbl)
}
