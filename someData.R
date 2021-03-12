library(dplyr)
library(readr)
library(rvest)

# Refer to: https://www.sports-reference.com/cbb/seasons/

# Years with NCAA March Madness Tournament and Have Stats not separated by conference
# There are many years on the website that have season stats but they are separated by conference into many tables
years <- c(1993:2021)

all_tbls <- c()

for (yr in years){
    site <- paste0("https://www.sports-reference.com/cbb/seasons/", yr ,"-school-stats.html")
    site_selector <- site %>% read_html() %>% html_nodes("table") %>% html_table()

    temp_tbl <- site_selector[[1]]
    
    names(temp_tbl) <- paste(names(temp_tbl), temp_tbl[1, ]) #concatenate the colnames with the first row
    temp_tbl <- temp_tbl[-c(1), ]   #remove 1st row that contains all colnames
    temp_tbl <- temp_tbl[!(temp_tbl$`Overall W-L%`=="Overall" | temp_tbl$`Overall W-L%`=="W-L%"), ] # remove rows acting as titles
    
    # remove NA columns
    temp_tbl <- temp_tbl[, !duplicated(colnames(temp_tbl))]
    temp_tbl <- temp_tbl[, - c(9)]
    
    temp_tbl$year <- yr
    
    temp_tbl$` School` <- gsub(pattern = "\\s+NCAA$", "", x = temp_tbl$` School`)
    
    all_tbls <- rbind(temp_tbl, all_tbls)
    assign(paste0("stats",yr), temp_tbl)
}

write.csv(all_tbls, file = "~/Desktop/SeasonStats1993-2021.csv", row.names = FALSE)