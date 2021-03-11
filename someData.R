library(dplyr)
library(rvest)

# Refer to: https://www.sports-reference.com/cbb/seasons/

# Just one year (2019) as a template for the years in loop below:
site <- "https://www.sports-reference.com/cbb/seasons/2019-school-stats.html"
site_selector <- site %>% read_html() %>% html_nodes("table") %>% html_table()

reduced_tbl <- site_selector[[1]] #%>% filter() get rid of rows acting as headers, drop na columns

stats2019 <- reduced_tbl



stats2019 <- stats2019[-c(seq(22,374,by = 22),seq(23,374,by = 22)),] #removed unnecessary rows

names(stats2019) <- paste(names(stats2019),stats2019[1,]) #concatenate the colnames with the first row 

stats2019 <- stats2019[-c(1),] #remove 1st row that contains all colnames

#remove NA columns
stats2019 <- stats2019[,!duplicated(colnames(stats2019))] #thought it would be easier to remove the bulk and then remove the remaining one
stats2019 <- stats2019[,-c(9)] 

View(stats2019)

# Years with NCAA March Madness Tournament and Have Stats not separated by conference
# There are many years on the website that have season stats but they are separated by conference into many tables
#years <- c(1993:2019)

#for (yr in years){
#    site <- paste0("https://www.sports-reference.com/cbb/seasons/", yr ,"-school-stats.html")
#    site_selector <- site %>% read_html() %>% html_nodes("table") %>% html_table()
    
#    reduced_tbl <- site_selector[[1]] #%>% filter() get rid of rows acting as headers
    
#    assign(paste0("stats",yr), reduced_tbl)
#}
