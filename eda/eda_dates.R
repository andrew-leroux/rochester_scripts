rm(list=ls())

## load packages needed for reading and analyzing data
pckgs <- c("dplyr","readr","lubridate","mgcv","ggplot2","tidyr")
sapply(pckgs, function(x) if(!require(x,character.only=TRUE,quietly=TRUE)) {
        install.packages(x)
        require(x, character.only=TRUE)
})
rm(pckgs)



arm_date <- read_rds(path="/dcl01/smart/data/activity/Rochester/processed_data/arm_date.rds")
arm_date <- 
        arm_date %>% 
        mutate(baseline = as_date(baseline,format="%m/%d/%Y" ),
               mid = as_date(mid, format="%m/%d/%Y"),
               post = as_date(post, format="%m/%d/%Y"),
               "3month" = as_date(.$`3month`, format="%m/%d/%Y"),
               "6month" = as_date(.$`6month`, format="%m/%d/%Y"))


figure_path <- 