scenario <- as.numeric(commandArgs(trailingOnly=TRUE))
# scenario <- c(3, 1000)

## load packages needed for reading and analyzing data
pckgs <- c("dplyr","readr","data.table","lubridate","mgcv","readxl","stringr","tidyr")
sapply(pckgs, function(x) if(!require(x,character.only=TRUE,quietly=TRUE)) {
        install.packages(x)
        require(x, character.only=TRUE)
})
rm(pckgs)

if(!require(read.gt3x)){
        devtools::install_github("THLfi/read.gt3x")
        library("read.gt3x")
}
if(!require(SummarizedActigraphy)){
        remotes::install_github("muschellij2/SummarizedActigraphy")
        library("SummarizedActigraphy")
}


arm_date <- read_rds(path="/dcl01/smart/data/activity/Rochester/processed_data/arm_date.rds")
arm_date <- 
        arm_date %>% 
        mutate(baseline = as_date(baseline,format="%m/%d/%Y" ),
               mid = as_date(mid, format="%m/%d/%Y"),
               post = as_date(post, format="%m/%d/%Y"),
               "3month" = as_date(.$`3month`, format="%m/%d/%Y"),
               "6month" = as_date(.$`6month`, format="%m/%d/%Y"))


calculate_ai = function(df, epoch = "1 min") {
        sec_df = df %>% 
                mutate(
                        HEADER_TIME_STAMP = lubridate::floor_date(HEADER_TIME_STAMP, "1 sec")) %>% 
                group_by(HEADER_TIME_STAMP) %>% 
                summarise(
                        AI = sqrt((var(X) + var(Y) + var(Z))/3),
                )
        sec_df %>% mutate(
                HEADER_TIME_STAMP = lubridate::floor_date(HEADER_TIME_STAMP, epoch)) %>% 
                group_by(HEADER_TIME_STAMP) %>% 
                summarise(
                        AI = sum(AI)
                )
}

calculate_mad = function(df, epoch = "1 min") {
        df %>% 
                mutate(         
                        r = sqrt(X^2+Y^2+Z^2),
                        HEADER_TIME_STAMP = lubridate::floor_date(HEADER_TIME_STAMP, epoch)) %>% 
                group_by(HEADER_TIME_STAMP) %>% 
                summarise(
                        SD = sd(r),
                        MAD = mean(abs(r - mean(r))),
                        MEDAD = median(abs(r - mean(r)))
                )
}

calculate_measures = function(df, epoch = "1 min") {
        ai0 = calculate_ai(df, epoch = epoch)
        mad = calculate_mad(df, epoch = epoch)
        res = full_join(ai0, mad)
        res
}

check_zeros = function(df) {
        any(rowSums(df[, c("X", "Y", "Z")] == 0) == 3)
}
fix_zeros = function(df, fill_in = TRUE) {
        zero = rowSums(df[, c("X", "Y", "Z")] == 0) == 3
        names(zero) = NULL
        df$X[zero] = NA
        df$Y[zero] = NA
        df$Z[zero] = NA
        if (fill_in) {
                df$X = zoo::na.locf(df$X, na.rm = FALSE)
                df$Y = zoo::na.locf(df$Y, na.rm = FALSE)
                df$Z = zoo::na.locf(df$Z, na.rm = FALSE)
                
                df$X[ is.na(df$X)] = 0
                df$Y[ is.na(df$Y)] = 0
                df$Z[ is.na(df$Z)] = 0
        }
        df
}
data_path <- "/dcl01/smart/data/activity/Rochester/data"
accel_files <- list.files(data_path)
n_files <- length(accel_files)

inx_arr <- scenario[1]
n_arr   <- scenario[2]
inx_ls  <- split(1:n_files, rep(1:n_arr, ceiling(n_files/n_arr))[1:n_files])
# accel_files <- accel_files[inx_ls[[inx_arr]]]


## transform to 1440+ format
## for each of the various measures (AI, MAD, MED)
## data frame contains the followign columns:
##   ID = participant identifier
##   date = date of observation
##   phase = phase of the study
##   study_arm = which intervention arm this individual was assigned to
##   study_site = site of recruitment
##   device_location = location of the device (waist vs wrist)
##   device_type = type of accelerometer (e.g. GT3 vs GT3X vs GT9X)
##   device_id = device identifier
##   MIN1-MIN1440 = minute level measure 

## note that this doesnt account for "corrected" files or switcing of waist and wrist devices
for(i in inx_ls[[inx_arr]]){
        raw_data_i <- read_actigraphy(file=file.path(data_path, accel_files[i]), verbose=TRUE)
        ID_i    <- as.numeric(str_extract(accel_files[i], "[0-9]+"))
        
        ## get dates of (instructed) wear time
        dates_i <- filter(arm_date, ID == ID_i)
        baseline_dates_i <- as.character(seq(dates_i$baseline, dates_i$baseline+days(6), by="1 day"))
        mid_dates_i <- as.character(seq(dates_i$mid, dates_i$mid+days(6), by="1 day"))
        post_dates_i <- as.character(seq(dates_i$post, dates_i$post+days(6), by="1 day"))
        mo3_dates_i <- as.character(seq(dates_i$`3month`, dates_i$`3month`+days(6), by="1 day"))
        mo6_dates_i <- as.character(seq(dates_i$`6month`, dates_i$`6month`+days(6), by="1 day"))
        all_dates_i <- c(baseline_dates_i, mid_dates_i, post_dates_i,mo3_dates_i, mo6_dates_i)
        
        
        ## replace 0s with last observation carry forward
        ## this corresponds to what the actilife software does
        df_i <- fix_zeros(raw_data_i$data.out)
        ## filter to just those dates which the participant was supposed to be wearing the device
        ## identify which phase of the study each date corresponds to 
        df_i <- df_i %>% 
                rename(HEADER_TIME_STAMP = time) %>% 
                mutate(date = as.character(lubridate::floor_date(HEADER_TIME_STAMP,unit="1 day")),
                       ID = ID_i) %>% 
                filter(date %in% all_dates_i) %>% 
                mutate(phase = NA, 
                       phase = replace(phase, date %in% baseline_dates_i, "baseline"),
                       phase = replace(phase, date %in% mid_dates_i, "mid"),
                       phase = replace(phase, date %in% post_dates_i, "post"),
                       phase = replace(phase, date %in% mo3_dates_i, "3months"),
                       phase = replace(phase, date %in% mo6_dates_i, "6months")
                       ) %>% 
                dplyr::select(ID, HEADER_TIME_STAMP, date, phase, X, Y, Z) 
        
        ## calculate minute level measures (AI, MAD, MED, SD)
        df_measures_i <- calculate_measures(df_i)
        df_measures_i <- 
                df_measures_i %>% 
                mutate(ID = ID_i, 
                       date = as.character(lubridate::floor_date(HEADER_TIME_STAMP,unit="1 day")),
                       time = paste0("MIN",hour(HEADER_TIME_STAMP)*60 + minute(HEADER_TIME_STAMP)+1),
                       time = factor(time, levels=paste0("MIN",1:1440), labels=paste0("MIN",1:1440)),
                       phase = NA, 
                       phase = replace(phase, date %in% baseline_dates_i, "baseline"),
                       phase = replace(phase, date %in% mid_dates_i, "mid"),
                       phase = replace(phase, date %in% post_dates_i, "post"),
                       phase = replace(phase, date %in% mo3_dates_i, "3months"),
                       phase = replace(phase, date %in% mo6_dates_i, "6months")
                )
        ## add in demographic and device data
        df_measures_i <- 
                df_measures_i %>% 
                mutate("arm" = dates_i$ARM,
                       "site" = dates_i$site,
                       "sex" = as.vector(raw_data_i$header$Value[raw_data_i$header$Field =="Sex"]),
                       "age" = as.vector(raw_data_i$header$Value[raw_data_i$header$Field =="Age"]),
                       "location" = as.vector(raw_data_i$header$Value[raw_data_i$header$Field =="Limb"]),
                       "height" = as.vector(raw_data_i$header$Value[raw_data_i$header$Field =="Height"]),
                       "mass" = as.vector(raw_data_i$header$Value[raw_data_i$header$Field =="Mass"]),
                       "sample_rate" = as.vector(raw_data_i$header$Value[raw_data_i$header$Field =="Sample Rate"]),
                       "time_zone" = as.vector(raw_data_i$header$Value[raw_data_i$header$Field =="TimeZone"]),
                       "device_model" = as.vector(raw_data_i$header$Value[raw_data_i$header$Field =="Device Type"]),
                       "device_id" = as.vector(raw_data_i$header$Value[raw_data_i$header$Field =="Serial Number"]))
        
        ## started at 11:50
        ## transform to 1440+ format
        AI_mat_i <- 
                df_measures_i %>% 
                dplyr::select(-one_of(c("SD","MAD","MEDAD","HEADER_TIME_STAMP"))) %>% 
                pivot_wider(names_from=time, values_from=AI)
        
        MAD_mat_i <- 
                df_measures_i %>% 
                dplyr::select(-one_of(c("SD","AI","MEDAD","HEADER_TIME_STAMP"))) %>% 
                pivot_wider(names_from=time, values_from=MAD)
        
        MEDAD_mat_i <- 
                df_measures_i %>% 
                dplyr::select(-one_of(c("SD","MAD","AI","HEADER_TIME_STAMP"))) %>% 
                pivot_wider(names_from=time, values_from=MEDAD)
        
        SD_mat_i <- 
                df_measures_i %>% 
                dplyr::select(-one_of(c("AI","MAD","MEDAD","HEADER_TIME_STAMP"))) %>% 
                pivot_wider(names_from=time, values_from=SD)
        
        ## dynamically assign to the global environment
        assign(paste0("AI_",i), value=AI_mat_i)
        assign(paste0("MAD_",i), value=MAD_mat_i)
        assign(paste0("MEDAD_",i), value=MEDAD_mat_i)
        assign(paste0("SD_",i), value=SD_mat_i)
        
        
        ## print progress
        print(i)
        
        ## clean up workspace
        rm(raw_data_i, df_i, AI_mat_i, MAD_mat_i, MEDAD_mat_i, SD_mat_i, df_measures_i, 
           dates_i, all_dates_i, baseline_dates_i, mid_dates_i, post_dates_i, mo3_dates_i, mo6_dates_i, 
           ID_i)
        gc()
        
}

## save output
save(list=c(paste0("AI_",inx_ls[[inx_arr]]),
     paste0("MAD_",inx_ls[[inx_arr]]),
     paste0("MEDAD_",inx_ls[[inx_arr]]),
     paste0("SD_",inx_ls[[inx_arr]])), 
     file=paste0("/dcl01/smart/data/activity/Rochester/processed_data/accel_wide_arr_",inx_arr,".RData"))


