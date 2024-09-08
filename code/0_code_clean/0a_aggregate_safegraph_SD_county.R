#' description: "Aggregate Safegraph to county level"


#****************************************************************************************************************************************************

# Requires Safegraph access

# SafeGraph Physical Distancing Data

#aws s3 sync s3://sg-c19-response/social-distancing/v2/ ./myLocalDirectory/ --profile safegraphws --endpoint https://s3.wasabisys.com

#****************************************************************************************************************************************************


my_packages <- c("tidyverse","magrittr","lubridate","data.table","sf","bit64","tictoc")
sapply(my_packages, require, character.only = TRUE)
library(jsonlite)

safegraph_version <- "raw_SD_05mar21/2020"

#### SET PATHS #### 

datain = file.path(paste0("Code/clean_safegraph/",safegraph_version) ) # Not shared, proprietary data
dataout = file.path("data_replicate/1_data_intermediate")


first_month <- 1 # which month start
last_month <- 4 # which month end

m=1
d="01"


for (m in first_month:last_month) { 
  
  f <- list.files(file.path(datain, paste("0", m,sep=""))) # get all days of the month for which theres data
  
  for (d in f) { #d ="01"
   
    
    dat <- fread(file.path(datain, paste("0", m, "/", d, "/2020-0",m,"-",d,"-social-distancing.csv.gz",sep="")))
    
    #glimpse(dat)
    
    dat[, median_dwell_at_bucketed_distance_traveled := NULL]
    dat[, bucketed_distance_traveled := NULL]
    dat[, destination_cbgs := NULL]
    
    
    #transform CBG with correct number of digits
    dat[ , origin_census_block_group := as.character(origin_census_block_group)]
    
    unique(nchar(dat$origin_census_block_group))
    
    dat[nchar(origin_census_block_group) == 11, origin_census_block_group := paste0("0",origin_census_block_group)]
    dat[, fips_5 := str_sub(origin_census_block_group,1,5) ]
    
    unique(nchar(dat$fips_5))
    
    dat[, Devices_fips5 := sum(device_count, na.rm = T) , by = .(fips_5)]
    dat[, Devices_fips5 := as.numeric(Devices_fips5)]
    #check <- dat[is.na(device_count),]

    
    #------------- Construct average time away from home

    dat[ ,bucketed_away_from_home_time := str_remove_all(bucketed_away_from_home_time,"\"")]
    dat[ ,bucketed_away_from_home_time := str_remove_all(bucketed_away_from_home_time,"\\{")]
    dat[ ,bucketed_away_from_home_time := str_remove_all(bucketed_away_from_home_time,"\\}")]
    dat[ ,bucketed_away_from_home_time := str_replace_all(bucketed_away_from_home_time,"-","_")]
    
    gen_var <- function(df, pat) {
    
      df[ , paste0("v",pat) := as.numeric(
        str_remove(
          fifelse(str_detect(bucketed_away_from_home_time,pat),
                  str_extract(bucketed_away_from_home_time, paste0(pat,":\\d*")),"0"),
          ".*:")
        )
      ]
      
    }

    #---- Now clean all buckets and create variable of total time away from home
    
    possible_buckets <- c("<20","21_45","46_60","61_120","121_180","181_240","241_300","301_360","361_420","421_480","481_540",
                          "541_600","601_660","661_720","721_840","841_960","961_1080","1081_1200","1201_1320","1321_1440")
    
    for (v in possible_buckets) {
        gen_var(dat,v)  
    }
    
    #glimpse(dat)
    
    paste(names(dat), collapse = "+")
    
    dat[ , sum_devices_away_home := `v<20`+v21_45+v46_60+v61_120+v121_180+v181_240+v241_300+v301_360+
           v361_420+v421_480+v481_540+v541_600+v601_660+v661_720+
           v721_840+v841_960+v961_1080+v1081_1200+v1201_1320+v1321_1440]
    
    dat[ , sum_devices_away_home_fips5 := sum(sum_devices_away_home), by = .(fips_5)]
    

     
    # Find the midpoints
    vars_to_keep <- names(dat)[str_detect(names(dat), "^v")]   
    vars_to_keep <- str_replace_all(vars_to_keep, "_", "+")
    vars_to_keep <- str_remove_all(vars_to_keep, "<|v")
    
    vars_num <- rep(NA,length(vars_to_keep))
    for (i in 1:length(vars_to_keep)) {
      vars_num[i] <- eval(parse(text = vars_to_keep[i]))/2
    }
    
    dat[ , total_time_away_home := 10*`v<20` + 33*v21_45 + 53*v46_60 + 90.5*v61_120 + 150.5*v121_180 + 
           210.5*v181_240 + 270.5*v241_300 + 330.5*v301_360 +
           390.5*v361_420 + 450.5*v421_480 + 510.5*v481_540 + 570.5*v541_600 + 630.5*v601_660 + 690.5*v661_720 +
           780.5*v721_840 + 900.5*v841_960 + 1020.5*v961_1080 + 1140.5*v1081_1200 + 1260.5*v1201_1320 + 1380.5*v1321_1440]
    
    dat[ , check := total_time_away_home/sum_devices_away_home]
    
    # summary(dat$check)
    
    #Transform integers to doubles
    cols<- names(dat[, .SD, .SDcols = which(sapply(dat, is.integer))])
    dat[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
    
    dat_agg <- dat[ , .(SD_distHome = weighted.mean(distance_traveled_from_home,w = device_count, na.rm=T), #NEW APRIL 19, 2021
                        Devices_fips5 = mean(Devices_fips5),
                        SD_dwellHome = weighted.mean(median_home_dwell_time,w = device_count, na.rm=T), #NEW APRIL 19, 2021
                        SDnew_nonHomeDwell = weighted.mean(median_non_home_dwell_time,w = device_count, na.rm=T), #NEW APRIL 19, 2021
                        SDnew_medianPercHome = weighted.mean(median_percentage_time_home,w = device_count, na.rm=T), #NEW APRIL 19, 2021
                        SD_delivery = sum(delivery_behavior_devices / Devices_fips5,na.rm=T),
                        SD_allHomeCount = sum(completely_home_device_count,na.rm=T), 
                        SD_pctHome = sum(completely_home_device_count / Devices_fips5, na.rm=T),
                        SD_partTimers = sum(part_time_work_behavior_devices,na.rm=T),
                        SD_fullTimers = sum(full_time_work_behavior_devices,na.rm=T),
                        SD_fullTimersShare =  sum(full_time_work_behavior_devices / Devices_fips5,na.rm=T),
                        SD_partTimersShare =  sum(part_time_work_behavior_devices / Devices_fips5,na.rm=T),
                        SD_average_bucketed_time_away_from_home = sum(total_time_away_home/sum_devices_away_home_fips5, na.rm = T)
                        )
                    , by = .(fips_5)]
    

    dat_agg$date <- paste("2020-","0",m,"-",d,sep="")
    
    if ((m==first_month) & (d=="01")) { temp <- dat_agg} else { temp <- rbind(temp, dat_agg)}
        
        message("done for month ",m,", day  ",d)
        
  }
}

temp$date <- as.Date(temp$date, "%Y-%m-%d")

saveRDS(temp, file.path(dataout,"safeGraph_SD_county.rds"))

message("smooth exit")
