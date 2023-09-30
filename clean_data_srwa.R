#####################
## Load and clean Saipan Reed Warbler daily vocalization data
## 
## author: Willson Gaul  willson.gaul@gmail.com   & Ellie Roark
## created: 28 Oct 2022
## last modified: 8 July 2023
######################

if(!on_cloud) {
  srwa <- read.csv("../data/Reed_Warbler_detections_8July2023.csv")
  jie_srwa <- read_csv("../data/Reed_Warbler_detections_Jie_8July2023.csv", 
                       na = c("NA", "N/A", ""))
}
if(on_cloud) {
  srwa <- read.csv("Reed_Warbler_detections_8July2023.csv")
  jie_srwa <- read_csv("Reed_Warbler_detections_Jie_8July2023.csv", 
                       na = c("NA", "N/A", ""))
}


# drop blank rows
srwa <- srwa[which(!is.na(srwa$filename)), ]
srwa <- srwa[which(srwa$filename != ""), ]
jie_srwa <- jie_srwa[which(!is.na(jie_srwa$filename)), ]
jie_srwa <- jie_srwa[which(jie_srwa$filename != ""), ]


# drop rows that were not evaluated for SRWA b/c of noise or some other reason
srwa <- srwa[!is.na(srwa$SRWA), ]
jie_srwa <- jie_srwa[!is.na(jie_srwa$SRWA), ]
jie_srwa$observer <- "JL"

# join Ellie, Willson, and Jie's data
if(identical(colnames(srwa), colnames(jie_srwa))) {
  srwa <- bind_rows(srwa, jie_srwa)
} else stop("Jie and Ellie / Willson data do not have same columns")

# add metadata
md <- read.csv("../data/metadata_ReedWarbler_DailyVoc_8July2023.csv")
md$filename <- gsub(".wav", "", md$filename)
md_e_saipan <- read.csv("~/Documents/Saipan_ecology/data/audio_recordings/Saipan_ARU_from_Ellie/aru_file_METADATA.csv")
colnames(md_e_saipan)[colnames(md_e_saipan) == "loc_name"] <- "point_id"

# get different metadata dfs to have the same variables
md_e_saipan <- md_e_saipan[, -which(colnames(md_e_saipan) %in% 
                                      c("aru_id", "rec_start", "rec_end", 
                                        "date", "download_date"))]
md <- bind_rows(md, md_e_saipan)

# add metadata
srwa <- left_join(srwa, md, by = "filename")

rm(md)
rm(md_e_saipan)
rm(jie_srwa)

# Add the start time of song, which is hh:mm:ss since beginning of recording,
# to the time of day when the recording started.
srwa$rec_start_time <- srwa$filename
srwa$rec_start_time <- gsub("SWIFT.._", "", srwa$rec_start_time)
srwa$rec_start_time <- as.POSIXct(srwa$rec_start_time, 
                                  format = "%Y%m%d_%H%M%S")
srwa$minute_of_recording <- srwa$minute_of_hour
srwa$minute_of_recording[srwa$hour_of_recording == 1] <- 
  srwa$minute_of_recording[srwa$hour_of_recording == 1] + 60
# convert minute of recording to second of recording
srwa$second_of_recording <- srwa$minute_of_recording * 60

# get the time of day for each 1-minute segment
srwa$time_of_day <- srwa$rec_start_time + 
  srwa$second_of_recording
# drop date
srwa$time_of_day <- as_hms(srwa$time_of_day)

# remove sites with no SRWA detections
det_points <- data.frame(table(srwa$point_id, srwa$SRWA))
srwa <- srwa[-which(srwa$point_id %in% 
                      det_points$Var1[det_points$Var2 == TRUE & 
                                        det_points$Freq == 0]), ]

# make point_id a factor for gam fitting
srwa$point_id <- factor(as.character(srwa$point_id))

# make time of day seconds for gam fitting
srwa$time_of_day_sec <- as.numeric(srwa$time_of_day)

# add date
srwa$date_fac <- factor(as.character(date(srwa$rec_start_time)))
srwa$date_num <- as.numeric(date(srwa$rec_start_time))
srwa$doy <- yday(date(srwa$rec_start_time))
srwa$month <-  factor(as.character(month(date(srwa$rec_start_time))))
srwa$season <- NA
srwa$season[srwa$month %in% c(1, 2, 3)] <- "winter"
srwa$season[srwa$month %in% c(7, 8)] <- "summer"
srwa$season <- factor(as.character(srwa$season))

srwa$observer <- factor(as.character(srwa$observer))
srwa$rain_wind <- factor(as.character(
  grepl(".*rain.*|.*Rain.*|.*wind.*|.*Wind.*", srwa$notes)))

# remove duplicate rows
srwa <- unique(srwa)

# remove a row where there is a missing minute of hour value
srwa <- srwa[!is.na(srwa$minute_of_hour), ]

# make a variable for season



# Some locations and/or dates have more than 4 minutes of data per hour.
# Most locations and dates have 4 minutes of data per hour.
# This is because:
# As of 21 Aug 2022 we listened to only 4 minutes, per hour, based on 
# preliminary analysis comparing results from 4 minutes/hour to results from
# more (e.g. 5 or 10) minutes per hour.
# For now, we will keep all data, because uneven amounts of data from different
# locations should not be particularly problematic for our models.  We could
# change this if reviewers want later.  I do not expect rarefying data to 
# 4 minutes per hour to change conclusions.

## Rarefying to 4 minutes per hour just to be sure this does not change things
srwa_rarefied <- srwa
srwa_rarefied$hour <- hour(srwa$time_of_day)

flnms <- unique(srwa$filename) 
# if there are more than 8 minutes annotated from a file, rarefy the data to 
# have only 8 minutes (4 per hour)
for(i in 1:length(flnms)) {
  if(length(which(srwa_rarefied$filename == flnms[i])) > 8) {
    # find number of minutes from hour zero of this recording
    n0 <- length(which(
      srwa_rarefied$hour_of_recording[srwa_rarefied$filename == flnms[i]] == 0))
    # find number of minutes from hour one of this recording
    n1 <- length(which(
      srwa_rarefied$hour_of_recording[srwa_rarefied$filename == flnms[i]] == 1))
    
    # drop all but the last four minutes that were annotated for hour zero
    if(n0 > 4) {
      srwa_rarefied <- srwa_rarefied[-which(
        srwa_rarefied$filename == flnms[i] & 
          srwa_rarefied$hour_of_recording == 0)[1:(n0-4)], ]
    }
    # drop all but the last four minutes that were annotated for hour one
    if(n1 > 4) {
      srwa_rarefied <- srwa_rarefied[-which(
        srwa_rarefied$filename == flnms[i] & 
          srwa_rarefied$hour_of_recording == 1)[1:(n1-4)], ]
    }
  }
}
