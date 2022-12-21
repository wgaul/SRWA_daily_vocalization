#####################
## Load and clean Saipan Reed Warbler daily vocalization data
## 
## author: Willson Gaul  willson.gaul@gmail.com   & Ellie Roark
## created: 28 Oct 2022
## last modified: 28 Oct 2022
######################

srwa <- read.csv("../data/ReedWarbler_detections_5Nov2022.csv")

# drop blank rows
srwa <- srwa[which(!is.na(srwa$filename)), ]
srwa <- srwa[which(srwa$filename != ""), ]

# drop rows that were not evaluated for SRWA b/c of noise or some other reason
srwa <- srwa[!is.na(srwa$SRWA), ]

# add metadata
md <- read.csv("../data/metadata_ReedWarbler_DailyVoc.csv")
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

srwa$observer <- factor(as.character(srwa$observer))
srwa$rain <- factor(as.character(
  grepl(".*rain.*|.*Rain.*", srwa$notes)))

# Some locations and/or dates have more than 4 minutes of data per hour.
# Most locations and dates have 4 minutes of data per hour.
# This is because:
# As of 21 Aug 2022 we listened to only 4 minutes, per hour, based on 
# preliminary analysis comparing results from 4 minutes/hour to results from
# more (e.g. 5 or 10) minutes per hour.
# For now, we will keep all data, because uneven amounts of data from different
# locations should not be particularly problematic for our models.  We could
# change this if reviewers want later.  I do not expect rarifying data to 
# 4 minutes per hour to change conclusions.