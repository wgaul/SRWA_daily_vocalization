#####################
## Check student Saipan Reed Warbler data for trustworthiness
## 
## Compare multiple students' data
## 
## author: Willson Gaul  willson.gaul@gmail.com
## created: 6 March 2023
## last modified: 8 July 2023
######################

alex_dat <- read_csv("../data/Reed_Warbler_detections_Alex_21April2023.csv", 
                     na = c("NA", "N/A", ""))
jie_dat <- read_csv("../data/Reed_Warbler_detections_Jie_8July2023.csv", 
                    na = c("NA", "N/A", ""))

alex_dat <- alex_dat[!is.na(alex_dat$SRWA), ]
alex_dat$observer <- "AT"

jie_dat <- jie_dat[!is.na(jie_dat$SRWA), ]
jie_dat$observer <- "JL"

alex_dat$filename <- gsub(".wav|.WAV", "", alex_dat$filename)
jie_dat$filename <- gsub(".wav|.WAV", "", jie_dat$filename)

alex_dat$min_ID <- paste(alex_dat$filename, alex_dat$hour_of_recording, 
                         alex_dat$minute_of_hour, sep = "_")
jie_dat$min_ID <- paste(jie_dat$filename, jie_dat$hour_of_recording, 
                        jie_dat$minute_of_hour, sep = "_")

# subset to only minutes analyzed by both students
mins <- c(alex_dat$min_ID, jie_dat$min_ID)
mins <- data.frame(table(mins))
mins <- mins[mins$Freq >= 2, ]

alex_dat <- alex_dat[alex_dat$min_ID %in% mins$mins, ]
jie_dat <- jie_dat[jie_dat$min_ID %in% mins$mins, ]
jie_dat <- unique(jie_dat) # Jie duplicated a few minutes somehow

if(!identical(alex_dat$min_ID, jie_dat$min_ID)) stop("min_ID not yet identical.")

# join data from all students
combined_dat <- full_join(alex_dat, jie_dat)
combined_dat <- combined_dat[, -which(colnames(combined_dat) == "notes")]
combined_dat_wide <- pivot_wider(combined_dat, 
                                 names_from = observer, values_from = SRWA)

# figure out whether they agree for each observation
combined_dat_wide$agree <- combined_dat_wide$AT == combined_dat_wide$JL
table(combined_dat_wide$agree)


## calculate Krippendorf's Alpha
# preferred value for Krippendor's alpha is > 0.8
# make matrix
# each row is an observer, each column is a study (sampling event)
student_matrix <- matrix(c(as.character(combined_dat_wide$AT), 
                           as.character(combined_dat_wide$JL)), 
                         nrow = 2, byrow = TRUE)
krp <- kripp.alpha(student_matrix, method = "nominal")

# Number of required observations to estimate k's alpha given probability
# of detections (which is usually the less common class).  Note that this
# is about the number of observations, not the number of units (recordings),
# so double-coding 20 recordings gives 40 observations. 
# 
# On p 240 (or 323 for 3rd edition) of Krippendorff book, it says that if 
# acceptable alpha is 0.667 and 
# desired significance level is 0.1, then sample size needed is as follows:
# if probability of smallest class > 0.25, sample size needed to estimate K is 50
# if probability of smallest class > 0.2, sample size needed to estimate K is 60
# if probability of smallest class > 0.167, sample size needed is 70
# if probability of smallest class > 0.125, sample size needed is 84

length(which(alex_dat$SRWA == TRUE)) / nrow(alex_dat) # prob. of smallest class
length(which(jie_dat$SRWA == TRUE)) / nrow(jie_dat) # prob of smallest class
# it seems SRWA song prevalence is about 0.15, so sample size needed to
# estimate Krippendorf's alpha is around 84.  So I have enough.

krp # look at Krippendorf's alpha

combined_dat_wide[combined_dat_wide$agree == FALSE, ]

# on 21 April, I looked through recordings with Jie:
# SWIFT01_20230301_180103   hour 0 minute  44 (Jie correct I think)
# SWIFT01_20230301_160057  hour 1 minute  21 (Jie correct)
# SWIFT01_20230118_080010  hour 0 minute 5 (Jie correct)
# SWIFT01_20230118_060007   hour 1  minute 4 (Jie wrong)
# 12 SWIFT01_20230118_060007  hour 1  minute 49 (Jie wrong)
#  SWIFT01_20230118_060007  hour 0  minute 45  (difficult, maybe jie wrong)


