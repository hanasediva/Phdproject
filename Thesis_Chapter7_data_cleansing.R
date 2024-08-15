# PhD thesis of Hana Sediva, December 2023
# data loading, merging and cleanup of 47 files from 37 participants
#######################################################################

#install packages
install.packages("tidyverse") # data management package
install.packages("dplyr") # data management package
install.packages("janitor") #to fix column names
install.packages("zoo") #to fix column names
library("tidyverse")
library("dplyr")
library("janitor")
library("zoo")


####################################################################
# Read and cleanse baseline survey data
# ****** Baseline survey *********************************************
#read file
rawBaseline <- read_csv("C:/R_code/study4/survey/Survey_preStudy.csv")

#check the file was loaded                        
head(rawBaseline)

#create a copy, to keep raw file always intact
dfBaseline <- rawBaseline


#delete first 17 columns containing mainly metadata and not the needed
#intervention data
dfBaseline = subset(dfBaseline, select = -c(1:17) )

#delete PAR-Q part of the baseline file
dfBaseline = subset(dfBaseline, select = -c(Q55_1, Q55_2, Q55_3, Q55_4, Q55_5,
                                            Q55_6, Q55_7, Q56, Q57) )

#delete first two rows containing Qualtrics metadata
dfBaseline <- dfBaseline[-(1:2),]

#fix pid for participant who entered wrong pId number
dfBaseline$Q7_1[dfBaseline$Q7_1 == 67731477] <- 67731447

#fix age for 91667394 participant
dfBaseline$Q16_1[dfBaseline$Q16_1 == 55175] <- 55

#fix ethnicities where 2 participants added comments
dfBaseline$Q24[dfBaseline$Q24_17_TEXT == "Eritrean"] <- "Black African"
dfBaseline$Q24[dfBaseline$Q24_17_TEXT == "Latinoamericano"] <- "Latin American"

#drop other_ethnicities column after assigning these to the ethnicity column
dfBaseline = select(dfBaseline, -c("Q24_17_TEXT"))

#remove special characters from income field
dfBaseline$Q27 <- gsub('Â','', dfBaseline$Q27)

#rename fields, add prefix to all fields
dfBaseline <- rename_with(dfBaseline, 
                          ~ paste0("survey_pre_", .x, recycle0 = TRUE))


#rename pId, to make sure this field never has a prefix.  Same goes for wave, 
#and primaryKey which are all used to merge files
dfBaseline <- dfBaseline %>% 
  rename("pId" = "survey_pre_Q7_1")
#dfBaseline


#create new variable for wave.Baseline has wave value of 0.
#intervention day1 has wave value of 1, etc. 
#post-intervention survey and exit survey have wave of 15.
dfBaseline <- mutate(dfBaseline, wave = 0)


#create a primary key (pId + wave)
dfBaseline <- mutate(dfBaseline, primaryKey = paste0(pId,"_",wave))

#remove one participant created test entry
dfBaseline <- subset(dfBaseline, dfBaseline$pId != 11111)


########################################################################
# ****** post-study survey *****************************************
#read the file
rawPostStudy <- read_csv("C:/R_code/study4/survey/Survey_postStudy.csv")

#head(rawPostStudy)

#create a copy of the data frame
dfPost <- rawPostStudy


#delete first 17 columns containing mainly metadata and not the needed
#intervention data
dfPost <- dfPost[ -c(1:17) ]

#delete first two rows containing Qualtrics metadata
dfPost <- dfPost[-(1:2),]

#fix ethnicities
dfPost$Q24[dfPost$Q24_17_TEXT == "Eritrean"] <- "Black African"

#drop other_ethnicities column after assigning these to the ethnicity column
dfPost = select(dfPost, -c("Q24_17_TEXT"))

#clean up extra characters in the income field
dfPost$Q27 <- gsub('Â','', dfPost$Q27)

#rename fields, add prefix
dfPost <- rename_with(dfPost, ~ paste0("survey_post_", .x, recycle0 = TRUE))

#rename pId field
dfPost <- dfPost %>% 
  rename("pId" = "survey_post_Q7_1")


#create new variable for wave.Baseline has wave value of 0.
#intervention day1 has wave value of 1, etc. 
#post-intervention survey has wave of 15.
dfPost <- mutate(dfPost, wave = 15)

#create a primary key (pId + wave)
dfPost <- mutate(dfPost, primaryKey = paste0(pId,"_",wave))


#merge baseline and post-intervention files
dfMerge1 <- full_join(dfBaseline, dfPost, by = join_by(pId, wave, primaryKey))

#check data
dfMerge

#write the output to a file
write.csv(dfMerge1, file = "C:/R_code/study4/survey2_pre_post_merged_aug13_1.csv ", 
          row.names = FALSE)

###############################################################################
# **** Exit survey *****
exitCols <- c("Q45_1", "Q2", "Q3", "Q4", "Q43", "Q5", "Q6", "Q7", "Q8", "Q9", 
              "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", "Q17", "Q18",
              "Q19", "Q50", "Q20", "Q44", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", 
              "Q27","Q28", "Q29", "Q49", "Q51", "Q52", "Q53", "Q30", "Q31", "Q32",
              "Q33", "Q34", "Q35", "Q36", "Q37", "Q48", "Q38", "Q39",
              "Q40", "Q41", "Q46", "Q47", "Q42", "Q55", "Q54"
)                  

rawExit <- read_csv("C:/R_code/study4/survey/Survey_exit.csv",
                    col_select = all_of(exitCols))
head(rawExit)

dfExit <- rawExit

#rename fields, add prefix
dfExit <- rename_with(dfExit, 
                      ~ paste0("survey_exit_", .x, recycle0 = TRUE))

dfExit <- dfExit %>% 
  rename("pId" = "survey_exit_Q45_1")

#create new variable for wave.Baseline has wave value of 0.
#intervention day1 has wave value of 1, etc. 
#post-intervention survey has wave of 15.
dfExit <- mutate(dfExit, wave = 15)

#create a primary key (pId + wave)
dfExit <- mutate(dfExit, primaryKey = paste0(pId,"_",wave))

#delete first two rows containing Qualtrics metadata
dfExit <- dfExit[-(1:2),]

#remove one participant created test entry
dfExit <- subset(dfExit, dfExit$pId != 555)


#merge exit survey with baseline and post survey
dfMerge2 <- full_join(dfMerge1, dfExit, by = join_by(pId, wave, primaryKey))

write.csv(dfMerge2, file = "C:/R_code/study4/merged_pre_post_exit_Aug13_1.csv", 
          row.names = FALSE)



# *******read Garmin sleep data, in one file ******
# ************************************************************************

sleepCols <- c("Mobile Code", "Calendar Date", "Start Time UTC", "Total Sleep Minutes", "Deep Sleep Minutes", "Light Sleep Minutes", "REM Sleep Minutes", "Awake Minutes", "Validation Code")                  

rawSleep <- read_csv("C:/R_code/study4/garminSleep/garminSleep.csv",
                     col_select = all_of(sleepCols))
head(rawSleep)

#rename fields
dfSleep <- rawSleep %>% 
  rename("pId" = "Mobile Code",
         "GarminSleepDate" = "Calendar Date",
         "GarminSleepStart" = "Start Time UTC",
         "GarminSleepTotalMins" = "Total Sleep Minutes",
         "GarminSleepDeepMins" = "Deep Sleep Minutes",
         "GarminSleepLightMins" = "Light Sleep Minutes",
         "GarminSleepREMMins" = "REM Sleep Minutes",
         "GarminSleepAwakeMins" = "Awake Minutes",
         "GarminSleepValidationCode" = "Validation Code"
  )
#summarise data by pId and sleepdate, which has several times stamps under 
#under garminsleepstart field of each night. We want to pick only one timestamp
#each night.
dfSleep <- dfSleep %>%
  group_by(pId, GarminSleepDate) %>%
  summarise(pId, GarminSleepDate, GarminSleepStart, GarminSleepTotalMins, GarminSleepDeepMins, GarminSleepLightMins, GarminSleepREMMins, GarminSleepAwakeMins)


#use the latest sleep date captured each night by Garmin
dfSleep <- dfSleep %>% 
  group_by(pId, GarminSleepDate) %>% 
  slice(which.max(GarminSleepStart))


#initialise baselinestart, week1start, and week2start dates
dfSleep <- mutate(dfSleep, baselineStart = ymd("2023-01-01"))
dfSleep <- mutate(dfSleep, week1Start = ymd("2023-01-01"))
dfSleep <- mutate(dfSleep, week2Start = ymd("2023-01-01"))

#set study dates for each participant
#group1 8 
dfSleep$baselineStart[dfSleep$pId == 69185659 | dfSleep$pId == 37950503 |
                        dfSleep$pId == 87076103 | dfSleep$pId == 95485425 |
                        dfSleep$pId == 19436405 | dfSleep$pId == 49842571 |
                        dfSleep$pId == 22721164 | dfSleep$pId == 17526894] <- ymd("2023-05-22")

dfSleep$week1Start[dfSleep$pId == 69185659 | dfSleep$pId == 37950503 |
                     dfSleep$pId == 87076103 | dfSleep$pId == 95485425 |
                     dfSleep$pId == 19436405 | dfSleep$pId == 49842571 |
                     dfSleep$pId == 22721164 | dfSleep$pId == 17526894] <- ymd("2023-05-29")

dfSleep$week2Start[dfSleep$pId == 69185659 | dfSleep$pId == 37950503 |
                     dfSleep$pId == 87076103 | dfSleep$pId == 95485425 |
                     dfSleep$pId == 19436405 | dfSleep$pId == 49842571 |
                     dfSleep$pId == 22721164 | dfSleep$pId == 17526894] <- ymd("2023-06-05")

#group2  
dfSleep$baselineStart[dfSleep$pId == 11779098 | dfSleep$pId == 64217955 | 
                        dfSleep$pId == 84814374 | dfSleep$pId == 85297032 |
                        dfSleep$pId == 41968397 | dfSleep$pId == 19563674 |
                        dfSleep$pId == 55478635 | dfSleep$pId == 35821427 |
                        dfSleep$pId == 12981890 | dfSleep$pId == 47829255 |
                        dfSleep$pId == 19663473 | dfSleep$pId == 15431169 |
                        dfSleep$pId == 75089435] <- ymd("2023-05-29")

dfSleep$week1Start[dfSleep$pId == 11779098 | dfSleep$pId == 64217955 | 
                     dfSleep$pId == 84814374 | dfSleep$pId == 85297032 |
                     dfSleep$pId == 41968397 | dfSleep$pId == 19563674 |
                     dfSleep$pId == 55478635 | dfSleep$pId == 35821427 |
                     dfSleep$pId == 12981890 | dfSleep$pId == 47829255 |
                     dfSleep$pId == 19663473 | dfSleep$pId == 15431169 |
                     dfSleep$pId == 75089435] <- ymd("2023-06-05")

dfSleep$week2Start[dfSleep$pId == 11779098 | dfSleep$pId == 64217955 | 
                     dfSleep$pId == 84814374 | dfSleep$pId == 85297032 |
                     dfSleep$pId == 41968397 | dfSleep$pId == 19563674 |
                     dfSleep$pId == 55478635 | dfSleep$pId == 35821427 |
                     dfSleep$pId == 12981890 | dfSleep$pId == 47829255 |
                     dfSleep$pId == 19663473 | dfSleep$pId == 15431169 |
                     dfSleep$pId == 75089435] <- ymd("2023-06-12")

#group3 
dfSleep$baselineStart[dfSleep$pId == 49241918 | dfSleep$pId == 50522107 |
                        dfSleep$pId == 52080478 | dfSleep$pId == 67731447  |
                        dfSleep$pId == 28025920] <- ymd("2023-06-05")
dfSleep$week1Start[dfSleep$pId == 49241918 | dfSleep$pId == 50522107 |
                     dfSleep$pId == 52080478 | dfSleep$pId == 67731447  |
                     dfSleep$pId == 28025920] <- ymd("2023-06-12")
dfSleep$week2Start[dfSleep$pId == 49241918 | dfSleep$pId == 50522107 |
                     dfSleep$pId == 52080478 | dfSleep$pId == 67731447  |
                     dfSleep$pId == 28025920] <- ymd("2023-06-19")

#group4 3 
dfSleep$baselineStart[dfSleep$pId == 20703294 | dfSleep$pId == 39385673 |
                        dfSleep$pId == 39722835] <- ymd("2023-06-12")
dfSleep$week1Start[dfSleep$pId == 20703294 | dfSleep$pId == 39385673 |
                     dfSleep$pId == 39722835] <- ymd("2023-06-19")
dfSleep$week2Start[dfSleep$pId == 20703294 | dfSleep$pId == 39385673 |
                     dfSleep$pId == 39722835] <- ymd("2023-06-26")

#group5 
dfSleep$baselineStart[dfSleep$pId == 85347320] <- ymd("2023-07-03")
dfSleep$week1Start[dfSleep$pId == 85347320] <- ymd("2023-07-10")
dfSleep$week2Start[dfSleep$pId == 85347320] <- ymd("2023-07-17")


#calculating waves
dfSleep <- mutate(dfSleep, GarminDayMonthYear = 
                    format(as.POSIXct(GarminSleepStart,
                                      format = '%m/%d/%Y %H:%M:%S'),
                           format = '%Y/%m/%d'))

#initialise wave to -1
dfSleep <- mutate(dfSleep, wave = -1)
#set all baseline dates to wave 0
dfSleep$wave[dfSleep$GarminDayMonthYear >= dfSleep$baselineStart & 
               dfSleep$GarminDayMonthYear <= dfSleep$baselineStart+6] <- 0
#set first week intervention day 1 to wave 1
dfSleep$wave[dfSleep$GarminDayMonthYear == dfSleep$week1Start] <- 1
#set first week intervention day 2 to wave 2
dfSleep$wave[dfSleep$GarminDayMonthYear == (dfSleep$week1Start+1)] <- 2
#set first week intervention day 3 to wave 3
dfSleep$wave[dfSleep$GarminDayMonthYear == (dfSleep$week1Start+2)] <- 3
#set first week intervention day 4 to wave 4
dfSleep$wave[dfSleep$GarminDayMonthYear == (dfSleep$week1Start +3)] <- 4
#set first week intervention day 5 to wave 5
dfSleep$wave[dfSleep$GarminDayMonthYear == (dfSleep$week1Start+ 4)] <- 5
#set first week intervention day 6 to wave 6
dfSleep$wave[dfSleep$GarminDayMonthYear == (dfSleep$week1Start+5)] <- 6
#set first week intervention day 7 to wave 7
dfSleep$wave[dfSleep$GarminDayMonthYear == (dfSleep$week1Start + 6)] <- 7
#set second week intervention day 1 to wave 8
dfSleep$wave[dfSleep$GarminDayMonthYear == dfSleep$week2Start] <- 8
#set second week intervention day 2 to wave 9
dfSleep$wave[dfSleep$GarminDayMonthYear == (dfSleep$week2Start+1)] <- 9
#set second week intervention day 3 to wave 10
dfSleep$wave[dfSleep$GarminDayMonthYear == (dfSleep$week2Start+2)] <- 10
#set second week intervention day 4 to wave 11
dfSleep$wave[dfSleep$GarminDayMonthYear == (dfSleep$week2Start+3)] <- 11
#set second week intervention day 5 to wave 12
dfSleep$wave[dfSleep$GarminDayMonthYear == (dfSleep$week2Start+4)] <- 12
#set second week intervention day 6 to wave 13
dfSleep$wave[dfSleep$GarminDayMonthYear == (dfSleep$week2Start+5)] <- 13
#set second week intervention day 7 to wave 14
dfSleep$wave[dfSleep$GarminDayMonthYear == (dfSleep$week2Start+6)] <- 14

#remove records outside of the study period
dfSleep <- subset(dfSleep, wave != -1)

#create primaryKey for each row
dfSleep <- mutate(dfSleep, primaryKey = paste0(pId,"_",wave))


#average all measures for the baseline week
df2Sleep <- dfSleep %>%
  group_by(pId, wave, primaryKey) %>%
  summarise(GarminDayMoGarminSleep_DaysInWave0 = sum(wave == 0, na.rm = TRUE),
            GarminSleep_TotalMins = ifelse(wave == 0, mean(GarminSleepTotalMins, na.rm = TRUE), GarminSleepTotalMins),
            GarminSleep_DeepMins = ifelse(wave == 0, mean(GarminSleepDeepMins, na.rm = TRUE), GarminSleepDeepMins),
            GarminSleep_LightMins = ifelse(wave == 0, mean(GarminSleepLightMins, na.rm = TRUE), GarminSleepLightMins),
            GarminSleep_REMMins = ifelse(wave == 0, mean(GarminSleepREMMins, na.rm = TRUE), GarminSleepREMMins),
            GarminSleep_AwakeMins = ifelse(wave == 0, mean(GarminSleepAwakeMins, na.rm = TRUE), GarminSleepAwakeMins))



#prepare final output, summarise all baseline rows into one
dfSleepFinal <- df2Sleep %>%
  distinct(wave, pId, primaryKey,.keep_all=TRUE)


dfMerge4 <- full_join(dfMerge3, dfSleepFinal, 
                      by = join_by(pId, wave, primaryKey))
#write all exercise data to a file
write.csv(dfMerge4, 
          file = "C:/R_code/study4/PrePostExitGarminExerciseSleepMerged_Aug13_1.csv",
          row.names = FALSE)

## ***********************EMA data cleansing *************************************

# **** EMA morning surveys ****
emaCols <- c("mbl_cod", "local_time", "MORNING_SLEEP", "ENERGY_LEVEL_YESNO", 
             "ENERGY_LEVEL", "ALCOHOL_CONSUMPTION", "ALCOHOL_AMOUNT",
             "FRUIT_VEG_GOAL", "PORTIONS_OF_VEGETABLES","STEP_GOAL...15",
             "WALKING_CHALLENGE","RUNNING_CHALLENGE","CYCLING_CHALLENGE",
             "STEPS_CHALLENGE","OTHER_CHALLENGE","EARLY_MORNING_PLAN",
             "LATE_MORNING_PLAN","AFTERNOON_PLAN","EVENING_PLAN","MORNING_SURVEY_END",
             "has_data")                  

rawEMA <- read_csv("C:/R_code/study4/EMA/EMA_morning_7984000.csv",
                   col_select = all_of(emaCols))
head(rawEMA)

#rename fields
dfEMA <- rawEMA %>% 
  rename("pId" = "mbl_cod",
         "EMAdate" = "local_time",
         "EMAsleepQuality" = "MORNING_SLEEP",
         "EMAlowEnergyLevel" = "ENERGY_LEVEL_YESNO",
         "EMEenergyLevelRating" = "ENERGY_LEVEL",
         "EMAhadAlcoholLastNight" = "ALCOHOL_CONSUMPTION",
         "EMAamountAlcoholLastNight" = "ALCOHOL_AMOUNT",
         "EMAfruitPortionsGoal" = "FRUIT_VEG_GOAL",
         "EMAvegPortionsGoal" = "PORTIONS_OF_VEGETABLES",
         "EMAstepsGoal" = "STEP_GOAL...15", #the data is mixed, only extract non NA
         "EMAwalkingChallenge" = "WALKING_CHALLENGE",
         "EMArunningChallenge" = "RUNNING_CHALLENGE",
         "EMAcyclingChallenge" = "CYCLING_CHALLENGE",
         "EMAstepsChallenge" = "STEPS_CHALLENGE",
         "EMAotherChallenge" = "OTHER_CHALLENGE",
         "EMAmorningExercisePlan" = "EARLY_MORNING_PLAN",
         "EMAlateMorningExersisePlan" = "LATE_MORNING_PLAN",
         "EMAafternoonExersisePlan" = "AFTERNOON_PLAN",
         "EMAeveningExersisePlan" = "EVENING_PLAN",
         "EMAmorningSurveyCompleted"= "MORNING_SURVEY_END",
         "EMAmorningAnswered" = "has_data"
  )


#initialise week1start, and week2start dates
dfEMA <- mutate(dfEMA, week1Start = ymd("2023-01-01"))
dfEMA <- mutate(dfEMA, week2Start = ymd("2023-01-01"))


dfEMA$week1Start[dfEMA$pId == 69185659 | dfEMA$pId == 37950503 |
                   dfEMA$pId == 87076103 | dfEMA$pId == 95485425 |
                   dfEMA$pId == 19436405 | dfEMA$pId == 49842571 |
                   dfEMA$pId == 22721164 | dfEMA$pId == 17526894] <- ymd("2023-05-29")

dfEMA$week2Start[dfEMA$pId == 69185659 | dfEMA$pId == 37950503 |
                   dfEMA$pId == 87076103 | dfEMA$pId == 95485425 |
                   dfEMA$pId == 19436405 | dfEMA$pId == 49842571 |
                   dfEMA$pId == 22721164 | dfEMA$pId == 17526894] <- ymd("2023-06-05")


dfEMA$week1Start[dfEMA$pId == 11779098 | dfEMA$pId == 64217955 | 
                   dfEMA$pId == 84814374 | dfEMA$pId == 85297032 |
                   dfEMA$pId == 41968397 | dfEMA$pId == 19563674 |
                   dfEMA$pId == 55478635 | dfEMA$pId == 35821427 |
                   dfEMA$pId == 12981890 | dfEMA$pId == 47829255 |
                   dfEMA$pId == 19663473 | dfEMA$pId == 15431169 |
                   dfEMA$pId == 75089435] <- ymd("2023-06-05")

dfEMA$week2Start[dfEMA$pId == 11779098 | dfEMA$pId == 64217955 | 
                   dfEMA$pId == 84814374 | dfEMA$pId == 85297032 |
                   dfEMA$pId == 41968397 | dfEMA$pId == 19563674 |
                   dfEMA$pId == 55478635 | dfEMA$pId == 35821427 |
                   dfEMA$pId == 12981890 | dfEMA$pId == 47829255 |
                   dfEMA$pId == 19663473 | dfEMA$pId == 15431169 |
                   dfEMA$pId == 75089435] <- ymd("2023-06-12")

#group3 
dfEMA$week1Start[dfEMA$pId == 49241918 | dfEMA$pId == 50522107 |
                   dfEMA$pId == 52080478 | dfEMA$pId == 67731447  |
                   dfEMA$pId == 28025920] <- ymd("2023-06-12")
dfEMA$week2Start[dfEMA$pId == 49241918 | dfEMA$pId == 50522107 |
                   dfEMA$pId == 52080478 | dfEMA$pId == 67731447  |
                   dfEMA$pId == 28025920] <- ymd("2023-06-19")

#group4 
dfEMA$week1Start[dfEMA$pId == 20703294 | dfEMA$pId == 39385673 |
                   dfEMA$pId == 39722835] <- ymd("2023-06-19")
dfEMA$week2Start[dfEMA$pId == 20703294 | dfEMA$pId == 39385673 |
                   dfEMA$pId == 39722835] <- ymd("2023-06-26")

#group5 
dfEMA$week1Start[dfEMA$pId == 85347320] <- ymd("2023-07-10")
dfEMA$week2Start[dfEMA$pId == 85347320] <- ymd("2023-07-17")


#calculating waves
dfEMA <- mutate(dfEMA, EMADayMonthYear = 
                  format(as.POSIXct(EMAdate,
                                    format = '%m/%d/%Y %H:%M:%S'),
                         format = '%Y/%m/%d'))

#initialise wave to -1
dfEMA <- mutate(dfEMA, wave = -1)
#set first week intervention day 1 to wave 1
dfEMA$wave[dfEMA$EMADayMonthYear == dfEMA$week1Start] <- 1
#set first week intervention day 2 to wave 2
dfEMA$wave[dfEMA$EMADayMonthYear == (dfEMA$week1Start+1)] <- 2
#set first week intervention day 3 to wave 3
dfEMA$wave[dfEMA$EMADayMonthYear == (dfEMA$week1Start+2)] <- 3
#set first week intervention day 4 to wave 4
dfEMA$wave[dfEMA$EMADayMonthYear == (dfEMA$week1Start +3)] <- 4
#set first week intervention day 5 to wave 5
dfEMA$wave[dfEMA$EMADayMonthYear == (dfEMA$week1Start+ 4)] <- 5
#set first week intervention day 6 to wave 6
dfEMA$wave[dfEMA$EMADayMonthYear == (dfEMA$week1Start+5)] <- 6
#set first week intervention day 7 to wave 7
dfEMA$wave[dfEMA$EMADayMonthYear == (dfEMA$week1Start + 6)] <- 7
#set second week intervention day 1 to wave 8
dfEMA$wave[dfEMA$EMADayMonthYear == dfEMA$week2Start] <- 8
#set second week intervention day 2 to wave 9
dfEMA$wave[dfEMA$EMADayMonthYear == (dfEMA$week2Start+1)] <- 9
#set second week intervention day 3 to wave 10
dfEMA$wave[dfEMA$EMADayMonthYear == (dfEMA$week2Start+2)] <- 10
#set second week intervention day 4 to wave 11
dfEMA$wave[dfEMA$EMADayMonthYear == (dfEMA$week2Start+3)] <- 11
#set second week intervention day 5 to wave 12
dfEMA$wave[dfEMA$EMADayMonthYear == (dfEMA$week2Start+4)] <- 12
#set second week intervention day 6 to wave 13
dfEMA$wave[dfEMA$EMADayMonthYear == (dfEMA$week2Start+5)] <- 13
#set second week intervention day 7 to wave 14
dfEMA$wave[dfEMA$EMADayMonthYear == (dfEMA$week2Start+6)] <- 14

#remove records outside of the study period
dfEMA <- subset(dfEMA, wave != -1)

#create primaryKey for each row
dfEMA <- mutate(dfEMA, primaryKey = paste0(pId,"_",wave))

#use the latest EMA date captured each time, in case there is an error and more
# than 1 survey is saved.  This should not happen, but it has happened.
df2EMA <- dfEMA %>% 
  group_by(primaryKey) %>% 
  slice(which.max(EMAdate))

df2EMA <- mutate(df2EMA, ema_steps_goal_number = as.numeric(gsub("\\D", "", EMAstepsGoal)))

#create a new variable to identify whether the participant set any challenges each day
#create a new variable to identify if they set goal to exercise in the morning, 
#late morning, afternoon, evening.
#create a new variable to identify if they completed the survey each day
df3EMA <- df2EMA %>%
  group_by(pId, wave) %>%
  summarise(primaryKey, EMADayMonthYear, EMAsleepQuality, EMAlowEnergyLevel, EMEenergyLevelRating,
            EMAhadAlcoholLastNight, EMAamountAlcoholLastNight, EMAfruitPortionsGoal,
            EMAvegPortionsGoal, ema_steps_goal_number, EMAstepsGoal = ifelse(!is.na(ema_steps_goal_number), 1, 0),
            EMAexerciseChallengeSet = ifelse(!is.na(EMAwalkingChallenge) |
                                               !is.na(EMArunningChallenge) |
                                               !is.na(EMAcyclingChallenge) |
                                               !is.na(EMAstepsChallenge) |
                                               !is.na(EMAotherChallenge), 1, 0),
            EMAmorningExercisePlan = ifelse(!is.na(EMAmorningExercisePlan), 1, 0),
            EMAlateMorningExersisePlan = ifelse(!is.na(EMAlateMorningExersisePlan), 1, 0),
            EMAafternoonExersisePlan = ifelse(!is.na(EMAafternoonExersisePlan), 1, 0),
            EMAeveningExersisePlan = ifelse(!is.na(EMAeveningExersisePlan), 1, 0),
            EMAmorningSurveyCompleted, 
            EMAmorningAnswered = ifelse(is.na(EMAmorningAnswered), 0, 1))


dfEMAmorningSurveyFinal <- subset(df3EMA, EMAmorningAnswered == TRUE)


dfMerge5 <- full_join(dfMerge4, dfEMAmorningSurveyFinal, 
                      by = join_by(pId, wave, primaryKey))
#write all exercise data to a file
write.csv(dfMerge5, 
          file = "C:/R_code/study4/AllPlusMorningEMAMerged_Sept2.csv",
          row.names = FALSE)




#prepare daily totals for all outcome variables
dfMergeTotalsEMA <- dfMerge12 %>%
  group_by(pId) %>%
  summarise(wave, primaryKey, EMA_daily_total_Fruit = ifelse(wave >= 1 & wave <= 14, 
                                                             + ifelse(is.na(EMAbreakfastFruitPortions), 0,
                                                             na.locf0(EMAbreakfastFruitPortions, fromLast = FALSE), 
                                                             EMAbreakfastFruitPortions)

                                                             + ifelse(is.na(EMAlunchFruitPortions), 0,
                                                                      na.locf0(EMAlunchFruitPortions, fromLast = FALSE), 
                                                                      EMAlunchFruitPortions)

                                                             + ifelse(is.na(EMAdinnerFruitPortions), 0,
                                                                      na.locf0(EMAdinnerFruitPortions, fromLast = FALSE), 
                                                                      EMAdinnerFruitPortions)

                                                             + ifelse(is.na(EMAeveningBreakfastFruitMissed), 0, EMAeveningBreakfastFruitMissed - EMAbreakfastFruitPortions)
                                                             + ifelse(is.na(EMAeveningLunchFruitMissed), 0, EMAeveningLunchFruitMissed - EMAlunchFruitPortions)
                                                             , 0),
            
            EMA_daily_total_Veg = ifelse(wave >= 1 & wave <= 14, 
                                         + ifelse(is.na(EMAbreakfastVegPortions), 0,
                                                  na.locf0(EMAbreakfastVegPortions, fromLast = FALSE), 
                                                  EMAbreakfastVegPortions)

                                         + ifelse(is.na(EMAlunchVegPortions), 0,
                                                  na.locf0(EMAlunchVegPortions, fromLast = FALSE), 
                                                  EMAlunchVegPortions)

                                         + ifelse(is.na(EMAdinnerVegPortions),0, 
                                                  na.locf0(EMAdinnerVegPortions, fromLast = FALSE), 
                                                  EMAdinnerVegPortions)

                                         + ifelse(is.na(EMAeveningBreakfastVegMissed), 
                                                  0, EMAeveningBreakfastVegMissed - EMAbreakfastVegPortions)

                                         + ifelse(is.na(EMAeveningLunchVegMissed), 0, EMAeveningLunchVegMissed - EMAlunchVegPortions)
                                         
                                         , 0),
            
            EMA_daily_total_meals_Skipped = ifelse(wave >= 1 & wave <= 14, 
                                                   + ifelse(is.na(EMAhadBreakfast),
                                                            0, 
                                                            ifelse(EMAhadBreakfast == 2, 1, 0))
                                                  
                                                   + ifelse(is.na(EMAhadLunch), 0, ifelse(EMAhadLunch == 2, 1, 0))

                                                   + ifelse(is.na(EMAhadDinner), 0, ifelse(EMAhadDinner == 2, 1, 0))
 
                                                   + ifelse(is.na(EMAeveningHadBreakfastMissed), 0, ifelse(EMAeveningHadBreakfastMissed == 2, 1, 0))
                                                   + ifelse(is.na(EMAeveningHadLunchMissed), 0, ifelse(EMAeveningHadLunchMissed == 2, 1, 0))
                                                   
                                                   , 0),
            
            EMA_daily_total_snacks = ifelse(wave >= 1 & wave <= 14, 
                                            ifelse(is.na(EMAdailySnacksPortions), 0,
                                                   na.locf0(EMAdailySnacksPortions, fromLast = FALSE), 
                                                   EMAdailySnacksPortions)
                                            ,0),
            
            EMA_daily_CaffeineCups = ifelse(wave >= 1 & wave <= 14, 
                                            ifelse(is.na(EMAdailyCaffeineCups), 0,
                                                   na.locf0(EMAdailyCaffeineCups, fromLast = FALSE), 
                                                   EMAdailyCaffeineCups)
                                            ,0),
            EMA_daily_WaterGlasses = ifelse(wave >= 1 & wave <= 14, 
                                            ifelse(is.na(EMAdailyWaterGlasses), 0,
                                                   na.locf0(EMAdailyWaterGlasses, fromLast = FALSE), 
                                                   EMAdailyWaterGlasses)
                                            ,0),
            
            EMA_daily_total_alcohol = ifelse(wave >= 1 & wave <= 14, 
                                             ifelse(is.na(EMAamountAlcoholLastNight), 0,
                                                    na.locf0(EMAamountAlcoholLastNight, fromLast = FALSE), 
                                                    EMAamountAlcoholLastNight)
                                             + ifelse(is.na(EMAeveningAlcoholUnitsMissed), 0, EMAeveningAlcoholUnitsMissed)
                                             
                                             ,0),
            
            EMA_daily_total_education_shown = ifelse(wave >= 1 & wave <= 14,
                                                     + ifelse(is.na(EMAbreakfastEducationRead), 0, EMAbreakfastEducationRead) 
                                                     +  ifelse(is.na(EMAlunchEducationRead), 0, EMAlunchEducationRead) 
                                                     +  ifelse(is.na(EMAafternoonEducationRead), 0, EMAafternoonEducationRead) 
                                                     +  ifelse(is.na(EMAeveningEducationRead), 0, EMAeveningEducationRead) 
                                                     
                                                     , 0),
            
            EMA_daily_total_education_Read =  ifelse(wave >= 1 & wave <= 14, 
                                                     + ifelse(is.na(EMAedDietReadTotal), 0, EMAedDietReadTotal) 
                                                     + ifelse(is.na(EMAedExerciseReadTotal), 0, EMAedExerciseReadTotal) 
                                                     + ifelse(is.na(EMAedMenoReadTotal), 0, EMAedMenoReadTotal), 0),
            
            EMA_daily_total_Surveys_Answered = ifelse(wave >= 1 & wave <= 14, 
                                                      +  ifelse(is.na(EMAmorningAnswered), 0, 1) 
                                                      + ifelse(is.na(EMAlateMorningAnswered), 0, 1)
                                                      + ifelse(is.na(EMAearlyAfternoonAnswered), 0, 1)  
                                                      + ifelse(is.na(EMAlateAfternoonAnswered), 0, 1)  
                                                      + ifelse(is.na(EMAeveningAnswered), 0, 1), 0)
  )



#final merge and write to a file
dfMergeFinal <- full_join(dfMerge12, dfMergeTotalsEMA, 
                          by = join_by(pId, wave, primaryKey))

write.csv(dfMergeFinal, 
          file = "C:/R_code/study4/Allstudy4merged_complete_Sept11.csv", 
          row.names = FALSE)

saveRDS(dfMergeFinal, file = "C:/R_code/study4/study4_Sept11.rds")


# ***********************************************************************************
#continue with additional cleaning data after merging

#fix column names
dfStudy4_cleaning <- clean_names(dfMergeFinal)

#fix ema column names
dfStudy4_cleaning <- dfStudy4_cleaning %>% 
  rename_with(.cols = 517, ~"ema_daily_total_caffeine")

dfStudy4_cleaning <- dfStudy4_cleaning %>% 
  rename_with(.cols = 518, ~"ema_daily_total_water")

colnames(dfStudy4_cleaning)<-gsub("em_a", "ema_",colnames(dfStudy4_cleaning))
colnames(dfStudy4_cleaning)<-gsub("em_e", "ema_",colnames(dfStudy4_cleaning))




# *******************************************************************************************
#change data types for baseline survey data
dfStudy4_cleaning$survey_pre_q16_1 <- as.double(dfStudy4_cleaning$survey_pre_q16_1)
dfStudy4_cleaning$survey_pre_q17_1 <- as.double(dfStudy4_cleaning$survey_pre_q17_1)
dfStudy4_cleaning$survey_pre_q18_1 <- as.double(dfStudy4_cleaning$survey_pre_q18_1)
dfStudy4_cleaning$survey_pre_q43_1 <- as.double(dfStudy4_cleaning$survey_pre_q43_1)
dfStudy4_cleaning$survey_pre_q44_1 <- as.double(dfStudy4_cleaning$survey_pre_q44_1)
dfStudy4_cleaning$survey_pre_q45_1 <- as.double(dfStudy4_cleaning$survey_pre_q45_1)
dfStudy4_cleaning$survey_pre_q46_1 <- as.double(dfStudy4_cleaning$survey_pre_q46_1)
dfStudy4_cleaning$survey_pre_q47_1 <- as.double(dfStudy4_cleaning$survey_pre_q47_1)
dfStudy4_cleaning$survey_pre_q48_2 <- as.double(dfStudy4_cleaning$survey_pre_q48_2)
dfStudy4_cleaning$survey_pre_q49_1 <- as.double(dfStudy4_cleaning$survey_pre_q49_1)
dfStudy4_cleaning$survey_pre_q50_1 <- as.double(dfStudy4_cleaning$survey_pre_q50_1)

#change data types for post survey data
dfStudy4_cleaning$survey_post_q16_1 <- as.double(dfStudy4_cleaning$survey_post_q16_1)
dfStudy4_cleaning$survey_post_q17_1 <- as.double(dfStudy4_cleaning$survey_post_q17_1)
dfStudy4_cleaning$survey_post_q18_1 <- as.double(dfStudy4_cleaning$survey_post_q18_1)
dfStudy4_cleaning$survey_post_q43_1 <- as.double(dfStudy4_cleaning$survey_post_q43_1)
dfStudy4_cleaning$survey_post_q44_1 <- as.double(dfStudy4_cleaning$survey_post_q44_1)
dfStudy4_cleaning$survey_post_q45_1 <- as.double(dfStudy4_cleaning$survey_post_q45_1)
dfStudy4_cleaning$survey_post_q46_1 <- as.double(dfStudy4_cleaning$survey_post_q46_1)
dfStudy4_cleaning$survey_post_q47_1 <- as.double(dfStudy4_cleaning$survey_post_q47_1)
dfStudy4_cleaning$survey_post_q48_2 <- as.double(dfStudy4_cleaning$survey_post_q48_2)
dfStudy4_cleaning$survey_post_q49_1 <- as.double(dfStudy4_cleaning$survey_post_q49_1)
dfStudy4_cleaning$survey_post_q50_1 <- as.double(dfStudy4_cleaning$survey_post_q50_1)



#fix exit survey data scale, for some reason this seemed to have changed, unsure by whom
dfStudy4_cleaning$survey_exit_q7[dfStudy4_cleaning$survey_exit_q7 == "None at all"] <- "No effort at all"
dfStudy4_cleaning$survey_exit_q7[dfStudy4_cleaning$survey_exit_q7 == "A little effot" | dfStudy4_cleaning$survey_exit_q7 == "A little"] <- "A little effort"
dfStudy4_cleaning$survey_exit_q7[dfStudy4_cleaning$survey_exit_q7 == "A moderate amount"] <- "No opinion"
dfStudy4_cleaning$survey_exit_q7[dfStudy4_cleaning$survey_exit_q7 == "A lot"] <- "A lot of effort"
dfStudy4_cleaning$survey_exit_q7[dfStudy4_cleaning$survey_exit_q7 == "A great deal"] <- "Huge effort"

dfStudy4_cleaning$survey_exit_q8[dfStudy4_cleaning$survey_exit_q8 == "None at all"] <- "No effort at all"
dfStudy4_cleaning$survey_exit_q8[dfStudy4_cleaning$survey_exit_q8 == "A little effot" | dfStudy4_cleaning$survey_exit_q8 == "A little"] <- "A little effort"
dfStudy4_cleaning$survey_exit_q8[dfStudy4_cleaning$survey_exit_q8 == "A moderate amount"] <- "No opinion"
dfStudy4_cleaning$survey_exit_q8[dfStudy4_cleaning$survey_exit_q8 == "A lot"] <- "A lot of effort"
dfStudy4_cleaning$survey_exit_q8[dfStudy4_cleaning$survey_exit_q8 == "A great deal"] <- "Huge effort"

dfStudy4_cleaning$survey_exit_q9[dfStudy4_cleaning$survey_exit_q9 == "None at all"] <- "No effort at all"
dfStudy4_cleaning$survey_exit_q9[dfStudy4_cleaning$survey_exit_q9 == "A little effot" | dfStudy4_cleaning$survey_exit_q9 == "A little"] <- "A little effort"
dfStudy4_cleaning$survey_exit_q9[dfStudy4_cleaning$survey_exit_q9 == "A moderate amount"] <- "No opinion"
dfStudy4_cleaning$survey_exit_q9[dfStudy4_cleaning$survey_exit_q9 == "A lot"] <- "A lot of effort"
dfStudy4_cleaning$survey_exit_q9[dfStudy4_cleaning$survey_exit_q9 == "A great deal"] <- "Huge effort"

dfStudy4_cleaning$survey_exit_q10[dfStudy4_cleaning$survey_exit_q10 == "None at all"] <- "No effort at all"
dfStudy4_cleaning$survey_exit_q10[dfStudy4_cleaning$survey_exit_q10 == "A little effot" | dfStudy4_cleaning$survey_exit_q10 == "A little"] <- "A little effort"
dfStudy4_cleaning$survey_exit_q10[dfStudy4_cleaning$survey_exit_q10 == "A moderate amount"] <- "No opinion"
dfStudy4_cleaning$survey_exit_q10[dfStudy4_cleaning$survey_exit_q10 == "A lot"] <- "A lot of effort"
dfStudy4_cleaning$survey_exit_q10[dfStudy4_cleaning$survey_exit_q10 == "A great deal"] <- "Huge effort"

dfStudy4_cleaning$survey_exit_q11[dfStudy4_cleaning$survey_exit_q11 == "None at all"] <- "No effort at all"
dfStudy4_cleaning$survey_exit_q11[dfStudy4_cleaning$survey_exit_q11 == "A little effot" | dfStudy4_cleaning$survey_exit_q11 == "A little"] <- "A little effort"
dfStudy4_cleaning$survey_exit_q11[dfStudy4_cleaning$survey_exit_q11 == "A moderate amount"] <- "No opinion"
dfStudy4_cleaning$survey_exit_q11[dfStudy4_cleaning$survey_exit_q11 == "A lot"] <- "A lot of effort"
dfStudy4_cleaning$survey_exit_q11[dfStudy4_cleaning$survey_exit_q11 == "A great deal"] <- "Huge effort"

dfStudy4_cleaning$survey_exit_q12[dfStudy4_cleaning$survey_exit_q12 == "None at all"] <- "No effort at all"
dfStudy4_cleaning$survey_exit_q12[dfStudy4_cleaning$survey_exit_q12 == "A little effot" | dfStudy4_cleaning$survey_exit_q12 == "A little"] <- "A little effort"
dfStudy4_cleaning$survey_exit_q12[dfStudy4_cleaning$survey_exit_q12 == "A moderate amount"] <- "No opinion"
dfStudy4_cleaning$survey_exit_q12[dfStudy4_cleaning$survey_exit_q12 == "A lot"] <- "A lot of effort"
dfStudy4_cleaning$survey_exit_q12[dfStudy4_cleaning$survey_exit_q12 == "A great deal"] <- "Huge effort"


dfStudy4_cleaning$survey_exit_q13[dfStudy4_cleaning$survey_exit_q13 == "None at all"] <- "No effort at all"
dfStudy4_cleaning$survey_exit_q13[dfStudy4_cleaning$survey_exit_q13 == "A little effot" | dfStudy4_cleaning$survey_exit_q13 == "A little"] <- "A little effort"
dfStudy4_cleaning$survey_exit_q13[dfStudy4_cleaning$survey_exit_q13 == "A moderate amount"] <- "No opinion"
dfStudy4_cleaning$survey_exit_q13[dfStudy4_cleaning$survey_exit_q13 == "A lot"] <- "A lot of effort"
dfStudy4_cleaning$survey_exit_q13[dfStudy4_cleaning$survey_exit_q13 == "A great deal"] <- "Huge effort"

dfStudy4_cleaning$survey_exit_q14[dfStudy4_cleaning$survey_exit_q14 == "None at all"] <- "No effort at all"
dfStudy4_cleaning$survey_exit_q14[dfStudy4_cleaning$survey_exit_q14 == "A little effot" | dfStudy4_cleaning$survey_exit_q14 == "A little"] <- "A little effort"
dfStudy4_cleaning$survey_exit_q14[dfStudy4_cleaning$survey_exit_q14 == "A moderate amount"] <- "No opinion"
dfStudy4_cleaning$survey_exit_q14[dfStudy4_cleaning$survey_exit_q14 == "A lot"] <- "A lot of effort"
dfStudy4_cleaning$survey_exit_q14[dfStudy4_cleaning$survey_exit_q14 == "A great deal"] <- "Huge effort"

dfStudy4_cleaning$survey_exit_q15[dfStudy4_cleaning$survey_exit_q15 == "None at all"] <- "No effort at all"
dfStudy4_cleaning$survey_exit_q15[dfStudy4_cleaning$survey_exit_q15 == "A little effot" | dfStudy4_cleaning$survey_exit_q15 == "A little"] <- "A little effort"
dfStudy4_cleaning$survey_exit_q15[dfStudy4_cleaning$survey_exit_q15 == "A moderate amount"] <- "No opinion"
dfStudy4_cleaning$survey_exit_q15[dfStudy4_cleaning$survey_exit_q15 == "A lot"] <- "A lot of effort"
dfStudy4_cleaning$survey_exit_q15[dfStudy4_cleaning$survey_exit_q15 == "A great deal"] <- "Huge effort"

dfStudy4_cleaning$survey_exit_q16[dfStudy4_cleaning$survey_exit_q16 == "None at all"] <- "No effort at all"
dfStudy4_cleaning$survey_exit_q16[dfStudy4_cleaning$survey_exit_q16 == "A little effot" | dfStudy4_cleaning$survey_exit_q16 == "A little"] <- "A little effort"
dfStudy4_cleaning$survey_exit_q16[dfStudy4_cleaning$survey_exit_q16 == "A moderate amount"] <- "No opinion"
dfStudy4_cleaning$survey_exit_q16[dfStudy4_cleaning$survey_exit_q16 == "A lot"] <- "A lot of effort"
dfStudy4_cleaning$survey_exit_q16[dfStudy4_cleaning$survey_exit_q16 == "A great deal"] <- "Huge effort"




#Compute daily totals
#create new daily diet totals variables

#total fruit
dfStudy4_cleaning_fruit <- dfStudy4_cleaning %>% 
  group_by(p_id) %>%
  arrange(wave) %>%
  mutate(daily_total_fruit = ifelse(wave == 0 & !is.na(survey_pre_q43_1), survey_pre_q43_1, 
                                    ifelse(wave == 0 & is.na(survey_pre_q43_1), 0,
                                           ifelse(wave == 1 & is.na(ema_daily_total_fruit), 
                                                  survey_post_q43_1, ema_daily_total_fruit)))
  ) %>%
  ungroup()

dfStudy4_cleaning <- dfStudy4_cleaning_fruit %>% 
  group_by(p_id) %>%
  arrange(wave) %>%
  mutate(daily_total_fruit =  ifelse(wave >= 1 & wave <= 14 & is.na(daily_total_fruit),0,
                                     daily_total_fruit))%>%
  
  ungroup()


#vegetables
dfStudy4_cleaning_veg <- dfStudy4_cleaning %>% 
  group_by(p_id) %>%
  arrange(wave) %>%
  mutate(daily_total_veg = ifelse(wave == 0 & !is.na(survey_pre_q44_1), survey_pre_q44_1, 
                                  ifelse(wave == 0 & is.na(survey_pre_q44_1), 0,
                                         ifelse(wave == 1 & is.na(ema_daily_total_veg), 
                                                survey_pre_q44_1, ema_daily_total_veg)))
  ) %>%
  ungroup()

dfStudy4_cleaning <- dfStudy4_cleaning_veg %>% 
  group_by(p_id) %>%
  arrange(wave) %>%
  mutate(daily_total_veg =  ifelse(wave >= 1 & wave <= 14 & is.na(daily_total_veg),0,
                                   daily_total_veg))%>%
  
  ungroup()



#total caffeine cups
dfStudy4_cleaning_coffee <- dfStudy4_cleaning %>% 
  group_by(p_id) %>%
  arrange(wave) %>%
  mutate(daily_total_caffeine = ifelse(wave == 0 & !is.na(survey_pre_q45_1), survey_pre_q45_1, 
                                       ifelse(wave == 0 & is.na(survey_pre_q45_1), 0,
                                              ifelse(wave == 1 & is.na(ema_daily_caffeine_cups), 
                                                     survey_pre_q45_1, ema_daily_caffeine_cups)))
  ) %>%
  ungroup()

dfStudy4_cleaning <- dfStudy4_cleaning_coffee %>% 
  group_by(p_id) %>%
  arrange(wave) %>%
  mutate(daily_total_caffeine =  ifelse(wave >= 1 & wave <= 14 & is.na(daily_total_caffeine),0,
                                        daily_total_caffeine))%>%
  
  ungroup()



#total water glasses
dfStudy4_cleaning_water <- dfStudy4_cleaning %>% 
  group_by(p_id) %>%
  arrange(wave) %>%
  mutate(daily_total_water = ifelse(wave == 0 & !is.na(survey_pre_q46_1), survey_pre_q46_1, 
                                    ifelse(wave == 0 & is.na(survey_pre_q46_1), 0,
                                           ifelse(wave == 1 & is.na(ema_daily_water_glasses), 
                                                  survey_pre_q46_1, ema_daily_water_glasses)))
  ) %>%
  ungroup()

dfStudy4_cleaning <- dfStudy4_cleaning_water %>% 
  group_by(p_id) %>%
  arrange(wave) %>%
  mutate(daily_total_water =  ifelse(wave >= 1 & wave <= 14 & is.na(daily_total_water),0,
                                     daily_total_water))%>%
  
  ungroup()

#alcohol intake
dfStudy4_cleaning_alcohol <- dfStudy4_cleaning %>% 
  group_by(p_id) %>%
  arrange(wave) %>%
  mutate(daily_total_alcohol = ifelse(wave == 0 & survey_pre_q47_1 > 0 & !is.na(survey_pre_q47_1), 
                                      (survey_pre_q47_1 * survey_pre_q48_2)/7, 
                                      ifelse(wave == 0 & is.na(survey_pre_q47_1), 0,
                                             ifelse(wave == 1 & is.na(ema_daily_total_alcohol), 
                                                    (survey_pre_q47_1 * survey_pre_q48_2)/7, 
                                                    ema_daily_total_alcohol)))
  ) %>%
  ungroup()

dfStudy4_cleaning <- dfStudy4_cleaning_alcohol %>% 
  group_by(p_id) %>%
  arrange(wave) %>%
  mutate(daily_total_alcohol =  ifelse(wave >= 1 & wave <= 14 & is.na(daily_total_alcohol),0,
                                       daily_total_alcohol))%>%
  
  ungroup()


#total snacks consumed
dfStudy4_cleaning_snacks <- dfStudy4_cleaning %>% 
  group_by(p_id) %>%
  arrange(wave) %>%
  mutate(daily_total_snacks = ifelse(wave == 0 & !is.na(survey_pre_q50_1), survey_pre_q50_1, 
                                     ifelse(wave == 0 & is.na(survey_pre_q50_1), 0,
                                            ifelse(wave == 1 & is.na(ema_daily_total_snacks), 
                                                   survey_pre_q50_1, ema_daily_total_snacks)))
  ) %>%
  ungroup()

dfStudy4_cleaning <- dfStudy4_cleaning_snacks %>% 
  group_by(p_id) %>%
  arrange(wave) %>%
  mutate(daily_total_snacks =  ifelse(wave >= 1 & wave <= 14 & is.na(daily_total_snacks),0,
                                      daily_total_snacks))%>%
  
  ungroup()

#meals skipped in the pre/post survey will become meals consumed, so 3 - skipped.
dfStudy4_cleaning_meals <- dfStudy4_cleaning %>% 
  group_by(p_id) %>%
  arrange(wave) %>%
  mutate(daily_total_meals = ifelse(wave == 0 & !is.na(survey_pre_q49_1), survey_pre_q49_1, 
                                    ifelse(wave == 0 & is.na(survey_pre_q49_1), 0,
                                           ifelse(wave == 1 & is.na(ema_daily_total_meals_skipped), 
                                                  survey_pre_q50_1, 3-ema_daily_total_meals_skipped)))
  ) %>%
  ungroup()

dfStudy4_cleaning <- dfStudy4_cleaning_meals %>% 
  group_by(p_id) %>%
  arrange(wave) %>%
  mutate(daily_total_meals =  ifelse(wave >= 1 & wave <= 14 & is.na(daily_total_meals),0,
                                     daily_total_meals))%>%
  
  ungroup()

# **********************************************************
#create daily goal setting summary variable (this shows total number of goals set)
dfStudy4_cleaning <- dfStudy4_cleaning %>%   
  mutate(daily_total_goals_set = ifelse(wave >= 1 & wave <=14,
                                        ifelse(is.na(ema_fruit_portions_goal) | ema_fruit_portions_goal == 0, 0, 1) 
                                        + ifelse(is.na(ema_veg_portions_goal)| ema_veg_portions_goal == 0, 0, 1)
                                        + ifelse(is.na(ema_steps_goal)| ema_steps_goal == 0, 0, 1)  
                                        + ifelse(is.na(ema_garmin_connect_challenge_participation)| ema_garmin_connect_challenge_participation == 0, 0, 1)  
                                        + (ifelse(is.na(ema_morning_exercise_plan)| ema_morning_exercise_plan == 0, 0, 1)  |
                                             ifelse(is.na(ema_late_morning_exersise_plan)| ema_late_morning_exersise_plan == 0, 0, 1) | 
                                             ifelse(is.na(ema_afternoon_exersise_plan)| ema_afternoon_exersise_plan == 0, 0, 1) |  
                                             ifelse(is.na(ema_evening_exersise_plan)| ema_evening_exersise_plan == 0, 0, 1))
                                        , 0))



dfStudy4_cleaning <- mutate(dfStudy4_cleaning, daily_total_goals_fct = 
                              factor(daily_total_goals_set, 
                                     labels = c("0 goals", "1 goal", "2 goals", "3 goals", "4 goals", "5 goals")))

#create a new variable for any diet goal setting, binary
dfStudy4_cleaning <- mutate(dfStudy4_cleaning, 
                            daily_fruit_goals_set_binary = 
                              ifelse(is.na(ema_fruit_portions_goal) , 
                                     0, 1))

dfStudy4_cleaning <- mutate(dfStudy4_cleaning, 
                            daily_veg_goals_set_binary = 
                              ifelse(is.na(ema_veg_portions_goal), 
                                     0, 1))




# ****************************************************************************************
#baseline survey data, creating factors to use in ML
#create a factor for location
dfStudy4_cleaning <- mutate(dfStudy4_cleaning, survey_pre_location_fct = 
                              factor(survey_pre_q1, 
                                     labels =  c(# "East Midlands",
                                       "East of England",
                                       "London",
                                       #"NA",
                                       #"North East England", 
                                       #"Northern Ireland",
                                       
                                       "North West England",
                                       "Scotland",
                                       "South East England",
                                       "South West England",
                                       
                                       "Wales",
                                       "West Midlands",
                                       "Yorkshire and the Humber"
                                     )))

count(dfStudy4_cleaning, survey_pre_q1, survey_pre_location_fct)


#create a factor for marital status
dfStudy4_cleaning <- mutate(dfStudy4_cleaning, survey_pre_marital_status_fct = 
                              factor(survey_pre_q21, 
                                     labels =  c("Divorced, separated, widowed",
                                                 "Married, living as married",
                                                 "Never married",
                                                 "Prefer not to say")))

count(dfStudy4_cleaning, survey_pre_q21, survey_pre_marital_status_fct)


#create a factor for num of children
dfStudy4_cleaning <- mutate(dfStudy4_cleaning, survey_pre_num_children_fct = 
                              factor(survey_pre_q22, 
                                     labels =  c("0",
                                                 "1-2",
                                                 "3 or more"
                                     )))

count(dfStudy4_cleaning, survey_pre_q22, survey_pre_num_children_fct)


#create a factor for if children are still living at home
dfStudy4_cleaning <- mutate(dfStudy4_cleaning, survey_pre_children_home_fct = 
                              factor(survey_pre_q23, 
                                     labels =  c("No",
                                                 "Not applicable",
                                                 "Yes"
                                     )))

count(dfStudy4_cleaning, survey_pre_q23, survey_pre_children_home_fct)


#create a factor for ethnicity.  This doesn't behave correctly when specifying the ethnicity labels,
#keeping them out still creates a correct factor
dfStudy4_cleaning <- mutate(dfStudy4_cleaning, survey_pre_ethnicity_fct = 
                              factor(survey_pre_q24
                              ))

count(dfStudy4_cleaning, survey_pre_q24, survey_pre_ethnicity_fct)


#create a factor for generation British.  To be used to see if dietary intake is affected
#by the immigration background and therefore types of foods more likely to be consumed at home.

dfStudy4_cleaning <- mutate(dfStudy4_cleaning, survey_pre_generation_british_fct = 
                              factor(survey_pre_q67, 
                                     labels =  c("First generation",
                                                 "Second generation",
                                                 "Third generation or more"
                                     )))

count(dfStudy4_cleaning, survey_pre_q67, survey_pre_generation_british_fct)

#create a factor for education/qualification
dfStudy4_cleaning <- mutate(dfStudy4_cleaning, survey_pre_qualification_fct = 
                              factor(survey_pre_q25, 
                                     labels =  c("College or university",
                                                 "Lower secondary",
                                                 "Postgraduate degree"
                                     )))

count(dfStudy4_cleaning, survey_pre_q25, survey_pre_qualification_fct)


#create a factor for employment capacity	
dfStudy4_cleaning <- mutate(dfStudy4_cleaning, survey_pre_employment_fct = 
                              factor(survey_pre_q26, 
                                     labels =  c("Full-time",
                                                 "Not working",
                                                 "Part-time"
                                     )))

count(dfStudy4_cleaning, survey_pre_q26, survey_pre_employment_fct)


#create a factor for household income, again, using labels does not work well, possibly due
#to the special characters: > <  and £
dfStudy4_cleaning <- mutate(dfStudy4_cleaning, survey_pre_income_fct = 
                              factor(survey_pre_q27#, 
                                     #   labels =  c("< £18,000",
                                     #               "> £100,000",
                                     #               "Prefer no to say",
                                     #               "£18,00 to £30,999",
                                     #               "£31,000 to £51,999",
                                     #               "£52,000 to £100,000"
                              ))

count(dfStudy4_cleaning, survey_pre_q27, survey_pre_income_fct)

#create a factor for own assessment of overall health	
dfStudy4_cleaning <- mutate(dfStudy4_cleaning,
                            survey_pre_health_status_binary = 
                              ifelse(survey_pre_q28 == "Poor", 0, survey_pre_q28))

dfStudy4_cleaning$survey_pre_health_status_binary[dfStudy4_cleaning$survey_pre_health_status_binary == "Fair"] <- 1
dfStudy4_cleaning$survey_pre_health_status_binary[dfStudy4_cleaning$survey_pre_health_status_binary == "Good"] <- 2
dfStudy4_cleaning$survey_pre_health_status_binary[dfStudy4_cleaning$survey_pre_health_status_binary == "Very good"] <- 3
dfStudy4_cleaning$survey_pre_health_status_binary[dfStudy4_cleaning$survey_pre_health_status_binary == "Excellent"] <- 4

dfStudy4_cleaning$survey_pre_health_status_binary = as.numeric(dfStudy4_cleaning$survey_pre_health_status_binary)

dfStudy4_cleaning <- mutate(dfStudy4_cleaning, survey_pre_health_status_fct = 
                              factor(survey_pre_health_status_binary,
                                     labels = c("Poor", "Fair", "Good", "Very good", "Excellent")
                              ))

count(dfStudy4_cleaning, survey_pre_health_status_binary, survey_pre_health_status_fct)


#create a factor for age at menarche
dfStudy4_cleaning <- mutate(dfStudy4_cleaning, survey_pre_menarche_age_fct = 
                              factor(survey_pre_q29))

count(dfStudy4_cleaning, survey_pre_q29, survey_pre_menarche_age_fct)


#create a factor for having lifelong irregular menstrual cycle
dfStudy4_cleaning <- mutate(dfStudy4_cleaning,
                            survey_pre_irregular_cycle_binary = 
                              ifelse(dfStudy4_cleaning$survey_pre_q30 == "No", 0,
                                     ifelse(dfStudy4_cleaning$survey_pre_q30 == "Yes", 1, NA)))

dfStudy4_cleaning <- mutate(dfStudy4_cleaning, survey_pre_irregular_cycle_fct = 
                              factor(survey_pre_irregular_cycle_binary,
                                     labels = c("No", "Yes")))

count(dfStudy4_cleaning, survey_pre_q30, survey_pre_irregular_cycle_binary, survey_pre_irregular_cycle_fct)

#create a factor for age at first pregnancy
dfStudy4_cleaning <- mutate(dfStudy4_cleaning, survey_pre_pregnancy_age_fct = 
                              factor(survey_pre_q31))

count(dfStudy4_cleaning, survey_pre_q31, survey_pre_pregnancy_age_fct)

#create a factor for own assessment of menopausal stage
dfStudy4_cleaning <- mutate(dfStudy4_cleaning, survey_pre_meno_stage_fct = 
                              factor(survey_pre_q32,
                                     labels =  c("Peri-menopause",
                                                 "Post-menopause",
                                                 "Pre-menopause",
                                                 "Surgical menopause",
                                                 "Unsure"
                                     )))

count(dfStudy4_cleaning, survey_pre_q32, survey_pre_meno_stage_fct)

#create a factor for age when entered post-menopause
dfStudy4_cleaning <- mutate(dfStudy4_cleaning, survey_pre_postmeno_age_fct = 
                              factor(survey_pre_q33
                              ))

count(dfStudy4_cleaning, survey_pre_q33, survey_pre_postmeno_age_fct)


#create a factor for being sick prior to the start of the study
dfStudy4_cleaning <- mutate(dfStudy4_cleaning, 
                            survey_pre_sick_binary = ifelse(dfStudy4_cleaning$survey_pre_q71_46 == "No", 0, 1))

dfStudy4_cleaning <- mutate(dfStudy4_cleaning, survey_pre_sick_fct = 
                              factor(survey_pre_sick_binary, 
                                     labels =  c("No","Yes")
                                     
                              ))

count(dfStudy4_cleaning, survey_pre_sick_binary, survey_pre_sick_fct)



#create a factor for being a tech user, binary
dfStudy4_cleaning <- mutate(dfStudy4_cleaning, 
                            survey_pre_tech_user_binary = 
                              ifelse(dfStudy4_cleaning$survey_pre_q39 == "No", 0, 1))

dfStudy4_cleaning <- mutate(dfStudy4_cleaning, survey_pre_tech_user_fct = 
                              factor(survey_pre_tech_user_binary, 
                                     labels =  c("No", "Yes")
                              ))

count(dfStudy4_cleaning, survey_pre_tech_user_binary, survey_pre_tech_user_fct)



#update sleep wave 0 with baseline survey sleep data when baseline doens't exist

dfStudy4_cleaning$garmin_sleep_total_mins[dfStudy4_cleaning$wave == 0 & 
                                            dfStudy4_cleaning$primary_key == "82545275_0"] <- as.double(dfStudy4_cleaning$survey_pre_q59_1[dfStudy4_cleaning$wave == 0 & dfStudy4_cleaning$primary_key == "82545275_0"])*60

dfStudy4_cleaning$garmin_sleep_awake_mins[dfStudy4_cleaning$wave == 0 & 
                                            dfStudy4_cleaning$primary_key == "82545275_0"] <- as.double(dfStudy4_cleaning$survey_pre_q54_1[dfStudy4_cleaning$wave == 0 & dfStudy4_cleaning$primary_key == "82545275_0"])



#read MENQOL for tiredness level
#convert field names coming from Qualtrix
dfStudy4_cleaning$survey_pre_q41_18[dfStudy4_cleaning$wave == 0 & 
                                      dfStudy4_cleaning$survey_pre_q41_18
                                    == "Extremely bothered 6"] <- "6"
dfStudy4_cleaning$survey_pre_q41_18[dfStudy4_cleaning$wave == 0 &
                                      dfStudy4_cleaning$survey_pre_q41_18
                                    == "Not experienced"] <- "0"

count(dfStudy4_cleaning, survey_pre_q41_18)




#assign baseline tiredness factor to EMA tiredness factor so that we can use the EMA field starting
#from 0 in our analysis
#dfStudy4_cleaning$survey_pre_q41_18_fct[dfStudy4_cleaning$survey_pre_q41_18_fct == "No"] <- 0
#dfStudy4_cleaning$survey_pre_q41_18_fct[dfStudy4_cleaning$survey_pre_q41_18_fct == "Yes"] <- 1

dfStudy4_cleaning$ema_low_energy_level_fct[dfStudy4_cleaning$wave == 0] <- 
  dfStudy4_cleaning$survey_pre_q41_18_fct

#check count
count(dfStudy4_cleaning, ema_low_energy_level_fct, survey_pre_q41_18_fct)


#create a new variable to indicate interaction with a human coach.  Typically a day prior to a new
#phase of the program, the participants received both an EMA message and also an email from me. This would
#be a day prior to starting baseline, a day (Sunday) prior to starting week 1, an email prior to
#starting week 2, and a final EMA message an an email from me on the last day (DAy 14) of the study.
#On Day 15, the last message informed them to complete the post study and exit surveys.
dfStudy4_cleaning <- 
  mutate(dfStudy4_cleaning, ema_human_coach_interaction = 
           ifelse(wave == 1 | wave == 6 |wave == 7 | wave == 13 |wave == 14, 1, 0))


#create a new factor for coaching interaction
dfStudy4_cleaning <- mutate(dfStudy4_cleaning, ema_human_coach_interaction_fct = 
                              factor(ifelse(ema_human_coach_interaction == 0, 0, 1),
                                     labels = c("No", "Yes")
                              ))
count(dfStudy4_cleaning, ema_human_coach_interaction, ema_human_coach_interaction_fct)

dfStudy4_cleaning <- mutate(dfStudy4_cleaning, ema_achieved_planned_exercise_binary =
                              ifelse(is.na(ema_achieved_planned_exercise) | ema_achieved_planned_exercise == 2, 0, 
                                     ifelse(ema_achieved_planned_exercise == 3, 0, 1)))

dfStudy4_cleaning <- mutate(dfStudy4_cleaning, ema_achieved_planned_exercise_fct = 
                              factor(ifelse(ema_achieved_planned_exercise == 2, 0, 
                                            ifelse(ema_achieved_planned_exercise == 3, NA, 1)
                                            
                              ), labels = c("No", "Yes")
                              ))
count(dfStudy4_cleaning, ema_achieved_planned_exercise_fct)



####################################################################################

#write data to a file

write.csv(dfStudy4_cleaning, 
          file = "C:/R_code/study4/Allstudy4merged_complete_Sept26_cleaned.csv", 
          row.names = FALSE)

saveRDS(dfStudy4_cleaning, file = "C:/R_code/study4/study4_Sept26_cleaned.rds")

###################################################################################