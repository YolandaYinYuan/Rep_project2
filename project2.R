#load and read the data
library(downloader)
download("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile = "StormData.csv.bz2")
storm <- read.csv("Stormdata.csv.bz2", stringsAsFactors = FALSE, header = TRUE)

#only select the data related with "population health" and "economic consequences", storage to the new dataframe "storm1"
storm1 <- storm[ , c("STATE", "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
#storm1$EVTYPE <- as.factor(storm1$EVTYPE)

#let's initial understand the injuries data.
#sort the event type based on the fatalities total number and storage the data on a new dataframe "fatal_order"
library(plyr)
storm_fatal <- ddply(storm1,.(EVTYPE), summarize, sum = sum(FATALITIES))
fatal_order <- storm_fatal[order(storm_fatal$sum, decreasing = TRUE),]


#group the event types based on the top 10 fatalities event types. "FLASH FLOOD" - "FLOOD", "EXCESSIVE HEAT"-"HEAT", "TSTM/HIGH/else WIND"- "WIND"
storm1$EVTYPE <- gsub("FLASH FLOOD", "FLOOD", storm1$EVTYPE)
storm1$EVTYPE <- gsub("EXCESSIVE HEAT","HEAT", storm1$EVTYPE )
storm1$EVTYPE <- gsub("(.*)WIND","WIND", storm1$EVTYPE)

#get the ordered data based on the fatalities.
storm_fatal <- ddply(storm1,.(EVTYPE), summarize, sum_fatal = sum(FATALITIES))
fatal_order <- storm_fatal[order(storm_fatal$sum_fatal, decreasing = TRUE),]
head(fatal_order, 10)

#let's initial understand the injuries data
#sort the event types based on the injuries total number and storage the data on a new dataframe "injur_order"
storm_injur <- ddply(storm1,.(EVTYPE), summarize, sum_injur = sum(INJURIES))
injur_order <- storm_injur[order(storm_injur$sum_injur, decreasing = TRUE),]


#group again the event types based on the top 10 injuries event types. "ICE STORM" - "WINTER STORM", "WINDS"- "WIND"
storm1$EVTYPE <- gsub("ICE STORM", "WINTER STORM",storm1$EVTYPE)
storm1$EVTYPE <- gsub("WINDS","WIND", storm1$EVTYPE)

#get the ordered data based on the fatalities.
storm_injur <- ddply(storm1,.(EVTYPE), summarize, sum_injur = sum(INJURIES))
injur_order <- storm_injur[order(storm_injur$sum_injur, decreasing = TRUE),]
head(injur_order, 10)

#Q2
#'?', '+' and '-' values have no specific value, so i converted them to 'NA'
#I treated the exponent values in form of characters as follows:

'h/H' means hundred, exp value = 2
'k/K' means 'thousand', exp value = 3
'm/M' means 'million' , exp value = 6
'b/B',means billion, exp value of 9
Then I compute values as x * 10^exp value

storm1$CROPDMGEXP <- gsub("\\?", "NA", storm1$CROPDMGEXP)
storm1$CROPDMGEXP <- gsub("k|K", "3", storm1$CROPDMGEXP)
storm1$CROPDMGEXP <- gsub("m|M", "6", storm1$CROPDMGEXP)
storm1$CROPDMGEXP <- gsub("b|B", "9", storm1$CROPDMGEXP)

storm1$PROPDMGEXP <- gsub("\\?|\\+|\\-", "NA", storm1$PROPDMGEXP)
storm1$PROPDMGEXP <- gsub("h|H", "2", storm1$PROPDMGEXP)
storm1$PROPDMGEXP <- gsub("k|K", "3", storm1$PROPDMGEXP)
storm1$PROPDMGEXP <- gsub("m|M", "6", storm1$PROPDMGEXP)
storm1$PROPDMGEXP <- gsub("b|B", "9", storm1$PROPDMGEXP)

#change CROPDMGEXP & PROPDMGEXP values to numeric vectors.
storm1$CROPDMGEXP <- as.numeric(storm1$CROPDMGEXP, na.rm = TRUE)
storm1$PROPDMGEXP <- as.numeric(storm1$PROPDMGEXP)

#make a new dataframe called "ecoloss" to calculate the money loss for CROPDMG & PROPDMG seperately(compute values as x * 10^exp value) 
ecoloss <- data.frame(prop_loss = storm1$PROPDMG*(10^storm1$PROPDMGEXP), crop_loss = storm1$CROPDMG*(10^storm1$CROPDMGEXP))

#add a new column "total_loss" to "ecoloss" dataframe" to calculate the total loss for both CROPDMG and PROPDMG. (rowSums) and then add the EVTYPE column.
ecoloss$total_loss <- rowSums(ecoloss, na.rm = TRUE)
ecoloss$EVTYPE <- storm1$EVTYPE

#make a new dataframe called "storm_ecoloss" to calculate the sum of total loss for each EVTYPE, store the values in "sum_eco" colomn. 
storm_ecoloss <- ddply(ecoloss,.(EVTYPE), summarize, sum_eco = sum(total_loss))

#sort the EVTYPE based on the sum_eco value and store the ordered data on a new dataframe "ecoloss_order". 
ecoloss_order <- storm_ecoloss[order(storm_ecoloss$sum_eco, decreasing = TRUE),]

