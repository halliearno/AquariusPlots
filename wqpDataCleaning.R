
#Hallie Arno 
#NPS GLKN/ SIP Ecology Assistant 
#Updated 15 Feb, 2023
#Downloads and cleans Great Lakes water quality data from WQP


#Load Packages
library(lubridate)
library(tidyverse)
library(dataRetrieval)


#Filepaths for site names and site metadata
locations <- read.csv("C:\\Users\\harno\\Documents\\RCode\\station.csv") #Site names and info for all parks except SACN
sacnLocations <- read.csv("C:\\Users\\harno\\Documents\\RCode\\SACNSites.csv") #Site names and info for SACN



#SACN site list to retrieve using dataRetrieval
SACN_Ongoing<-c("11NPSWRD_WQX-SACN_NAKA_84.6","11NPSWRD_WQX-SACN_NAKA_41.3","11NPSWRD_WQX-SACN_NAKA_4.8","11NPSWRD_WQX-SACN_STCR_138.9","11NPSWRD_WQX-SACN_STCR_104.0","11NPSWRD_WQX-SACN_STCR_89.7", "11NPSWRD_WQX-SACN_STCR_63.8","11NPSWRD_WQX-SACN_STCR_43.7","11NPSWRD_WQX-SACN_STCR_20.0","11NPSWRD_WQX-SACN_STCR_15.8","11NPSWRD_WQX-SACN_STCR_2.0")

#Extract sites from lake parks spreadsheet
lakes_Ongoing <- locations$MonitoringLocationIdentifier

#Bind SACN and other parks
sites_to_get <- c(SACN_Ongoing, lakes_Ongoing) 

#Read WQP Data 
df <- readWQPqw(c(siteNumbers = sites_to_get,""),'','') 

#Remove QAQC data
df<-df[!grepl("Quality Control", df$ActivityTypeCode),] 

#HA: I don't know what all of below does

####################################################################
#Change "ActivityMediaName" for "CharacteristicName" = "Water level in relation to reference point" from "Air" to "Water_Level". Creating a "Water_Level" activity media type allows the water level data to be saved and passed through to the final analysis dataset, and differntiates the data from other data in the analysis dataset that was collected in the water. All "Air" and "Other"activity media types are deleted in following steps.  
df$ActivityMediaName<-ifelse(grepl('Water level in relation to reference point',df$CharacteristicName, ignore.case = T), 'Water_Level', df$ActivityMediaName)

#delete any "non-data" results, such as weather obs, comments, air temp, etc. Do this by using "grepl" with "!" (not) to delete any records with "ActivityMediaName"="Air" or "Other". Note: the "|" is "or" and allows selection of multiple items to delete. Deletes whole row.  
df<-df[!grepl("Air|Other", df$ActivityMediaName),]
#delete other non-data (subjective) results that have "ActivityMediaName" = "Water". Delete in "CharacteristicName" =  "water apperance (text)" and "wave height" using "!grepl". Deletes whole row.
df<-df[!grepl("Wave height|Water appearance", df$CharacteristicName),]

######################################################################
#Change phosphorous to "bottom phosphorous" when measured at maximum depth 
d$CharacteristicName2 <- d$CharacteristicName
d$CharacteristicName2[d$Depth_m!=0 & d$CharacteristicName == "Total Phosphorus, mixed forms"] <- "Bottom Phosphorus"
d$CharacteristicName <- d$CharacteristicName2 #Replace old column name

#Select relevant columns
glknwqp <- df %>% select(c("ActivityIdentifier", "CharacteristicName", "ActivityDepthHeightMeasure.MeasureValue", "ResultMeasureValue", "ActivityEndDateTime", "MonitoringLocationIdentifier", "ResultMeasure.MeasureUnitCode")) %>% 
  
  mutate(Station_ID = str_extract(MonitoringLocationIdentifier,"[^\\-]+$")) %>% 
  
  rename(c("Activity_ID" = "ActivityIdentifier", 
           "Depth_m" = "ActivityDepthHeightMeasure.MeasureValue", 
           "Result_Value" = "ResultMeasureValue",   
           "datetime_end_UTC" = "ActivityEndDateTime")) %>%
  
  mutate_at(vars("Depth_m"),~replace_na(.,0))



#d %>% select(c("CharacteristicName2", "ResultMeasure.MeasureUnitCode", "Result_Value", "Depth_m", "datetime_end_UTC", "Station_ID"))


#---------------------------------------------------------

#2. Get USGS Data from WQP Data

#USGS Flow data sites near SACN WQP Sampling Sites
usgssites <- (c("05331833", "05333500", "05340500", "05341550", "05344490"))
#
pCode <- "00060"
start.date <- "2007-01-01"
end.date <- Sys.Date()

sites_discharge <- readNWISdv(siteNumbers = usgssites,
                              parameterCd = pCode,
                              startDate = start.date,
                              endDate = end.date)
colnames(sites_discharge) <- c("Agency", "usgsid", "Date", "FlowValue", "letter")
usgs <- sites_discharge %>% select(-letter)


usgssitematch <- data.frame(id = c(1:5), Site = c("SACN_NAKA_84.6", "SACN_STCR_104.0", "SACN_STCR_43.7", "SACN_STCR_20.0", "SACN_STCR_2.0"))
usgssitematch$usgsid <- c("05331833", "05333500", "05340500", "05341550", "05344490")


glkn_sacn_id <- merge(glknwqp, usgssitematch[, c("Site", "usgsid")], by.x = "Station_ID", by.y = "Site", all.x = TRUE)
glkn_sacn_id$Date <- as.Date(glkn_sacn_id$datetime_end_UTC)

withFlow <- merge(glkn_sacn_id, usgs[, c("usgsid", "Date", "FlowValue")], by = c("usgsid", "Date"), all.x = TRUE)


#3. Add lat long

sacnLocations <- sacnLocations %>% select(SiteID, Latitude, Longitude, Coloquial.Name)
colnames(sacnLocations) <- c("ID", "LatitudeMeasure", "LongitudeMeasure", "Name")


locations$ID <- substr(locations$MonitoringLocationIdentifier, nchar(locations$MonitoringLocationIdentifier) - 6, nchar(locations$MonitoringLocationIdentifier)) 

locations <- locations %>% select(ID, LatitudeMeasure, LongitudeMeasure, MonitoringLocationName)
colnames(locations) <- c("ID", "LatitudeMeasure", "LongitudeMeasure", "Name")

latLong <- rbind(locations, sacnLocations)
colnames(latLong) <- c("glknid", "Latitude", "Longitude", "Name")



df <- merge(withFlow, latLong, by.x = "Station_ID", by.y = "glknid", all.x = TRUE)

df$Park <- substr(df$Station_ID, 1, 4)
df$Year <- year(df$Date)
df$Month <- month(df$Date)

colnames(df) <- c("Site", "USGSID", "Date", "Variable", "Units", "Value", "Depth", "DateTime", "Flow", "Latitude", "Longitude", "Name", "Park", "Year", "Month")


df