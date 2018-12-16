library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(readr)
library(Hmisc)
library(ggplot2)
library(rebus)
library(data.table)
library(readxl)

#importing data
data<- fread('MNCrime.csv')
#exploring the data
head(data)
tail(data, 20)

glimpse(data)
str(data)
describe(data)
unique(data$Offense)



# Unique Offenses
unique(data$Offense)

#exploring Offenses definitions
offense_desc<- select(data, Offense,Description)

#Group Motor Vehicle offenses in one category TMFV and everything else as THEFT
library(rebus)
data$Offense<- str_replace_all(data$Offense, pattern = or('AUTOTH','TMVP','MVTHFT','TMFV'),'TFMV')
data$Offense<- str_replace_all(data$Offense, pattern = or("SHOPLF" ,  "TBLDG" , "BURGD" , "TFPER", 
                                                           "ROBBIZ", "ROBPER", "ROBPAG","BURGB" , "ARSON" , "THFTSW", "ONLTHT", 
                                                           "NOPAY",  "MVTHFT", "POCKET", "COINOP", "DISARM", "COMPUT", "ADLTTN"
                                                           ,"BIKETF", "SCRAP" , "LOOT"),'THEFT')




#Cleaning Public Addresses 
data$publicaddress<- str_replace_all(data$publicaddress,('00'),"")
data$publicaddress<- str_replace_all(data$publicaddress,('XX'),"")

data1<- data
#Cleaning dates
data1$ReportedDate<- parse_date_time(data$ReportedDate, orders = c("ymd_HMS","mdy"))

#Both time of offense and reported time have 20156 Na's (16% of the data)
describe(data1)
tail(data1)

#Creating column showcasing difference between time of offense and time reported keeping tz as CDT

data1$ReportedDate<- ymd_hms(data1$ReportedDate, tz= 'America/Chicago')
data1$ReportedDate<- force_tz(data1$ReportedDate, tz= 'America/Chicago')
data1$dayofthemonth<- day(data1$ReportedDate)
data1$dayoftheweek<- weekdays(data1$ReportedDate)
data1$month<- lubridate::month(data1$ReportedDate, label= TRUE, abbr= TRUE)
data1$year<- year(data1$ReportedDate)
data1$`Time of Offense`<- as.Date(data$`Time of Offense`)
data1$`Time of Offense` <- hms(data$`Time of Offense`, tz= "America/Chicago", format= "%H:%M:%S")
data1$`Time of Offense` <- force_tz(data1$`Time of Offense`, tz= 'America/Chicago')

data1$time_reported_hms<- strftime(data1$ReportedDate, "%H:%M:%S", tz= 'America/Chicago')
data1$time_reported_hms<- hms(data1$time_reported_hms)

#Difference between time of offense and time reported
data1$Difftime_reported_occured<- as.numeric(data1$time_reported_hms - data1$`Time of Offense`) 
#Dealing with difference in days (nextday/ previousday time difference)
timediff_adjust<- function(x){
  if (x < 0)
  return(x+86400)
  else return(x)
}


data1$Difftime_adjusted<- sapply(data1$Difftime_reported_occured, timediff_adjust)

#Difftime in hours
data1$Difftime_reported_occured_hrs<- (data1$Difftime_adjusted)/3600

#Top offenses (69% of the thefts are motor vehicle related)
top_offenses<- data1%>%
  group_by(Offense) %>%
  summarise(counts= n()) %>%
  arrange(desc(counts)) %>%
  mutate(freq = counts / sum(counts))

#Plotting the offenses by count
offenses_plot<- ggplot(top_offenses, aes(x= top_offenses$Offense, y= top_offenses$counts))+
                         geom_bar(stat = "identity", fill= I("steelblue"))+
                        theme_bw()+ 
                        ggtitle("Count of Motor Vehicle related thefts and all other thefts")
                      
offenses_plot

#Months with the most offenses
top_offenses_month<- data1%>%
  group_by(month )%>%
  summarise(counts= n()) %>%
  arrange(desc(counts)) %>%
  mutate(freq = counts / sum(counts))

offenses_month_plot<- ggplot(top_offenses, aes(x= top_offenses_month$month, y= top_offenses_month$counts))+
  geom_bar(stat = "identity", fill= I("steelblue"))+
  theme_bw()+ 
  ggtitle("Theft and Motor vehicle theft counts by month")

write.csv(data1, file= "MN_Crime_Clean.csv")