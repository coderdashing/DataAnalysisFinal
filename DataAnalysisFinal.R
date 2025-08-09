###########################
#Libraries to Load        #
###########################

library(fastDummies)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(sqldf)

###########################
#Load Airport CSV Data    #
###########################

df = read.csv(file="Air_Traffic_Passenger_Statistics.csv", header=TRUE)

#Converting Passenger Count into a numeric
df$riders = as.numeric(gsub(",","",df$Passenger.Count))

#Testing a logarithm model for riders because there is a large number of passengers
df$logRiders = log(df$riders)

#Breaking Activity Period into numeric Year and Month
df$Year <- as.numeric(substr(df$Activity.Period, 1, 4))
df$Month <- as.numeric(substr(df$Activity.Period, 5, 6))

#Dummy Boolean Variables for each region
df$isMex <- ifelse(df$GEO.Region == "Mexico", 1, 0)
df$isUS <- ifelse(df$GEO.Region == "US", 1, 0)
df$isSA <- ifelse(df$GEO.Region == "South America", 1, 0)
df$isCentral <- ifelse(df$GEO.Region == "Central America", 1, 0)
df$isAsia <- ifelse(df$GEO.Region == "Asia", 1, 0)
df$isME <- ifelse(df$GEO.Region == "Middle East",1,0)
df$isCan <- ifelse(df$GEO.Region == "Canada",1,0)
df$isEU <- ifelse(df$GEO.Region == "Europe",1,0)
df$isAust <- ifelse(df$GEO.Region == "Australia / Oceania",1,0)

#Dummy Boolean Variables for condition for price, Activity summary
df$depart <- ifelse(df$Activity.Type.Code == "Enplaned",1,0)
df$arrive <- ifelse(df$Activity.Type.Code == "Deplaned",1,0)
df$exp = ifelse(df$Price.Category.Code == "Other",1,0)#consider new name: otherFare,etc
df$cheap = ifelse(df$Price.Category.Code == "Low Fare",1,0)#also consider new name like lowFare

#domestic flights
dom = df[which(df$GEO.Summary == "Domestic"),]#domestic
intl = df[which(df$GEO.Summary == "International"),]
intlAO = intl[which(df$isAust == 1),]
intlMX = intl[which(df$isMex == 1),]
intlSA = intl[which(df$isSA == 1),]
intlEU = intl[which(df$isEU == 1),]
intlME = intl[which(df$isME == 1),]

#come = df[which(df$Activity.Type.Code == "Deplaned"),]
#go = df[which(df$Activity.Type.Code == "Enplaned"),]
domutd = dom[which(dom$Operating.Airline == "United Airlines"),]#rename usUtd
domak = dom[which(dom$Operating.Airline == "Alaska Airlines"),]#consider renaming domAk

# Load the ggplot2 library
#using these variables names
sampleutd <- df %>%
  filter(Operating.Airline == "United Airlines") %>%
  group_by(Year) %>%
  summarise(Total_Passengers = sum(riders))

sampleutdmon <- domutd %>%
  group_by(Month)%>%
  summarise(Total_Passengers = sum(riders))


#create output files into a separate folder
if (!dir.exists("charts")) {
  dir.create("charts")
  cat("Created 'charts' directory\n")
}
if (!dir.exists("model")) {
  dir.create("model")
  cat("Created 'model' directory\n")
}


#summary plots
png("charts/united_patterns_mon.png", width=1000, height=600, res=120)
plot(1:12, sampleutdmon$Total_Passengers, 
     type="b", col="darkgreen", lwd=2, pch=16,
     main="Seasonal Passenger Traffic Patterns at SFO via United Airlines",
     xlab="Month", ylab="Total Passenger Count",
     xaxt="n")
axis(1, at=1:12, labels= sampleutdmon$Month, las=2)
abline(lm(Total_Passengers ~ as.numeric(Month), data=sampleutdmon), 
       col="red", lty=2)
dev.off()
samplemon <- df %>% #rename sample with meaningful name
  group_by(Month) %>%
  summarise(Total_Passengers = sum(riders, na.rm=TRUE))

png("charts/02_seasonal_patterns.png", width=1000, height=600, res=120)

plot(1:12, samplemon$Total_Passengers, 
     type="b", col="darkgreen", lwd=2, pch=16,
     main="Annual Passenger Traffic Patterns at SFO",
     xlab="Year", ylab="Total Passenger Count",
     xaxt="n")
axis(1, at=1:12, labels=samplemon$Month, las=2)
abline(lm(Total_Passengers ~ as.numeric(Month), data=samplemon), 
       col="red", lty=2)
dev.off()

png("charts/nz_passengers_mon.png", width=1000, height=600, res=120)
barplot(nzinfoyr$Total_Passengers,
        names.arg = nzinfoyr$Year,
        main="Total Passengers Between Australia and SFO via New sam",
        xlab="Year", ylab="Total Passengers",
        col=rainbow(10),
        border="darkblue")
dev.off()
austSum <- intlAO %>%
  group_by(Year)%>%
  summarise(Total_Passengers = sum(riders,na.rm = TRUE))
png("charts/yearly_aust_sum_bar.png", width=1000, height=600, res=120)
barplot(austSum$Total_Passengers,
        names.arg = austSum$Year,
        main="Total  Passengers by Year between Australia and SFO",
        xlab="Year", ylab="Total Passengers",
        col=rainbow(10),
        border="darkblue")
dev.off()

austStats <- df%>%
  filter(Year > 2013)%>%
  filter(isAust == 1)%>%
  group_by(Operating.Airline)%>%
  summarise(Total_Passengers = sum(riders,na.rm = TRUE))%>%
  arrange(desc(Total_Passengers))%>%
  head(10)

png("charts/airlines_aust_sum_bar_2.png", width=1000, height=600, res=120)
par(mar = c(10, 10, 5, 5))
barplot(austStats$Total_Passengers,
        names.arg = austStats$Operating.Airline,
        main="Most Popular Airlines between Australia and SFO",
        xlab="Airline", ylab="Total Passengers",
        col=rainbow(10),
        las = 2,
        border="darkblue")

dev.off()

euroSum <- intlEU %>%
  group_by(Year)%>%
  summarise(Total_Passengers = sum(riders,na.rm = TRUE))
png("charts/yearly_euro_sum_bar.png", width=1000, height=600, res=120)
barplot(euroSum$Total_Passengers,
        names.arg = euroSum$Year,
        main="Total  Passengers by Year between Europe and SFO",
        xlab="Year", ylab="Total Passengers",
        col=rainbow(10),
        border="darkblue")
dev.off()

saSum <- intlSA %>%
  group_by(Year)%>%
  summarise(Total_Passengers = sum(riders,na.rm = TRUE))
png("charts/yearly_sa_sum_bar.png", width=1000, height=600, res=120)
barplot(saSum$Total_Passengers,
        names.arg = saSum$Year,
        main="Total  Passengers by Year between South America and SFO",
        xlab="Year", ylab="Total Passengers",
        col=rainbow(10),
        border="darkblue")
dev.off()
austMon <- df %>%
  filter(GEO.Region == "Australia / Oceania")%>%
  group_by(Month)%>%
  summarise(Total_Passengers = sum(riders, na.rm=TRUE),.groups = 'drop')

png("charts/Aust_sfo_mon.png", width=1000, height=600, res=120)
plot(1:12, austMon$Total_Passengers, 
     type="b", col="darkgreen", lwd=2, pch=16,
     main="Seasonal Passenger Traffic Patterns between Australia and SFO",
     xlab="Month", ylab="Total Passenger Count",
     xaxt="n")
axis(1, at=1:12, labels=austMon$Month, las=2)
abline(lm(Total_Passengers ~ as.numeric(Month), data=austMon), 
       col="red", lty=2)

dev.off()

png("charts/Australia_SFO_Stats.png", width=1000, height=600, res=120)
austdata = df %>%
  filter(isSA == 1) %>%
  #filter(Year > 2013) %>%
  
  group_by(Operating.Airline) %>%
  summarise(Total_Passengers = sum(riders, na.rm=TRUE),
            .groups = 'drop') %>%
  arrange(desc(Total_Passengers))

travel_month = df%>%
  filter(GEO.Region == "Latin America") %>%
  group_by(Month) %>%
  summarise(Total_Passengers = sum(riders, na.rm=TRUE),
            Avg_Passengers = mean(riders, na.rm=TRUE),
            Flight_Count = n(),) %>%
  arrange(desc(Avg_Passengers))
  

