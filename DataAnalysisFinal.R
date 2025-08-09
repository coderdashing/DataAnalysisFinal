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


sample <- df %>% #rename sample with meaningful name
  group_by(Year) %>%
  summarise(Total_Passengers = sum(riders, na.rm=TRUE))
png("charts/yearly_passengers_bar.png", width=1000, height=600, res=120)
barplot(sample$Total_Passengers,
        names.arg = sample$Year,
        main="Total Passengers by Year at SFO",
        xlab="Year", ylab="Total Passengers",
        col=rainbow(10),
        border="darkblue")
dev.off()
png("charts/nz_passengers_mon.png", width=1000, height=600, res=120)
barplot(nzinfoyr$Total_Passengers,
        names.arg = nzinfoyr$Year,
        main="Total Passengers Between Australia and SFO via New sam",
        xlab="Year", ylab="Total Passengers",
        col=rainbow(10),
        border="darkblue")
dev.off()
intlSum <- df %>%
  group_by(Year)%>%
  filter(Operating.Airline == "International")%>%
  summarise(Total_Passengers = sum(riders, na.rm=TRUE))
png("charts/yearly_intl_sum_bar.png", width=1000, height=600, res=120)
barplot(intlSum$Total_Passengers,
        names.arg = intlSum$Year,
        main="Total International Passengers by Year at SFO",
        xlab="Year", ylab="Total Passengers",
        col=rainbow(10),
        border="darkblue")
dev.off()
domSum <- df %>%
  group_by(Year)%>%
  filter(Operating.Airline == "Domestic")%>%
  summarise(Total_Passengers = sum(riders, na.rm=TRUE))
png("charts/yearly_dom_sum_bar.png", width=1000, height=600, res=120)
barplot(domSum$Total_Passengers,
        names.arg = domSum$Year,
        main="Total Domestic Passengers by Year at SFO",
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

png("charts/02_annual_patterns.png", width=1000, height=600, res=120)

plot(1:12, sample$Total_Passengers, 
     type="b", col="darkgreen", lwd=2, pch=16,
     main="Annual Passenger Traffic Patterns at SFO",
     xlab="Year", ylab="Total Passenger Count",
     xaxt="n")
axis(1, at=1:12, labels=sample$Year, las=2)
abline(lm(Total_Passengers ~ as.numeric(Year), data=sample), 
       col="red", lty=2)
dev.off()

austMon <- df %>%
  filter(GEO.Region == "Australia / Oceania")%>%
  group_by(Month)%>%
  summarise(Total_Passengers = sum(riders, na.rm=TRUE),.groups = 'drop')
topAustMon<- austMon%>%
  arrange(desc(Total_Passengers))%>%
  summarise(MonName = month.name[Month],Passengers = Total_Passengers)%>%
  head(3)
write.csv(topAustMon,file = "charts/topAustMon.csv",row.names = FALSE)
# Create the line graph
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

png("charts/Nonlinear_Regression_Test.png", width=1000, height=600, res=120)
ggplot(df, aes(x = Year, y = riders)) +geom_line() + scale_x_continuous(trans = "log10",
                     breaks = c(1, 10, 100, 1000), # Example breaks
                     labels = function(x) paste0("Log(", x, ")")) + # Custom labels
  # Customize the y-axis with a function (e.g., custom formatting)
  scale_y_continuous(labels = function(y) paste0(y/1000, "K")) + # Example: show values in thousands
  labs(title = "SFO Airport Passengers vs. Years",
       x = "Years",
       y = "Riders") +
  theme_minimal() # Optional: use a minimalist theme
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
  filter()
  group_by(Month) %>%
  summarise(Total_Passengers = sum(riders, na.rm=TRUE),
            Avg_Passengers = mean(riders, na.rm=TRUE),
            Flight_Count = n(),) %>%
  arrange(desc(Avg_Passengers))%>%

#########################################
# Store the sample data in a data frame #
#########################################
barplot(austdata$Total_Passengers,names.arg = austdata$Operating.Airline,yaxt="n", ylab="Passenger Count in Thousands", main="Top 10 Airlines to and from Australia",col=rainbow(10),las=2)
yaxt="n"


####################
# Plot the points. #
####################

#plot(df$Month,df$riders)
#plot(intl$Month,intl$riders)
#plot(come$Month,come$riders)
#plot(go$Month,go$riders)
#plot(domak$Year+domak$Month,domutd$logRiders)
#plot(domak$depart + domak$arrive + domak$Month+domak$Year,domak$logRiders)
#plot(intl$isMex+intl$isAust+intl$isCentral+intl$isME+intl$Year+intl$Month+intl$depart+intl$arrive,intl$logRiders)
#boxplot(df$logRiders,dom$logRiders,intl$logRiders,main = "Distribution of All-Time Passengers",names = c("Total","Domestic","International"),col = c("gray","blue","red"))
#boxplot(intlAO[which(intlMX$Operating.Airline.IATA.Code == "UA"),]$logRiders,intlMX[which(intlMX$Operating.Airline.IATA.Code != "UA"),]$logRiders,main = "Comparing United with Other Airlines to Australia",names = c("United","Other"),col = c("blue","gray"))

##########################################
# Try a linear fit function with Y=a+b*X #plo
##########################################

model1=lm(sample$Total_Passengers~df$Month,data=df)
train_yrs <- c("2006", "2007", "2008", "2009", "2010", "2011", 
         "2012", "2013", "2014", "2015", "2016", "2017")

png("model/primary_model.png", width=1000, height=600, res=120)
train_data <- df[which(df$Year > 2013),]
val_data=df[which(df$Year > 2013),]
model_overall <- lm(logRiders ~ df$Year, data=df)
train_pred_overall <- predict(model_overall, train_data)
val_pred_overall <- predict(model_overall, val_data)
train_rmse_overall <- rmse(train_data$Log_Passenger_Count, train_pred_overall)
val_rmse_overall <- rmse(val_data$Log_Passenger_Count, val_pred_overall)
train_r2_overall <- cor(train_data$Log_Passenger_Count, train_pred_overall)^2
val_r2_overall <- cor(val_data$Log_Passenger_Count, val_pred_overall)^2
dev.off()


model1All = lm(logRiders~isMex+isAust+isCentral+isME+Year+Month+depart+arrive,data=intl)

model1Dom=lm(dom$logRiders~dom$Month+Year+depart+arrive+exp+cheap,data=dom)
#model1DomUtd=lm(logRiders~depart+arrive,data=domutd)
model1DomAk = lm(logRiders~depart+arrive,data=domak)

model1Intl=lm(intl$riders~intl$Month,data=intl)
#model1Come=lm(come$riders~come$Month,data=come)
#model1Go=lm(go$riders~go$Month,data=go)
#model1DomGo = lm(domGo$riders~domGo$Month,data=domGo)
#model1IntlCome = lm(intlCome$riders~intlCome$Month,data=intlCome)
#model1High = lm(high$riders~high$Month,data=high)
#model1Low = lm(low$riders~low$Month,data=low)
###################################################
# Plot the linear function obtained by regression #
###################################################

xVals=seq(min(df$Month),max(df$Month), by=1)
xValsDom=seq(min(dom$Month),max(dom$Month), by=1)
xValsIntl=seq(min(intl$Month),max(intl$Month), by=1)
xValsCome=seq(min(come$Month),max(come$Month), by=1)
xValsGo=seq(min(go$Month),max(go$Month), by=1)

#xValsDomGo=seq(min(domGo$Month),max(domGo$Month), by=1)
#xValsIntlCome=seq(min(intlCome$Month),max(intlCome$Month), by=1)

lines(xVals,xVals*model1$coefficients[2]+model1$coefficients[1],col="blue")
#lines(xValsDom,xValsDom*model1Dom$coefficients[2]+model1Dom$coefficients[1],col="blue")
#lines(xValsIntl,xValsIntl*model1Intl$coefficients[2]+model1Intl$coefficients[1],col="blue")
#lines(xValsCome,xValsCome*model1Come$coefficients[2]+model1Come$coefficients[1],col="blue")
#lines(xValsGo,xValsGo*model1Go$coefficients[2]+model1Go$coefficients[1],col="blue")
#lines(xValsDomGo,xValsDomGo*model1DomGo$coefficients[2]+model1DomGo$coefficients[1],col="blue")
#lines(xValsIntlCome,xValsIntlCome*model1IntlCome$coefficients[2]+model1IntlCome$coefficients[1],col="blue")
#lines(xVals,xVals*model1High$coefficients[2] + model1High$coefficients[1],col = "blue")
#lines(xVals,xVals*model1Low$coefficients[2] + model1Low$coefficients[1],col = "blue")
#lines(xVals,xVals*model1DomUtd$coefficients[2]+model1DomUtd$coefficients[1],col="blue")
lines(xVals,xVals*model1DomAk$coefficients[2]+model1DomAk$coefficients[1],col="blue")

print("got blue line")
##############################################################
# Find the correlation coefficient between X and Y - its     #
# value is close to 0 indicating a weak or absent linear     #
#relationship between X and Y                                #
#Notice the two lines of code do the same thing since the    #
#dataframe has only numeric variables.                       #
##############################################################

corr <- cor(df$Month,df$riders,method="pearson")
corrDom <- cor(dom$Month,dom$riders,method="pearson")
corrIntl <- cor(intl$Month,intl$riders,method="pearson")
corrCome <- cor(come$Month,come$riders,method = "pearson")
corrGo <- cor(go$Month,go$riders,method = "pearson")
corrDomUtd <- cor(domutd$depart+domutd$arrive,domutd$logRiders,method = "pearson")
corrDomAk <-cor(domak$depart+domak$arrive,domak$riders,method = "pearson")

##############################################################
# Read features of the model including the coeffients.       #
# Notice the very low "Multiple R-Squared" value, indicating #
# a poor fit                                                 #      
##############################################################

summary(model1)
#summary(model1Dom)
#summary(model1Intl)
#summary(model1Come)
#summary(model1Go)
#summary(model1DomGo)
#summary(model1IntlCome)
#summary(model1High)
#summary(model2High)
#summary(model1DomUtd)
summary(model1DomAk)
#plot(fitted(model1DomUtd), residuals(model1DomUtd), xlab = "Fitted Values of Activity Period Status", ylab = "Residuals of Passengers", main = "Residuals of Passengers for Domestic Flights on United Airlines")
#############################################################
# Since the points look parabolic, consider incorporating   #
# the X's squares - you need to add these to the data frame #
#############################################################
df$YrSq=df$Month^2
dom$YrSq=dom$Month^2
intl$YrSq=intl$Month^2
come$YrSq=come$Month^2
go$YrSq=go$Month^2
domak$monSq = domak$Month^2
#domutd$MonSq = domutd$Month^2
#domGo$YrSq=domGo$Month^2
#intlCome$YrSq=intlCome$Month^2


##################################################################
# Make a new model incorporating the original values and squares #
# Y = a + b*X+c*XSquared                                         #
##################################################################
model2<-lm(df$riders~df$Month+df$YrSq,data=df)
model2Dom<-lm(dom$logRiders~dom$Month+dom$YrSq,data=dom)
model2Intl<-lm(intl$riders~intl$Month+intl$YrSq,data=intl)
model2Come<-lm(come$riders~come$Month+come$YrSq,data=come)
model2Go<-lm(go$riders~go$Month+go$YrSq,data=go)
#model2DomUtd = lm(domutd$riders~domutd$Month+domutd$MonSq,data=domutd)
model2DomAk = lm(domak$riders~domak$Month+domak$MonSq,data=domak)
#model2DomGo <- lm(domGo$riders~domGo$Month+df$YrSq,data=domGo)
#model2IntlCome <- lm(intlCome$riders~intlCome$Month+df$YrSq,data=intlCome)

############################################
# Plot the points again, add the new model #
############################################

#lines(xVals,model2$coefficients[1]+xVals*model2$coefficients[2]+xVals^2*model2$coefficients[3],col="red")
#lines(xValsDom,model2Dom$coefficients[1]+xValsDom*model2Dom$coefficients[2]+xValsDom^2*model2Dom$coefficients[3],col="red")
#lines(xValsIntl,model2Intl$coefficients[1]+xValsIntl*model2Intl$coefficients[2]+xValsIntl^2*model2Intl$coefficients[3],col="red")
#lines(xValsCome,model2Come$coefficients[1]+xValsCome*model2Come$coefficients[2]+xValsCome^2*model2Come$coefficients[3],col="red")
#lines(xValsGo,model2Go$coefficients[1]+xValsGo*model2Go$coefficients[2]+xValsGo^2*model2Go$coefficients[3],col="red")
#lines(xVals,model2DomGo$coefficients[1]+xVals*model2DomGo$coefficients[2]+xValsDomGo^2*model2DomGo$coefficients[3],col="red")
#lines(xValsIntlCome,model2IntlCome$coefficients[1]+xValsIntlCome*model2IntlCome$coefficients[2]+xValsIntlCome^2*model2IntlCome$coefficients[3],col="red")
#lines(xVals,model2DomUtd$coefficients[1]+xVals*model2DomUtd$coefficients[2]+xVals^2*model2DomUtd$coefficients[3],col="red")
lines(xVals,model2DomAk$coefficients[1]+xVals*model2DomAk$coefficients[2]+xVals^2*model2DomAk$coefficients[3],col="red")

print("got red line")
################################################################
# Read features of the degree two polynomial linear regression #
# Notice that the value of the R-Squared value is much closer  #
# to 1, indicating a near-perfect match between model and data #
################################################################

#summary(model2)
#summary(model2Dom)
#summary(model2Intl)
#summary(model2Come)
#summary(model2Go)
#summary(model2DomGo)
#summary(model2IntlCome)
#summary(model2DomUtd)
summary(model2DomAk)

max_riders <- max(intl$riders)
y_breaks <- seq(0, max_riders + 1000, by = 2000) # Adjust by value based on your data range
axis(2, at = y_breaks, labels = y_breaks / 1000) # Display labels in thousands
# Create the line graph
ggplot(df, aes(x = Years, y = df$riders)) +
  geom_line() +
  # Customize the x-axis with a function (e.g., log transformation)
  scale_x_continuous(trans = "log10",
                     breaks = c(1, 10, 100, 1000), # Example breaks
                     labels = function(x) paste0("Log(", x, ")")) + # Custom labels
  # Customize the y-axis with a function (e.g., custom formatting)
  scale_y_continuous(labels = function(y) paste0(y/1000, "K")) + # Example: show values in thousands
  labs(title = "Your Line Graph Title",
       x = "Your X-Axis Label",
       y = "Your Y-Axis Label") +
  theme_minimal() # Optional: use a minimalist theme

austdata = df %>%
  filter(isSA == 1) %>%
  #filter(Year > 2013) %>%
  
  group_by(Operating.Airline) %>%
  summarise(Total_Passengers = sum(riders, na.rm=TRUE),
            .groups = 'drop') %>%
  arrange(desc(Total_Passengers))%>%
  
mexdata = df %>%
  filter(GEO.Region == "Mexico") %>%
  #filter(Year > 2013) %>%
  
  group_by(Operating.Airline) %>%
  summarise(Total_Passengers = sum(riders, na.rm=TRUE),
            .groups = 'drop') %>%
  arrange(desc(Total_Passengers))%>%
  
ladata = df %>%
  filter(GEO.Region == "Latin America") %>%
  #filter(Year > 2013) %>%
  
  group_by(Operating.Airline) %>%
  summarise(Total_Passengers = sum(riders, na.rm=TRUE),
            .groups = 'drop') %>%
  arrange(desc(Total_Passengers))%>%
  
travel_month = df%>%
  filter(GEO.Region == "Latin America") %>%
  group_by(Month) %>%
  summarise(Total_Passengers = sum(riders, na.rm=TRUE),
            Avg_Passengers = mean(riders, na.rm=TRUE),
            Flight_Count = n(),) %>%
  arrange(desc(Avg_Passengers))%>%

#########################################
# Store the sample data in a data frame #
#########################################
barplot(ladata$Total_Passengers,names.arg = ladata$Operating.Airline,yaxt="n", ylab="Passenger Count in Thousands", main="Top 10 Airlines to and from Australia",col=rainbow(10),las=2)
yaxt="n"



####################
# Plot the points. #
####################

#plot(df$Month,df$riders)
#plot(intl$Month,intl$riders)
#plot(come$Month,come$riders)
#plot(go$Month,go$riders)
#plot(domak$Year+domak$Month,domutd$logRiders)
#plot(domak$depart + domak$arrive + domak$Month+domak$Year,domak$logRiders)
#plot(intl$isMex+intl$isAust+intl$isCentral+intl$isME+intl$Year+intl$Month+intl$depart+intl$arrive,intl$logRiders)
#boxplot(df$logRiders,dom$logRiders,intl$logRiders,main = "Distribution of All-Time Passengers",names = c("Total","Domestic","International"),col = c("gray","blue","red"))
#boxplot(intlAO[which(intlMX$Operating.Airline.IATA.Code == "UA"),]$logRiders,intlMX[which(intlMX$Operating.Airline.IATA.Code != "UA"),]$logRiders,main = "Comparing United with Other Airlines to Australia",names = c("United","Other"),col = c("blue","gray"))

##########################################
# Try a linear fit function with Y=a+b*X #plo
##########################################

#model1=lm(df$riders~df$Month,data=df)
model1All = lm(logRiders~isMex+isAust+isCentral+isME+Year+Month+depart+arrive,data=intl)

model1Dom=lm(dom$logRiders~dom$Month+Year+depart+arrive+exp+cheap,data=dom)
#model1DomUtd=lm(logRiders~depart+arrive,data=domutd)
model1DomAk = lm(logRiders~depart+arrive,data=domak)

model1Intl=lm(intl$riders~intl$Month,data=intl)
#model1Come=lm(come$riders~come$Month,data=come)
#model1Go=lm(go$riders~go$Month,data=go)
#model1DomGo = lm(domGo$riders~domGo$Month,data=domGo)
#model1IntlCome = lm(intlCome$riders~intlCome$Month,data=intlCome)
#model1High = lm(high$riders~high$Month,data=high)
#model1Low = lm(low$riders~low$Month,data=low)
###################################################
# Plot the linear function obtained by regression #
###################################################

xVals=seq(min(df$Month),max(df$Month), by=1)
xValsDom=seq(min(dom$Month),max(dom$Month), by=1)
xValsIntl=seq(min(intl$Month),max(intl$Month), by=1)
xValsCome=seq(min(come$Month),max(come$Month), by=1)
xValsGo=seq(min(go$Month),max(go$Month), by=1)

#xValsDomGo=seq(min(domGo$Month),max(domGo$Month), by=1)
#xValsIntlCome=seq(min(intlCome$Month),max(intlCome$Month), by=1)

#lines(xVals,xVals*model1$coefficients[2]+model1$coefficients[1],col="blue")
lines(xValsDom,xValsDom*model1Dom$coefficients[2]+model1Dom$coefficients[1],col="blue")
#lines(xValsIntl,xValsIntl*model1Intl$coefficients[2]+model1Intl$coefficients[1],col="blue")
#lines(xValsCome,xValsCome*model1Come$coefficients[2]+model1Come$coefficients[1],col="blue")
#lines(xValsGo,xValsGo*model1Go$coefficients[2]+model1Go$coefficients[1],col="blue")
#lines(xValsDomGo,xValsDomGo*model1DomGo$coefficients[2]+model1DomGo$coefficients[1],col="blue")
#lines(xValsIntlCome,xValsIntlCome*model1IntlCome$coefficients[2]+model1IntlCome$coefficients[1],col="blue")
#lines(xVals,xVals*model1High$coefficients[2] + model1High$coefficients[1],col = "blue")
#lines(xVals,xVals*model1Low$coefficients[2] + model1Low$coefficients[1],col = "blue")
#lines(xVals,xVals*model1DomUtd$coefficients[2]+model1DomUtd$coefficients[1],col="blue")
lines(xVals,xVals*model1DomAk$coefficients[2]+model1DomAk$coefficients[1],col="blue")

print("got blue line")
##############################################################
# Find the correlation coefficient between X and Y - its     #
# value is close to 0 indicating a weak or absent linear     #
#relationship between X and Y                                #
#Notice the two lines of code do the same thing since the    #
#dataframe has only numeric variables.                       #
##############################################################

corr <- cor(df$Month,df$riders,method="pearson")
corrDom <- cor(dom$Month,dom$riders,method="pearson")
corrIntl <- cor(intl$Month,intl$riders,method="pearson")
corrCome <- cor(come$Month,come$riders,method = "pearson")
corrGo <- cor(go$Month,go$riders,method = "pearson")
corrDomUtd <- cor(domutd$depart+domutd$arrive,domutd$logRiders,method = "pearson")
corrDomAk <-cor(domak$depart+domak$arrive,domak$riders,method = "pearson")

##############################################################
# Read features of the model including the coeffients.       #
# Notice the very low "Multiple R-Squared" value, indicating #
# a poor fit                                                 #      
##############################################################

summary(model1)
#summary(model1Dom)
#summary(model1Intl)
#summary(model1Come)
#summary(model1Go)
#summary(model1DomGo)
#summary(model1IntlCome)
#summary(model1High)
#summary(model2High)
#summary(model1DomUtd)
summary(model1DomAk)
#plot(fitted(model1DomUtd), residuals(model1DomUtd), xlab = "Fitted Values of Activity Period Status", ylab = "Residuals of Passengers", main = "Residuals of Passengers for Domestic Flights on United Airlines")
#############################################################
# Since the points look parabolic, consider incorporating   #
# the X's squares - you need to add these to the data frame #
#############################################################
df$YrSq=df$Month^2
dom$YrSq=dom$Month^2
intl$YrSq=intl$Month^2
come$YrSq=come$Month^2
go$YrSq=go$Month^2
domak$monSq = domak$Month^2
#domutd$MonSq = domutd$Month^2
#domGo$YrSq=domGo$Month^2
#intlCome$YrSq=intlCome$Month^2


##################################################################
# Make a new model incorporating the original values and squares #
# Y = a + b*X+c*XSquared                                         #
##################################################################
model2<-lm(df$riders~df$Month+df$YrSq,data=df)
model2Dom<-lm(dom$logRiders~dom$Month+dom$YrSq,data=dom)
model2Intl<-lm(intl$riders~intl$Month+intl$YrSq,data=intl)
model2Come<-lm(come$riders~come$Month+come$YrSq,data=come)
model2Go<-lm(go$riders~go$Month+go$YrSq,data=go)
#model2DomUtd = lm(domutd$riders~domutd$Month+domutd$MonSq,data=domutd)
model2DomAk = lm(domak$riders~domak$Month+domak$MonSq,data=domak)
#model2DomGo <- lm(domGo$riders~domGo$Month+df$YrSq,data=domGo)
#model2IntlCome <- lm(intlCome$riders~intlCome$Month+df$YrSq,data=intlCome)

############################################
# Plot the points again, add the new model #
############################################

#lines(xVals,model2$coefficients[1]+xVals*model2$coefficients[2]+xVals^2*model2$coefficients[3],col="red")
#lines(xValsDom,model2Dom$coefficients[1]+xValsDom*model2Dom$coefficients[2]+xValsDom^2*model2Dom$coefficients[3],col="red")
#lines(xValsIntl,model2Intl$coefficients[1]+xValsIntl*model2Intl$coefficients[2]+xValsIntl^2*model2Intl$coefficients[3],col="red")
#lines(xValsCome,model2Come$coefficients[1]+xValsCome*model2Come$coefficients[2]+xValsCome^2*model2Come$coefficients[3],col="red")
#lines(xValsGo,model2Go$coefficients[1]+xValsGo*model2Go$coefficients[2]+xValsGo^2*model2Go$coefficients[3],col="red")
#lines(xVals,model2DomGo$coefficients[1]+xVals*model2DomGo$coefficients[2]+xValsDomGo^2*model2DomGo$coefficients[3],col="red")
#lines(xValsIntlCome,model2IntlCome$coefficients[1]+xValsIntlCome*model2IntlCome$coefficients[2]+xValsIntlCome^2*model2IntlCome$coefficients[3],col="red")
#lines(xVals,model2DomUtd$coefficients[1]+xVals*model2DomUtd$coefficients[2]+xVals^2*model2DomUtd$coefficients[3],col="red")
lines(xVals,model2DomAk$coefficients[1]+xVals*model2DomAk$coefficients[2]+xVals^2*model2DomAk$coefficients[3],col="red")

print("got red line")
################################################################
# Read features of the degree two polynomial linear regression #
# Notice that the value of the R-Squared value is much closer  #
# to 1, indicating a near-perfect match between model and data #
################################################################

#summary(model2)
#summary(model2Dom)
#summary(model2Intl)
#summary(model2Come)
#summary(model2Go)
#summary(model2DomGo)
#summary(model2IntlCome)
#summary(model2DomUtd)
summary(model2DomAk)

max_riders <- max(intl$riders)
y_breaks <- seq(0, max_riders + 1000, by = 2000) # Adjust by value based on your data range
axis(2, at = y_breaks, labels = y_breaks / 1000) # Display labels in thousands
# Create the line graph
ggplot(df, aes(x = Years, y = df$riders)) +
  geom_line() +
  # Customize the x-axis with a function (e.g., log transformation)
  scale_x_continuous(trans = "log10",
                     breaks = c(1, 10, 100, 1000), # Example breaks
                     labels = function(x) paste0("Log(", x, ")")) + # Custom labels
  # Customize the y-axis with a function (e.g., custom formatting)
  scale_y_continuous(labels = function(y) paste0(y/1000, "K")) + # Example: show values in thousands
  labs(title = "Your Line Graph Title",
       x = "Your X-Axis Label",
       y = "Your Y-Axis Label") +
  theme_minimal() # Optional: use a minimalist theme

austdata = df %>%
  filter(isSA == 1) %>%
  #filter(Year > 2013) %>%
  
  group_by(Operating.Airline) %>%
  summarise(Total_Passengers = sum(riders, na.rm=TRUE),
            .groups = 'drop') %>%
  arrange(desc(Total_Passengers))%>%
  
  mexdata = df %>%
  filter(GEO.Region == "Mexico") %>%
  #filter(Year > 2013) %>%
  
  group_by(Operating.Airline) %>%
  summarise(Total_Passengers = sum(riders, na.rm=TRUE),
            .groups = 'drop') %>%
  arrange(desc(Total_Passengers))%>%
  
  ladata = df %>%
  filter(GEO.Region == "Latin America") %>%
  #filter(Year > 2013) %>%
  
  group_by(Operating.Airline) %>%
  summarise(Total_Passengers = sum(riders, na.rm=TRUE),
            .groups = 'drop') %>%
  arrange(desc(Total_Passengers))%>%
  
  travel_month = df%>%
  filter(GEO.Region == "Latin America") %>%
  group_by(Month) %>%
  summarise(Total_Passengers = sum(riders, na.rm=TRUE),
            Avg_Passengers = mean(riders, na.rm=TRUE),
            Flight_Count = n(),) %>%
  arrange(desc(Avg_Passengers))%>%
  
  #########################################
# Store the sample data in a data frame #
#########################################
barplot(ladata$Total_Passengers,names.arg = ladata$Operating.Airline,yaxt="n", ylab="Passenger Count in Thousands", main="Top 10 Airlines to and from Australia",col=rainbow(10),las=2)
yaxt="n"



####################
# Plot the points. #
####################

#plot(df$Month,df$riders)
#plot(intl$Month,intl$riders)
#plot(come$Month,come$riders)
#plot(go$Month,go$riders)
#plot(domak$Year+domak$Month,domutd$logRiders)
#plot(domak$depart + domak$arrive + domak$Month+domak$Year,domak$logRiders)
#plot(intl$isMex+intl$isAust+intl$isCentral+intl$isME+intl$Year+intl$Month+intl$depart+intl$arrive,intl$logRiders)
#boxplot(df$logRiders,dom$logRiders,intl$logRiders,main = "Distribution of All-Time Passengers",names = c("Total","Domestic","International"),col = c("gray","blue","red"))
#boxplot(intlAO[which(intlMX$Operating.Airline.IATA.Code == "UA"),]$logRiders,intlMX[which(intlMX$Operating.Airline.IATA.Code != "UA"),]$logRiders,main = "Comparing United with Other Airlines to Australia",names = c("United","Other"),col = c("blue","gray"))

##########################################
# Try a linear fit function with Y=a+b*X #plo
##########################################

#model1=lm(df$riders~df$Month,data=df)
model1All = lm(logRiders~isMex+isAust+isCentral+isME+Year+Month+depart+arrive,data=intl)

model1Dom=lm(dom$logRiders~dom$Month+Year+depart+arrive+exp+cheap,data=dom)
#model1DomUtd=lm(logRiders~depart+arrive,data=domutd)
model1DomAk = lm(logRiders~depart+arrive,data=domak)

model1Intl=lm(intl$riders~intl$Month,data=intl)
#model1Come=lm(come$riders~come$Month,data=come)
#model1Go=lm(go$riders~go$Month,data=go)
#model1DomGo = lm(domGo$riders~domGo$Month,data=domGo)
#model1IntlCome = lm(intlCome$riders~intlCome$Month,data=intlCome)
#model1High = lm(high$riders~high$Month,data=high)
#model1Low = lm(low$riders~low$Month,data=low)
###################################################
# Plot the linear function obtained by regression #
###################################################

xVals=seq(min(df$Month),max(df$Month), by=1)
xValsDom=seq(min(dom$Month),max(dom$Month), by=1)
xValsIntl=seq(min(intl$Month),max(intl$Month), by=1)
xValsCome=seq(min(come$Month),max(come$Month), by=1)
xValsGo=seq(min(go$Month),max(go$Month), by=1)

#xValsDomGo=seq(min(domGo$Month),max(domGo$Month), by=1)
#xValsIntlCome=seq(min(intlCome$Month),max(intlCome$Month), by=1)

#lines(xVals,xVals*model1$coefficients[2]+model1$coefficients[1],col="blue")
lines(xValsDom,xValsDom*model1Dom$coefficients[2]+model1Dom$coefficients[1],col="blue")
#lines(xValsIntl,xValsIntl*model1Intl$coefficients[2]+model1Intl$coefficients[1],col="blue")
#lines(xValsCome,xValsCome*model1Come$coefficients[2]+model1Come$coefficients[1],col="blue")
#lines(xValsGo,xValsGo*model1Go$coefficients[2]+model1Go$coefficients[1],col="blue")
#lines(xValsDomGo,xValsDomGo*model1DomGo$coefficients[2]+model1DomGo$coefficients[1],col="blue")
#lines(xValsIntlCome,xValsIntlCome*model1IntlCome$coefficients[2]+model1IntlCome$coefficients[1],col="blue")
#lines(xVals,xVals*model1High$coefficients[2] + model1High$coefficients[1],col = "blue")
#lines(xVals,xVals*model1Low$coefficients[2] + model1Low$coefficients[1],col = "blue")
#lines(xVals,xVals*model1DomUtd$coefficients[2]+model1DomUtd$coefficients[1],col="blue")
lines(xVals,xVals*model1DomAk$coefficients[2]+model1DomAk$coefficients[1],col="blue")

print("got blue line")
##############################################################
# Find the correlation coefficient between X and Y - its     #
# value is close to 0 indicating a weak or absent linear     #
#relationship between X and Y                                #
#Notice the two lines of code do the same thing since the    #
#dataframe has only numeric variables.                       #
##############################################################

corr <- cor(df$Month,df$riders,method="pearson")
corrDom <- cor(dom$Month,dom$riders,method="pearson")
corrIntl <- cor(intl$Month,intl$riders,method="pearson")
corrCome <- cor(come$Month,come$riders,method = "pearson")
corrGo <- cor(go$Month,go$riders,method = "pearson")
corrDomUtd <- cor(domutd$depart+domutd$arrive,domutd$logRiders,method = "pearson")
corrDomAk <-cor(domak$depart+domak$arrive,domak$riders,method = "pearson")

##############################################################
# Read features of the model including the coeffients.       #
# Notice the very low "Multiple R-Squared" value, indicating #
# a poor fit                                                 #      
##############################################################

summary(model1)
#summary(model1Dom)
#summary(model1Intl)
#summary(model1Come)
#summary(model1Go)
#summary(model1DomGo)
#summary(model1IntlCome)
#summary(model1High)
#summary(model2High)
#summary(model1DomUtd)
summary(model1DomAk)
#plot(fitted(model1DomUtd), residuals(model1DomUtd), xlab = "Fitted Values of Activity Period Status", ylab = "Residuals of Passengers", main = "Residuals of Passengers for Domestic Flights on United Airlines")
#############################################################
# Since the points look parabolic, consider incorporating   #
# the X's squares - you need to add these to the data frame #
#############################################################
df$YrSq=df$Month^2
dom$YrSq=dom$Month^2
intl$YrSq=intl$Month^2
come$YrSq=come$Month^2
go$YrSq=go$Month^2
domak$monSq = domak$Month^2
#domutd$MonSq = domutd$Month^2
#domGo$YrSq=domGo$Month^2
#intlCome$YrSq=intlCome$Month^2


##################################################################
# Make a new model incorporating the original values and squares #
# Y = a + b*X+c*XSquared                                         #
##################################################################
model2<-lm(df$riders~df$Month+df$YrSq,data=df)
model2Dom<-lm(dom$logRiders~dom$Month+dom$YrSq,data=dom)
model2Intl<-lm(intl$riders~intl$Month+intl$YrSq,data=intl)
model2Come<-lm(come$riders~come$Month+come$YrSq,data=come)
model2Go<-lm(go$riders~go$Month+go$YrSq,data=go)
#model2DomUtd = lm(domutd$riders~domutd$Month+domutd$MonSq,data=domutd)
model2DomAk = lm(domak$riders~domak$Month+domak$MonSq,data=domak)
#model2DomGo <- lm(domGo$riders~domGo$Month+df$YrSq,data=domGo)
#model2IntlCome <- lm(intlCome$riders~intlCome$Month+df$YrSq,data=intlCome)

############################################
# Plot the points again, add the new model #
############################################

#lines(xVals,model2$coefficients[1]+xVals*model2$coefficients[2]+xVals^2*model2$coefficients[3],col="red")
#lines(xValsDom,model2Dom$coefficients[1]+xValsDom*model2Dom$coefficients[2]+xValsDom^2*model2Dom$coefficients[3],col="red")
#lines(xValsIntl,model2Intl$coefficients[1]+xValsIntl*model2Intl$coefficients[2]+xValsIntl^2*model2Intl$coefficients[3],col="red")
#lines(xValsCome,model2Come$coefficients[1]+xValsCome*model2Come$coefficients[2]+xValsCome^2*model2Come$coefficients[3],col="red")
#lines(xValsGo,model2Go$coefficients[1]+xValsGo*model2Go$coefficients[2]+xValsGo^2*model2Go$coefficients[3],col="red")
#lines(xVals,model2DomGo$coefficients[1]+xVals*model2DomGo$coefficients[2]+xValsDomGo^2*model2DomGo$coefficients[3],col="red")
#lines(xValsIntlCome,model2IntlCome$coefficients[1]+xValsIntlCome*model2IntlCome$coefficients[2]+xValsIntlCome^2*model2IntlCome$coefficients[3],col="red")
#lines(xVals,model2DomUtd$coefficients[1]+xVals*model2DomUtd$coefficients[2]+xVals^2*model2DomUtd$coefficients[3],col="red")
lines(xVals,model2DomAk$coefficients[1]+xVals*model2DomAk$coefficients[2]+xVals^2*model2DomAk$coefficients[3],col="red")

print("got red line")
################################################################
# Read features of the degree two polynomial linear regression #
# Notice that the value of the R-Squared value is much closer  #
# to 1, indicating a near-perfect match between model and data #
################################################################

#summary(model2)
#summary(model2Dom)
#summary(model2Intl)
#summary(model2Come)
#summary(model2Go)
#summary(model2DomGo)
#summary(model2IntlCome)
#summary(model2DomUtd)
summary(model2DomAk)

max_riders <- max(intl$riders)
y_breaks <- seq(0, max_riders + 1000, by = 2000) # Adjust by value based on your data range
axis(2, at = y_breaks, labels = y_breaks / 1000) # Display labels in thousands

# Create the line graph
ggplot(df, aes(x = Years, y = df$riders)) +
  geom_line() +
  # Customize the x-axis with a function (e.g., log transformation)
  scale_x_continuous(trans = "log10",
                     breaks = c(1, 10, 100, 1000), # Example breaks
                     labels = function(x) paste0("Log(", x, ")")) + # Custom labels
  # Customize the y-axis with a function (e.g., custom formatting)
  scale_y_continuous(labels = function(y) paste0(y/1000, "K")) + # Example: show values in thousands
  labs(title = "Your Line Graph Title",
       x = "Your X-Axis Label",
       y = "Your Y-Axis Label") +
  theme_minimal() # Optional: use a minimalist theme

austdata = df %>%
  filter(isSA == 1) %>%
  #filter(Year > 2013) %>%
  
  group_by(Operating.Airline) %>%
  summarise(Total_Passengers = sum(riders, na.rm=TRUE),
            .groups = 'drop') %>%
  arrange(desc(Total_Passengers))%>%
  
  mexdata = df %>%
  filter(GEO.Region == "Mexico") %>%
  #filter(Year > 2013) %>%
  
  group_by(Operating.Airline) %>%
  summarise(Total_Passengers = sum(riders, na.rm=TRUE),
            .groups = 'drop') %>%
  arrange(desc(Total_Passengers))%>%
  
  ladata = df %>%
  filter(GEO.Region == "Latin America") %>%
  #filter(Year > 2013) %>%
  
  group_by(Operating.Airline) %>%
  summarise(Total_Passengers = sum(riders, na.rm=TRUE),
            .groups = 'drop') %>%
  arrange(desc(Total_Passengers))%>%
  
  travel_month = df%>%
  filter(GEO.Region == "Latin America") %>%
  group_by(Month) %>%
  summarise(Total_Passengers = sum(riders, na.rm=TRUE),
            Avg_Passengers = mean(riders, na.rm=TRUE),
            Flight_Count = n(),) %>%
  arrange(desc(Avg_Passengers))%>%
  
  #########################################
# Store the sample data in a data frame #
#########################################
barplot(ladata$Total_Passengers,names.arg = ladata$Operating.Airline,yaxt="n", ylab="Passenger Count in Thousands", main="Top 10 Airlines to and from Australia",col=rainbow(10),las=2)
yaxt="n"



####################
# Plot the points. #
####################

#plot(df$Month,df$riders)
#plot(intl$Month,intl$riders)
#plot(come$Month,come$riders)
#plot(go$Month,go$riders)
#plot(domak$Year+domak$Month,domutd$logRiders)
#plot(domak$depart + domak$arrive + domak$Month+domak$Year,domak$logRiders)
#plot(intl$isMex+intl$isAust+intl$isCentral+intl$isME+intl$Year+intl$Month+intl$depart+intl$arrive,intl$logRiders)
#boxplot(df$logRiders,dom$logRiders,intl$logRiders,main = "Distribution of All-Time Passengers",names = c("Total","Domestic","International"),col = c("gray","blue","red"))
#boxplot(intlAO[which(intlMX$Operating.Airline.IATA.Code == "UA"),]$logRiders,intlMX[which(intlMX$Operating.Airline.IATA.Code != "UA"),]$logRiders,main = "Comparing United with Other Airlines to Australia",names = c("United","Other"),col = c("blue","gray"))

##########################################
# Try a linear fit function with Y=a+b*X #plo
##########################################

#model1=lm(df$riders~df$Month,data=df)
model1All = lm(logRiders~isMex+isAust+isCentral+isME+Year+Month+depart+arrive,data=intl)

model1Dom=lm(dom$logRiders~dom$Month+Year+depart+arrive+exp+cheap,data=dom)
#model1DomUtd=lm(logRiders~depart+arrive,data=domutd)
model1DomAk = lm(logRiders~depart+arrive,data=domak)

model1Intl=lm(intl$riders~intl$Month,data=intl)
#model1Come=lm(come$riders~come$Month,data=come)
#model1Go=lm(go$riders~go$Month,data=go)
#model1DomGo = lm(domGo$riders~domGo$Month,data=domGo)
#model1IntlCome = lm(intlCome$riders~intlCome$Month,data=intlCome)
#model1High = lm(high$riders~high$Month,data=high)
#model1Low = lm(low$riders~low$Month,data=low)
###################################################
# Plot the linear function obtained by regression #
###################################################

xVals=seq(min(df$Month),max(df$Month), by=1)
xValsDom=seq(min(dom$Month),max(dom$Month), by=1)
xValsIntl=seq(min(intl$Month),max(intl$Month), by=1)
xValsCome=seq(min(come$Month),max(come$Month), by=1)
xValsGo=seq(min(go$Month),max(go$Month), by=1)

#xValsDomGo=seq(min(domGo$Month),max(domGo$Month), by=1)
#xValsIntlCome=seq(min(intlCome$Month),max(intlCome$Month), by=1)

#lines(xVals,xVals*model1$coefficients[2]+model1$coefficients[1],col="blue")
lines(xValsDom,xValsDom*model1Dom$coefficients[2]+model1Dom$coefficients[1],col="blue")
#lines(xValsIntl,xValsIntl*model1Intl$coefficients[2]+model1Intl$coefficients[1],col="blue")
#lines(xValsCome,xValsCome*model1Come$coefficients[2]+model1Come$coefficients[1],col="blue")
#lines(xValsGo,xValsGo*model1Go$coefficients[2]+model1Go$coefficients[1],col="blue")
#lines(xValsDomGo,xValsDomGo*model1DomGo$coefficients[2]+model1DomGo$coefficients[1],col="blue")
#lines(xValsIntlCome,xValsIntlCome*model1IntlCome$coefficients[2]+model1IntlCome$coefficients[1],col="blue")
#lines(xVals,xVals*model1High$coefficients[2] + model1High$coefficients[1],col = "blue")
#lines(xVals,xVals*model1Low$coefficients[2] + model1Low$coefficients[1],col = "blue")
#lines(xVals,xVals*model1DomUtd$coefficients[2]+model1DomUtd$coefficients[1],col="blue")
lines(xVals,xVals*model1DomAk$coefficients[2]+model1DomAk$coefficients[1],col="blue")

print("got blue line")
##############################################################
# Find the correlation coefficient between X and Y - its     #
# value is close to 0 indicating a weak or absent linear     #
#relationship between X and Y                                #
#Notice the two lines of code do the same thing since the    #
#dataframe has only numeric variables.                       #
##############################################################

corr <- cor(df$Month,df$riders,method="pearson")
corrDom <- cor(dom$Month,dom$riders,method="pearson")
corrIntl <- cor(intl$Month,intl$riders,method="pearson")
corrCome <- cor(come$Month,come$riders,method = "pearson")
corrGo <- cor(go$Month,go$riders,method = "pearson")
corrDomUtd <- cor(domutd$depart+domutd$arrive,domutd$logRiders,method = "pearson")
corrDomAk <-cor(domak$depart+domak$arrive,domak$riders,method = "pearson")

##############################################################
# Read features of the model including the coeffients.       #
# Notice the very low "Multiple R-Squared" value, indicating #
# a poor fit                                                 #      
##############################################################

summary(model1)
#summary(model1Dom)
#summary(model1Intl)
#summary(model1Come)
#summary(model1Go)
#summary(model1DomGo)
#summary(model1IntlCome)
#summary(model1High)
#summary(model2High)
#summary(model1DomUtd)
summary(model1DomAk)
#plot(fitted(model1DomUtd), residuals(model1DomUtd), xlab = "Fitted Values of Activity Period Status", ylab = "Residuals of Passengers", main = "Residuals of Passengers for Domestic Flights on United Airlines")
#############################################################
# Since the points look parabolic, consider incorporating   #
# the X's squares - you need to add these to the data frame #
#############################################################
df$YrSq=df$Month^2
dom$YrSq=dom$Month^2
intl$YrSq=intl$Month^2
come$YrSq=come$Month^2
go$YrSq=go$Month^2
domak$monSq = domak$Month^2
#domutd$MonSq = domutd$Month^2
#domGo$YrSq=domGo$Month^2
#intlCome$YrSq=intlCome$Month^2


##################################################################
# Make a new model incorporating the original values and squares #
# Y = a + b*X+c*XSquared                                         #
##################################################################
model2<-lm(df$riders~df$Month+df$YrSq,data=df)
model2Dom<-lm(dom$logRiders~dom$Month+dom$YrSq,data=dom)
model2Intl<-lm(intl$riders~intl$Month+intl$YrSq,data=intl)
model2Come<-lm(come$riders~come$Month+come$YrSq,data=come)
model2Go<-lm(go$riders~go$Month+go$YrSq,data=go)
#model2DomUtd = lm(domutd$riders~domutd$Month+domutd$MonSq,data=domutd)
model2DomAk = lm(domak$riders~domak$Month+domak$MonSq,data=domak)
#model2DomGo <- lm(domGo$riders~domGo$Month+df$YrSq,data=domGo)
#model2IntlCome <- lm(intlCome$riders~intlCome$Month+df$YrSq,data=intlCome)

############################################
# Plot the points again, add the new model #
############################################

#lines(xVals,model2$coefficients[1]+xVals*model2$coefficients[2]+xVals^2*model2$coefficients[3],col="red")
#lines(xValsDom,model2Dom$coefficients[1]+xValsDom*model2Dom$coefficients[2]+xValsDom^2*model2Dom$coefficients[3],col="red")
#lines(xValsIntl,model2Intl$coefficients[1]+xValsIntl*model2Intl$coefficients[2]+xValsIntl^2*model2Intl$coefficients[3],col="red")
#lines(xValsCome,model2Come$coefficients[1]+xValsCome*model2Come$coefficients[2]+xValsCome^2*model2Come$coefficients[3],col="red")
#lines(xValsGo,model2Go$coefficients[1]+xValsGo*model2Go$coefficients[2]+xValsGo^2*model2Go$coefficients[3],col="red")
#lines(xVals,model2DomGo$coefficients[1]+xVals*model2DomGo$coefficients[2]+xValsDomGo^2*model2DomGo$coefficients[3],col="red")
#lines(xValsIntlCome,model2IntlCome$coefficients[1]+xValsIntlCome*model2IntlCome$coefficients[2]+xValsIntlCome^2*model2IntlCome$coefficients[3],col="red")
#lines(xVals,model2DomUtd$coefficients[1]+xVals*model2DomUtd$coefficients[2]+xVals^2*model2DomUtd$coefficients[3],col="red")
lines(xVals,model2DomAk$coefficients[1]+xVals*model2DomAk$coefficients[2]+xVals^2*model2DomAk$coefficients[3],col="red")

print("got red line")
################################################################
# Read features of the degree two polynomial linear regression #
# Notice that the value of the R-Squared value is much closer  #
# to 1, indicating a near-perfect match between model and data #
################################################################

#summary(model2)
#summary(model2Dom)
#summary(model2Intl)
#summary(model2Come)
#summary(model2Go)
#summary(model2DomGo)
#summary(model2IntlCome)
#summary(model2DomUtd)
summary(model2DomAk)

max_riders <- max(intl$riders)
y_breaks <- seq(0, max_riders + 1000, by = 2000) # Adjust by value based on your data range
axis(2, at = y_breaks, labels = y_breaks / 1000) # Display labels in thousands
