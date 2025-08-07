
library(ggplot2)

library(dplyr)
###########################
#Create some sample data. #
###########################

info = read.csv(file="Air_Traffic_Passenger_Statistics.csv", header=TRUE)

#info$is_international = factor(info$Geo.Summary)
info$riders = as.numeric(gsub(",","",info$Passenger.Count))
# The first 4 characters represent the Month
info$Year <- as.numeric(substr(info$Activity.Period, 1, 4))

# Extract month
# The last 2 characters represent the month
info$Month <- as.numeric(substr(info$Activity.Period, 5, 6))
info$isMex <- ifelse(info$GEO.Region == "Mexico", 1, 0)
info$isUS <- ifelse(info$GEO.Region == "US", 1, 0)
info$isSA <- ifelse(info$GEO.Region == "South America", 1, 0)
info$isCentral <- ifelse(info$GEO.Region == "Central America", 1, 0)
info$isAsia <- ifelse(info$GEO.Region == "Asia", 1, 0)
info$isME <- ifelse(info$GEO.Region == "Middle East",1,0)
info$isCan <- ifelse(info$GEO.Region == "Canada",1,0)
info$isEU <- ifelse(info$GEO.Region == "Europe",1,0)
info$isAust <- ifelse(info$GEO.Region == "Australia / Oceania",1,0)
info$depart <- ifelse(info$Activity.Type.Code == "Enplaned",1,0)
info$arrive <- ifelse(info$Activity.Type.Code == "Deplaned",1,0)
info$exp = ifelse(info$Price.Category.Code == "Other",1,0)
info$cheap = ifelse(info$Price.Category.Code == "Low Fare",1,0)
info$logRiders = log(info$riders)
#ggplot(data = info, aes(x = logRiders)) 
#geom_histogram(bins = 5, fill = "lightblue", color = "green")
#labs(title = "Histogram of log of passenger count(ggplot2)", x = "log (passenger count)", y = "Frequency")

low = info[which(info$isSA == 1),]
high = info[which(info$isAust == 1),]

dom = info[which(info$GEO.Summary == "Domestic"),]
#domGo = info[which(info$Geo.Summary == "Domestic" & info$Activity.Type.Code == "Deplaned"),]
#intlCome = info[which(info$GEO.Summary == "International" & info$Activity.Type.Code == "Enplaned"),]
intl = info[which(info$GEO.Summary == "International"),]
intlAO = intl[which(info$isAust == 1),]
intlMX = intl[which(info$isMex == 1),]
come = info[which(info$Activity.Type.Code == "Deplaned"),]
go = info[which(info$Activity.Type.Code == "Enplaned"),]
domutd = dom[which(dom$Operating.Airline == "United\ Airlines"),]
#domak = dom[info$Operating.Airline.IATA.Code == "NH"]


austdata = info %>%
  filter(GEO.Region == "Australia / Oceania") %>%
  #filter(Year > 2013) %>%
  
  group_by(Operating.Airline) %>%
  summarise(Total_Passengers = sum(riders, na.rm=TRUE),
            .groups = 'drop') %>%
  arrange(desc(Total_Passengers))
mexdata = info %>%
  filter(GEO.Region == "Mexico") %>%
  #filter(Year > 2013) %>%
  
  group_by(Operating.Airline) %>%
  summarise(Total_Passengers = sum(riders, na.rm=TRUE),
            .groups = 'drop') %>%
  arrange(desc(Total_Passengers))
travel_month = info%>%
  filter(GEO.Region == "Mexico") %>%
  group_by(Month) %>%
  summarise(Total_Passengers = sum(riders, na.rm=TRUE),
            Avg_Passengers = mean(riders, na.rm=TRUE),
            Flight_Count = n(),) %>%
  arrange(desc(Avg_Passengers))

#########################################
# Store the sample data in a data frame #
#########################################
barplot(mexdata$Total_Passengers,names.arg = mexdata$Operating.Airline,yaxt="n", ylab="Passenger Count in Thousands", main="Top 10 Airlines to and from Australia",col=rainbow(10),las=2)
yaxt="n"



####################
# Plot the points. #
####################

#plot(info$Month,info$riders)
plot(dom$Month,dom$logRiders)
#plot(intl$Month,intl$riders)
#plot(come$Month,come$riders)
#plot(go$Month,go$riders)
#plot(domGo$Month,domGo$riders)
#plot(intlCome$Month,intlCome$riders)
#plot(high$Month,high$riders)
#plot(low$Month,low$riders)
#plot(domak$Year+domak$Month,domutd$logRiders)
plot(domak$depart + domak$arrive + domak$Month+domak$Year,domak$logRiders)
#plot(intl$isMex+intl$isAust+intl$isCentral+intl$isME+intl$Year+intl$Month+intl$depart+intl$arrive,intl$logRiders)
#boxplot(info$logRiders,dom$logRiders,intl$logRiders,main = "Distribution of All-Time Passengers",names = c("Total","Domestic","International"),col = c("gray","blue","red"))
#boxplot(intlAO[which(intlMX$Operating.Airline.IATA.Code == "UA"),]$logRiders,intlMX[which(intlMX$Operating.Airline.IATA.Code != "UA"),]$logRiders,main = "Comparing United with Other Airlines to Australia",names = c("United","Other"),col = c("blue","gray"))

##########################################
# Try a linear fit function with Y=a+b*X #plo
##########################################

#model1=lm(info$riders~info$Month,data=info)
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

xVals=seq(min(info$Month),max(info$Month), by=1)
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

corr <- cor(info$Month,info$riders,method="pearson")
corrDom <- cor(dom$Month,dom$riders,method="pearson")
corrIntl <- cor(intl$Month,intl$riders,method="pearson")
corrCome <- cor(come$Month,come$riders,method = "pearson")
corrGo <- cor(go$Month,go$riders,method = "pearson")
corrDomUtd <- cor(domutd$depart+domutd$arrive,domutd$logRiders,method = "pearson")


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
info$YrSq=info$Month^2
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
model2<-lm(info$riders~info$Month+info$YrSq,data=info)
model2Dom<-lm(dom$logRiders~dom$Month+dom$YrSq,data=dom)
model2Intl<-lm(intl$riders~intl$Month+intl$YrSq,data=intl)
model2Come<-lm(come$riders~come$Month+come$YrSq,data=come)
model2Go<-lm(go$riders~go$Month+go$YrSq,data=go)
#model2DomUtd = lm(domutd$riders~domutd$Month+domutd$MonSq,data=domutd)
model2DomAk = lm(domak$riders~domak$Month+domak$MonSq,data=domak)
#model2DomGo <- lm(domGo$riders~domGo$Month+info$YrSq,data=domGo)
#model2IntlCome <- lm(intlCome$riders~intlCome$Month+info$YrSq,data=intlCome)

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
