#lets now get the wd done
getwd()
setwd("/Users/jemy/Desktop/advanced statistics.csv")

setwd("C:/Users/MyThinkpad/Desktop/Final Project/SF CRIMES!")


#libraries i will use below
install.packages("viridis")
install.packages("treemap")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("plyr")
install.packages("tidyverse")
install.packages("IPMRF")
install.packages("leaflet")
install.packages("hrbrthemes")
install.packages("readr")


library(hrbrthemes)
library(ggplot2)
library(viridis)
library(treemap)
library(hrbrthemes)
library('dplyr')
library('plyr')
library('ggplot2')
library(tidyverse)
library(tidyverse)
library(readr)
library(lubridate)
library(IPMRF)
library("leaflet")
library(treemap)
library(readr)



#set the column types for lubridate function to change date to posixct
coltypes <-
  list(Dates = col_datetime("%Y-%m-%d %H:%M:%S"))

#read the file
crime <-
  read_csv(file="train.csv",
           col_types=coltypes)

#making new variables out of the date into month, day of week, year and hour
crime <-
  crime %>%
  mutate(Year  = factor(year(Dates), levels=2003:2015),
         Month = factor(month(Dates), levels=1:12),
         Day   = day(Dates),
         Hour  = factor(hour(Dates), levels=0:23),
         dayDate = as.POSIXct(round(Dates, units = "days")),
         DayOfWeek = factor(DayOfWeek, levels=c("Monday",
                                                "Tuesday",
                                                "Wednesday",
                                                "Thursday",
                                                "Friday",
                                                "Saturday",
                                                "Sunday"))
  )

#removing 2015.. no good... missing data
crime <- crime[ grep("2015", crime$Year, invert = TRUE) , ]

#factorise and find out how many levels there are of crime type
crime$Category<-as.factor(crime$Category)
levels(crime$Category) #39.. way too many

###############lets get top 10 crimes, using frequency and subjectivity as a deciding factor

#figuring out top 10 crime types
table(crime$Category)
crime_category <- table(crime$Category)
crime_category = as.data.frame(crime_category)

#naming convention of columns
names(crime_category)[1] = 'Crime' 
names(crime_category)[2] = 'Freq' 

#list below <- top 10 and removed silly  ones.
#LARCENY/THEFT
#ASSAULT
#DRUG/NARCOTIC	
#VEHICLE THEFT	
#VANDALISM	
#WARRANTS
#BURGLARY
#ROBBERY	
#MISSING PERSON	
#FRAUD

#curated below. 
crime_curated <- filter(crime, Category %in% c("LARCENY/THEFT", "ASSAULT", "DRUG/NARCOTIC",	"VEHICLE THEFT", "VANDALISM","WARRANTS", "BURGLARY", "ROBBERY", "MISSING PERSON", "FRAUD"))

#i want to write this for future reference if required. 
write.csv(crime_curated, "crime_curated2.csv")
crime_curated <- read.csv("crime_curated2.csv")

#lets get the 
#1) count of crimes taking placet total year by year 
#2) and then get the SD mean and 
#3) test for normality and see 
#% increase/decrease




#lets make a subset of crimes from 2010-2015.. WHY? too much data to handle man
Year_2010_2014 = subset(crime, Year == "2010"| Year == "2011" | Year == "2012"
                        | Year == "2013"
                        | Year == "2014")


#just 2014 for future ref.
year2014  <- subset(crime, Year == "2014")

#lets make some fancy plots!!
plot_crimes_2010_2014 <- ggplot(data = Year_2010_2014) +
  geom_bar(mapping = aes(x = Year_2010_2014$Category)) +
  facet_wrap(~ Year, nrow = 1) +
  coord_flip() +
  xlab("Crime Types") +
  ylab("Freq")


#print that plot boi!
print(plot_crimes_2010_2014)



#############################################

####lets get % increase and decrease from each year###
(total_crime_year$Freq[total_crime_year$Year == "2011"] - total_crime_year$Year == "2010")

#growth % between 2010 and 2011
((total_crime_year$Freq[total_crime_year$Year == "2011"] - 
    total_crime_year$Freq[total_crime_year$Year == "2010"]) / 
    total_crime_year$Freq[total_crime_year$Year == "2010"])*100

##0.1157164%##

#growth % between 2011 and 2012

((total_crime_year$Freq[total_crime_year$Year == "2012"] - 
    total_crime_year$Freq[total_crime_year$Year == "2011"]) / 
    total_crime_year$Freq[total_crime_year$Year == "2011"])*100

##7.673487##

#growth % between 2012 and 2013

((total_crime_year$Freq[total_crime_year$Year == "2013"] - 
    total_crime_year$Freq[total_crime_year$Year == "2012"]) / 
    total_crime_year$Freq[total_crime_year$Year == "2012"])*100

##5.402127%##


#growth % between 2013 and 2014

((total_crime_year$Freq[total_crime_year$Year == "2014"] - 
    total_crime_year$Freq[total_crime_year$Year == "2013"]) / 
    total_crime_year$Freq[total_crime_year$Year == "2013"])*100

##-1.111023%## #definitely easier ways of doing this..


######lets now do some plats and more stats
crime_curated <- filter(crime_curated, !grepl("2015",crime_curated$Year))

#plotting years against category
ggplot(data = filter(crime_curated)) +
  geom_bar(mapping = aes(x = Year, fill = Category)) +
  xlab("Month") +
  ylab("Count")


##some stats on crime per year
table_crimes_per_year <- table(Year_2010_2014$Year)
table_crimes_per_year = as.data.frame(table_crimes_per_year)

#some stats on crime per year
summary(table_crimes_per_year$Freq)
sd(table_crimes_per_year$Freq) #4329.704
mean(table_crimes_per_year$Freq) #71052.8 mean number of crimes that occur each year
shapiro.test(table_crimes_per_year$Freq) #normally distributed p is less than alpha


#testing for normality again
plot(table_crimes_per_year$Freq)
qqnorm(table_crimes_per_year$Freq)
qqline(table_crimes_per_year$Freq)


#lets make a table out of it to make it more managebale TOTAL CRIMES
total_crime_year <- table(crime$Year)
total_crime_year = as.data.frame(total_crime_year)
names(total_crime_year)[1] = 'Year'


###getting rid of 2015 as barely any data on it
total_crime_year<- total_crime_year[ grep("2015", total_crime_year$Year, invert = TRUE) , ]

total_crime_year$Year <- as.integer(total_crime_year$Year)
total_crime_year$Freq <- as.numeric(total_crime_year$Freq)

#lets now make a line chart to show increase and decrease in crimes over the years..
p1 <- ggplot() + geom_line(aes(y = Freq, x = Year),
                           data = total_crime_year)
#print the plot!
p1




###multiple linear regression to predict the number of crimes that will occur in 2015.

#subset first
crime_year <- table(crime$Year)
crime_year = as.data.frame(crime_year)
names(crime_year)[1] = 'Year'

#remove 2015
crime_year <- crime_year[-13, ]

#class checking
class(crime_year$Var1)
class(crime_year$Freq)

#converting it to numeric
crime_year$Year <- as.numeric(crime_year$Year)

#lets do a simple linear regression to find out what the number of crimes will be in 2015 and 2016
Model1 <- lm(crime_year$Freq ~ crime_year$Year)
Model1 
summary(Model1)

#Coefficients:
#(Intercept)  crime_year$Year  
#70558.88            48.19 

#formula is Freq = 70558.88 + 48.19*year
plot(crime_year$Freq ~ crime_year$Year)
abline(Model1)


#lets now do a simple calculation

#2015 = 13
#2016 = 14

x <- 13
y <- 14

x1 = 70558.88 + 48.19*x
print(x1) #71185.35

y1 = 70558.88 + 48.19*y
print(y1) #71233.54

##increasing trend

####what type of crime happens most frequently in a specific month?

#making a table out of category and month
category_month <- table(crime_curated$Category, crime_curated$Month)
category_month = as.data.frame(category_month)
names(category_month)[1] = 'Category' #naming
names(category_month)[2] = 'Month' #naming

####lets group them now!
groupedcatmonth <- category_month %>%
  group_by(Category, Month) %>%
  summarise(Freq = sum(Freq))
            
#############################################


#table of month and year to find relationship between the two
table(crime_curated$Month, crime_curated$Year)

#
bymonth <- table(crime_curated$Month, crime_curated$Year)
bymonth = as.data.frame(bymonth)
names(bymonth)[1] = 'Month' 
names(bymonth)[2] = 'Year' 
names(bymonth)[3] = 'Freq' 

###sepereated by months for all crimes - 2015
ggplot(data = filter(crime)) +
  geom_bar(mapping = aes(x = Month, fill = Category)) +
  xlab("Month") +
  ylab("Count")


#lets make this into a table.. i took month as a way of grouping as crime in may in 2014 and 2013 can be easily compared etc..
bymonth$Year <- as.factor(bymonth$Year)
bymonth$Year = factor(bymonth$Year)

#boxplots of crimes occuring per month over the years.. We can see that January and july are strange months. Theres obviously very high and low 
# values meaning that in some years there has been in a lot more and a lot less crimes in these months.. 
boxplot(bymonth$Freq ~ Month, data=bymonth)

##shapiro testing for normality
shapiro.test(bymonth$Freq) #0.2515 relative normal still
summary(bymonth$Freq) 

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2741    3385    3650    3689    3973    4772

#lets now run an anova test
m1 <- aov(bymonth$Freq ~ bymonth$Year, bymonth$Month)
summary(m1)

#p is less than our alpha... the year has a significant impact on the number of crimes that will take place in a specific month. This may explain the great variations in january and in other months. 


##lets have a deeperdive in to january over the years.
january <- subset(bymonth, Month == "January")
shapiro.test(january$Freq) #0.0627 its not significant but low

#visulasing via boxplot
boxplot(january$Freq)
summary(january$Freq)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#3072    3252    3574    3481    3636    3761 

##crime_curated lets do the same for top 10 crimes now

ggplot(data = filter(crime_curated)) +
  geom_bar(mapping = aes(x = Month, fill = Category)) +
  xlab("Month") +
  ylab("Count")


#########lets do one just grouped by hour only...(first version)

#testing 
crimes_per_hour_dayx <- table(crime$Hour)
crimes_per_hour_dayx = as.data.frame(crimes_per_hour_dayx)
names(crimes_per_hour_dayx)[1] = 'Hour' 

#plot to visualise
boxplot(Freq ~ Hour, data=crimes_per_hour_dayx)

# Plot
crimes_per_hour_dayx %>%
  tail(24) %>%
  ggplot( aes(x=Hour, y=Freq)) +
  geom_line() +
  geom_point()


#
#lets get just do hours in general (second version)
crimes_per_hour <- table(crime$Hour)
crimes_per_hour = as.data.frame(crimes_per_hour)
names(crimes_per_hour)[1] = 'Hour' 

#factorise them 
crimes_per_hour$Freq <- as.numeric(crimes_per_hour$Freq)
crimes_per_hour$Hour <- as.integer(crimes_per_hour$Hour)

#ggplotting
p2 <- ggplot() + geom_line(aes(y = Freq, x = Hour),
                           data = crimes_per_hour)
p2

#statistics
sd(crimes_per_hour$Freq) #14059
shapiro.test(crimes_per_hour$Freq) #p vlaue is 0.01408 - not normally distributed, below alpha 0.05


###################

#lets find out if Is there a significant difference in the number of crimes occurring per hour in different months of the year
crimes_per_hour_day <- table(crime$Hour, crime$DayOfWeek)
crimes_per_hour_day = as.data.frame(crimes_per_hour_day)
names(crimes_per_hour_day)[1] = 'Hour' 
names(crimes_per_hour_day)[2] = 'Day' 
names(crimes_per_hour_day)[3] = 'Freq' 


##factorising them
crimes_per_hour_day$Hour <- as.factor(crimes_per_hour_day$Hour)
crimes_per_hour_day$Day = factor(crimes_per_hour_day$Day)

#crimes occuring
boxplot(Freq ~ Hour, data=crimes_per_hour_day)


#lets now run a 
m1 <- aov(crimes_per_hour_day$Freq ~ crimes_per_hour_day$Hour)
summary(m1)

#clearly the null hyptothsis that hours have a significant impact on the number of crimes occuring can be rejected

##hour and month
crime_hour_month <- table(crime$Hour, crime$Month)
crime_hour_month = as.data.frame(crime_hour_month)
names(crime_hour_month)[1] = 'Hour' 
names(crime_hour_month)[2] = 'Month' 
names(crime_hour_month)[3] = 'Freq' 

####hourlycrimesper month
ggplot(data = filter(crime)) +
  geom_bar(mapping = aes(x = crime$Hour)) +
  facet_wrap(~crime$Month) +
  xlab("Hour") +
  ylab("Count") +
  ggtitle("Hourly Crime Rates by the Month") +
  theme(plot.title = element_text(hjust = .5))


##lets do one more with hour and category
crime_hour_category <- table(crime_curated$Hour, crime_curated$Category)
crime_hour_category = as.data.frame(crime_hour_category)
names(crime_hour_category)[1] = 'Hour' 
names(crime_hour_category)[2] = 'Category' 
names(crime_hour_category)[3] = 'Freq' 

#for ref
write.csv(crime_hour_category, "crime_hour_category.csv")

####visualising this##absolutely beautiful!
ggplot(crime_hour_category, aes(fill=Category, y=Freq, x=Hour)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Crimes Per Hour/Category") +
  theme_ipsum() +
  xlab("")

#lets investigate hour 18 a little.. 
hour_1800 <- subset(crime_hour_category, Hour == "18")

#pie chart of the crimes that happen at 18:00 o clock. 
ggplot(hour_1800, aes(x="", y=Freq, fill=Category)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

##lets figure out which crimes are most likely to occur per hour
LARCENY_THEFT <- subset(crime_hour_category, Category == "LARCENY/THEFT")
ASSAULT <- subset(crime_hour_category, Category == "ASSAULT")
DRUG_NARCOTIC <- subset(crime_hour_category, Category == "DRUG/NARCOTIC")
VEHICLE_THEFT <- subset(crime_hour_category, Category == "VEHICLE THEFT")
VANDALISM <- subset(crime_hour_category, Category == "VANDALISM")
BURGLARY <- subset(crime_hour_category, Category == "BURGLARY")
WARRANTS <- subset(crime_hour_category, Category == "WARRANTS")
ROBBERY <- subset(crime_hour_category, Category == "ROBBERY")
MISSING_PERSON <- subset(crime_hour_category, Category == "MISSING PERSON")
FRAUD <- subset(crime_hour_category, Category == "FRAUD")


#shapiro testing each one indivdially
shapiro.test(LARCENY_THEFT$Freq)
shapiro.test(ASSAULT$Freq) #not normal
shapiro.test(DRUG_NARCOTIC$Freq)
shapiro.test(VEHICLE_THEFT$Freq)
shapiro.test(VANDALISM$Freq)
shapiro.test(BURGLARY$Freq)
shapiro.test(ROBBERY$Freq)
shapiro.test(MISSING_PERSON$Freq) #not normal
shapiro.test(FRAUD$Freq) #not normal

#factorising
crime_hour_category$Category <- as.factor(crime_hour_category$Category)
crime_hour_category$Hour <-  as.factor(crime_hour_category$Hour)

#boxplotting
boxplot(ASSAULT$Freq ~ ASSAULT$Hour)

p <- ggplot(ASSAULT, aes(x=Hour, y=Freq)) + 
  geom_bar(stat = "identity")

print(p)

#boxplotting it 
boxplot(DRUG_NARCOTIC$Freq)
boxplot(MISSING_PERSON$Freq)

#anova testing
m6 <- aov(crime_hour_category$Freq ~ crime_hour_category$Category + crime_hour_category$Hour)
summary(m6)

#both have significant imapcts..

#shapiro testing
shapiro.test(crime_hour_category$Freq)#not normally distributed.

#post hoc test
ph1 = TukeyHSD(m6) # we can see which crime types will not likley have same means.
ph1


###this is what I am looking for. tis ok, oculd be better
# library
library(treemap)

###this will tell us the most common crime per hour
# treemap
treemap(crime_hour_category,
        index=c("Hour","Category"),
        vSize="Freq",
        type="index"
) 



####day of week####

#lets do days now
table_dayofweek <- table(crime$DayOfWeek)
table_dayofweek = as.data.frame(table_dayofweek)
names(table_dayofweek)[1] = 'Day' 
names(table_dayofweek)[2] = 'Freq' 

#plotting
p <- ggplot(table_dayofweek, aes(x=Day, y=Freq)) + 
  geom_bar(stat = "identity")

print(p)


##another barplot.. much better imo..
table_dayofweek %>%
  mutate(name = fct_reorder(Day, desc(Freq))) %>%
  ggplot( aes(x=Day, y=Freq)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()


#number of crimes happening over the days normal?
shapiro.test(table_dayofweek$Freq) #very normal

#most number of crimes are on a friday

###lets do one now with day of week and type of crime only for curated however
table_dayofweek_curated <- table(crime_curated$DayOfWeek, crime_curated$Category)
table_dayofweek_curated = as.data.frame(table_dayofweek_curated)
names(table_dayofweek_curated)[1] = 'Day' 
names(table_dayofweek_curated)[2] = 'Category' 

#love this one.. source - https://www.r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html
#this below, tells us which crime takes place most on the day

# Small multiple
ggplot(table_dayofweek_curated, aes(fill=Category, y=Freq, x=Day)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Crimes Per Day") +
  theme_ipsum() +
  xlab("")


##some statistics on this!

#group by day
daysonlycrime <- table_dayofweek_curated %>%
  group_by(Day) %>%
  summarise(Freq = sum(Freq))

#shapiro that shiii
shapiro.test(daysonlycrime$Freq) #0.8459 normal

#####location_based###

#making tables for crime and pddistrict
pd_districtcrimes <- table(crime$PdDistrict)
pd_districtcrimes = as.data.frame(pd_districtcrimes)
names(pd_districtcrimes)[1] = 'PDdistrict' 

#plotting
p <- ggplot(pd_districtcrimes, aes(x=PDdistrict, y=Freq)) + 
  geom_bar(stat = "identity")

print(p)


##this is a very nice looking graph
pd_districtcrimes %>%
  arrange(Freq) %>%
  mutate(name = factor(PDdistrict, )) %>%
  ggplot( aes(x=PDdistrict, y=Freq)) +
  geom_segment( aes(xend=name, yend=0)) +
  geom_point( size=4, color="orange") +
  theme_bw() +
  xlab("")

#lets get a treemap 
treemap(pd_districtcrimes,
        index=c("PDdistrict"),
        vSize="Freq",
        type="index"
) 

##we can clearly see that southern receives the most number of crimes and richmond the least.

####this is for pd and category of crimes######

pd_districtcrimes_cat <- table(crime_curated$PdDistrict, crime_curated$Category)
pd_districtcrimes_cat = as.data.frame(pd_districtcrimes_cat)
names(pd_districtcrimes_cat)[1] = 'PDdistrict' 
names(pd_districtcrimes_cat)[2] = 'Category' 


# bar plot of pddistrict and cat of crime
p <- ggplot(pd_districtcrimes_cat, aes(fill=Var2, y=Freq, x=PDdistrict)) + 
  geom_bar(position="stack", stat="identity")

print(p)


###i dont like this one. 

##this one is better
ggplot(pd_districtcrimes_cat, aes(fill=Var2, y=Freq, x=PDdistrict)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Crimes Per Day") +
  theme_ipsum() +
  xlab("")


##lets do some stats now..  I want to find out if the category of crimes are the same accross all districts. This is interesting to know because then we will know that there exists a similar trend of crimes taking place accorss all districts. 

BAYVIEW <- subset(pd_districtcrimes_cat, PDdistrict == "BAYVIEW")
CENTRAL <- subset(pd_districtcrimes_cat, PDdistrict == "CENTRAL")
INGLESIDE <- subset(pd_districtcrimes_cat, PDdistrict == "INGLESIDE")
MISSION <- subset(pd_districtcrimes_cat, PDdistrict == "MISSION")
NORTHERN <- subset(pd_districtcrimes_cat, PDdistrict == "NORTHERN")
PARK <- subset(pd_districtcrimes_cat, PDdistrict == "PARK")
RICHMOND <- subset(pd_districtcrimes_cat, PDdistrict == "RICHMOND")
SOUTHERN <- subset(pd_districtcrimes_cat, PDdistrict == "SOUTHERN")
TARAVAL <- subset(pd_districtcrimes_cat, PDdistrict == "TARAVAL")
TENDERLOIN <- subset(pd_districtcrimes_cat, PDdistrict == "TENDERLOIN")


#making levels..factors
LARCENY_THEFT <- subset(pd_districtcrimes_cat, Category == "LARCENY/THEFT")
ASSAULT <- subset(pd_districtcrimes_cat, Category == "ASSAULT")
DRUG_NARCOTIC <- subset(pd_districtcrimes_cat, Category == "DRUG/NARCOTIC")
VEHICLE_THEFT <- subset(pd_districtcrimes_cat, Category == "VEHICLE THEFT")
VANDALISM <- subset(pd_districtcrimes_cat, Category == "VANDALISM")
BURGLARY <- subset(pd_districtcrimes_cat, Category == "BURGLARY")
WARRANTS <- subset(pd_districtcrimes_cat, Category == "WARRANTS")
ROBBERY <- subset(pd_districtcrimes_cat, Category == "ROBBERY")
MISSING_PERSON <- subset(pd_districtcrimes_cat, Category == "MISSING PERSON")
FRAUD <- subset(pd_districtcrimes_cat, Category == "FRAUD")


#shapiro testing them all now
shapiro.test(BAYVIEW$Freq)
shapiro.test(CENTRAL$Freq)
shapiro.test(INGLESIDE$Freq)
shapiro.test(MISSION$Freq)
shapiro.test(NORTHERN$Freq)
shapiro.test(RICHMOND$Freq)
shapiro.test(TARAVAL$Freq)
shapiro.test(SOUTHERN$Freq)
shapiro.test(TENDERLOIN$Freq)

#some are greater and some are less than alpha. for this reason, i will go with kruskal

#lets factorise
pd_districtcrimes_cat$PDdistrict <- as.factor(pd_districtcrimes_cat$PDdistrict)
pd_districtcrimes_cat$Category <- as.factor(pd_districtcrimes_cat$Category)


#lets do a kruskal to find out
kruskal.test(pd_districtcrimes_cat$Freq ~ pd_districtcrimes_cat$PDdistrict, pd_districtcrimes_cat$Category) #0.02941 less than our alpha at 0.05. Which means, that the mean number of crimes comitted per category may differ across each district

#now lets find out size effect between groups with a post hoc test

install.packages("FSA")
library("FSA")

dunnTest(pd_districtcrimes_cat$Freq ~ pd_districtcrimes_cat$PDdistrict, pd_districtcrimes_cat$Category) #0.02941 less than our alpha at 0.05. Which means, that the mean number of crimes comitted per category may differ across each district
#i can only see the differnce in groups in district.. this isn't what I was quite looking for.. i will attempt a twoway anova instead. However, there are some insights.. 

#anova testing now
m3 <- aov(pd_districtcrimes_cat$Freq ~ pd_districtcrimes_cat$Category + pd_districtcrimes_cat$PDdistrict)
summary(m3)



#                          Df    Sum Sq   Mean Sq F value   Pr(>F)    
#pd_districtcrimes_cat$Category    9 1.716e+09 190688098  13.856 3.26e-13 ***
#pd_districtcrimes_cat$PDdistrict  9 3.617e+08  40192669   2.921  0.00478 ** 
#Residuals                        81 1.115e+09  13761968       


##this is interesting.. we can see that both district and cateogory have a significant impact size on the number 
#of crimes that will take place and that the means are not likley to be equal.. This suggests that the number of crimes per category will differ per district

ph1 = TukeyHSD(m3, "pd_districtcrimes_cat$PDdistrict") 
ph1

#makes sense, southern and richmond are highly unlike to have equal means. 
#SOUTHERN-RICHMOND     6819.3   1421.6248 12216.9752 0.0035530


ph2 = TukeyHSD(m3, "pd_districtcrimes_cat$Category") 
ph2

##this is where it gets interesting! It appears that the means of certain category of crime types differe hugely accross districts. 
#This tells us that the crimes from fraud to assault for example do not have equal means across all the districts. And are likely to differ. 
#there appears to be discrepency between larcency and assault. This could very well be a reason as to why it was so hard for the MLS to predict category of crime type. 


##FRAUD-ASSAULT                 -5842.1 -11239.775  -444.4248 0.0233446
#LARCENY/THEFT-ASSAULT          9283.8   3886.125 14681.4752 0.0000125

###source = https://www.littlemissdata.com/blog/maps
#lets now have some maps fun!
#Load the library

install.packages("lubridate")
library("lubridate")
install.packages("ggplot2")
library("ggplot2")
install.packages("data.table")
library("data.table")
install.packages("ggrepel")
library("ggrepel")
install.packages("dplyr")
library("dplyr")
install.packages("data.table")
library("data.table")
install.packages("tidyverse")
library("tidyverse")
install.packages("devtools")

if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)

install.packages("ggmap")
library("ggmap")

#Set your API Key
ggmap::register_google(key = "xxxxxxxx")

p <- ggmap(get_googlemap(center = c(lon = -122.393990, lat = 37.781555),
                         zoom = 11, scale = 2,
                         maptype ='terrain',
                         color = 'color'))


p + geom_point(aes(x = X, y = Y,  colour = 531239), data = crime_curated, size = 0.5) + 
  theme(legend.position="bottom")


###########################################################################
###########################################################################
###########################################################################
#redundant code, did not use but would like to keep for future reference
#assaultonly2 <- subset(crime, crime$Category == "ASSAULT" | crime$Year == "2014")
#assaultonly <- subset(crime, crime$Category == "ASSAULT" | crime$Year == "2014")
############################################################################
############################################################################
############################################################################
############################################################################

#calling api.. got the key! yay.
ggmap::register_google(key = "AIzaSyARkLVdzYUmbxDt1pYNMcJcwuJQvAgb6NE")

p <- ggmap(get_googlemap(center = c(lon = -122.393990, lat = 37.781555),
                         zoom = 12, scale = 2,
                         maptype ='terrain',
                         color = 'color'))

###################
#######for assault only#########

#fpr assault only so redundant
#center = c(lon = -122.393990, lat = 37.781555)
#
#qmap(center, zoom = 12, source = "stamen", maptype = "terrain-lines") +
 # stat_density2d(
#    aes(x = X, y = Y, fill = ..level..),
 #   alpha = 0.25, size = 0.2, bins = 30, data = assaultonly,
#    geom = "polygon"
#  )  + geom_density2d(data = assaultonly, 
#                      aes(x = X, y = Y), size = 0.3) + scale_fill_gradient(low = "light blue", high= "dark blue") 



###########year2014 crimes only#####

#setting location
center = c(lon = -122.393990, lat = 37.781555)

#plotting
qmap(center, zoom = 12, source = "stamen", maptype = "terrain-lines") +
  stat_density2d(
    aes(x = X, y = Y, fill = ..level..),
    alpha = 0.25, size = 0.2, bins = 30, data = year2014,
    geom = "polygon"
  )  + geom_density2d(data = year2014, 
                      aes(x = X, y = Y), size = 0.3) + scale_fill_gradient(low = "red", high= "dark red") 




####assualt only#####

#
#center = c(lon = -122.393990, lat = 37.781555)
#
#assaultonly <- qmap(center, zoom = 12, source = "stamen", maptype = "terrain-lines") +
#  stat_density2d(
#    aes(x = X, y = Y, fill = ..level..),
#    alpha = 0.25, size = 0.2, bins = 30, data = assaultonly,
#    geom = "polygon"
#  )  + geom_density2d(data = subset_2015, 
#                      aes(x = X, y = Y), size = 0.3) + scale_fill_gradient(low = "light blue", high= "dark blue") 
##
#
#)
##

##now streets!
streetsgrouping <- table(crime_curated$Address)
streetsgrouping = as.data.frame(streetsgrouping)
names(streetsgrouping)[1] = 'Street'
streetsgrouping <- streetsgrouping[order(streetsgrouping$Freq, decreasing = TRUE),] #lets put it in order now

#lets curate top 15 to make it easier
top15streetscrime <- head(streetsgrouping,15)

#nice sideways bar plot
ggplot(top15streetscrime, aes(x=Street, y=Freq)) + 
  geom_bar(stat = "identity", width=0.2) +  coord_flip()

##lets do some more work with lat and longituted! They're so much fun!

ggplot(data = subset_2014onlycuratedcrimes) +
  geom_point(mapping = aes(x = X, y = Y), alpha = 0.1, color = 'red') +
  facet_wrap(~ Category) +
  xlab("") +
  ylab("") +
  ggtitle("Crime By Location") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())

##lets make some more manageable data
subset_2014onlycuratedcrimes <- subset(crime_curated, Year == "2014") 
write.csv(subset_2014onlycuratedcrimes, "subset_2014onlycuratedcrimes.csv")


#plotting data
ggplot(data = subset_2014onlycuratedcrimes) +
  geom_point(mapping = aes(x = X, y = Y), alpha = 0.1, color = 'red') +
  facet_wrap(~ Category) +
  xlab("") +
  ylab("") +
  ggtitle("Crime By Location") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())



###crimeyearcategory
crime_cat_year <-  table(crime_curated$Year, crime_curated$Category)
crime_cat_year = as.data.frame(crime_cat_year)
names(crime_cat_year)[1] = 'Year' 
names(crime_cat_year)[2] = 'Category' 
  
#plot of year against crime category  
ggplot(crime_cat_year, aes(fill=Category, y=Freq, x=Year)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Crimes Per Year/Category") +
  theme_ipsum() +
  xlab("")


##it looks as though vehicle theft has reduced in nimbers since 2003..

#lets investigate. 
vehicletheftyear <- subset(crime_cat_year, crime_cat_year$Category == "VEHICLE THEFT")


#removing 2015
vehicletheftyear <- vehicletheftyear[-13, ]


#shapiro testing for normality
shapiro.test(vehicletheftyear$Freq) #0.001879 not normally distributed!

vehicletheftyear$Year <- as.factor(vehicletheftyear$Year)
k2 <- kruskal.test(vehicletheftyear$Freq ~ vehicletheftyear$Year)
k2 

#despite there being a great reduction of number of vehicle thefts occuring per year, we fail to rejec tt

#lets visualise this
vehicletheftyear %>%
  mutate(name = fct_reorder(Year, desc(Freq))) %>%
  ggplot( aes(x=Year, y=Freq)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

#vehicle theft per year
vehicletheftyear$Freq[vehicletheftyear$Year == "2005"] #9111
vehicletheftyear$Freq[vehicletheftyear$Year == "2006"] #3700

(vehicletheftyear$Freq[vehicletheftyear$Year == "2006"] - vehicletheftyear$Freq[vehicletheftyear$Year == "2005"])/ vehicletheftyear$Freq[vehicletheftyear$Year == "2005"]

#-0.5938975 or -59% or 59% decrease  
#thats a drastic change in the number of vehicle thefts occuring. Very strange. 


#cleaning
vehicletheftyear$Year <- lubridate::ymd(vehicletheftyear$Year, truncated = 2L)


#time series analysis
vehicletheftyear <- data.frame(
  Year = as.Date("2015") - 0:364,
  Freq = runif(365) - seq(-140, 224)^2 / 10000
)


# Most basic bubble plot
p <- ggplot(vehicletheftyear, aes(x=Year, y=Freq)) +
  geom_line() + 
  xlab("")
p





