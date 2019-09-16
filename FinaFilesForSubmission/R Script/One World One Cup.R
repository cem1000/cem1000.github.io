

#Essential code to the left. The rest is indented heavily 
#muchas gracias: https://www.statmethods.net/

                            setwd('/Users/cexsligo/Documents/Data Analytics Nci')
                            getwd () #just a reminder as I don't know how to write 
                            #code to let you access the file from the submission

install.packages ('tidyverse')  #for cleaning data
library('lubridate') # for cleaning dates. 

#This is the original file with all the resuls. 
results <-read.csv(file = "/Users/cexsligo/Documents/Data Analytics Nci/Viz Data/FIFAresults.csv", head = TRUE, sep = ',')

#Taken from https://www.kaggle.com/martj42/international-football-results-from-1872-to-2017

year <- year(dmy(results$date)) #This creates a year for every game by changing the dmy date to a year (e.g. 18-7-1930 becomes 1930)
results <- cbind(year, results) #adds a year column to the results. 
#Note, lubridate doesn't like the nineteenth century so those years aiiled to parse th 128 international matches

results <- cbind(year, results) #and adds it as a column 
                              
                              results[1:200,]
                              results$date[1:200]
                              
                              
                              worldCup <- subset(results, tournament == "FIFA World Cup") 
                              head(worldCup)
                              
                              nrow(worldCup) #how many games? 
                              str(worldCup) #story? 

write.csv (worldCup, file = 'All world cup matches.csv', row.names=FALSE)

                                #random test run
                                #play around with data (mostly deleted, but you might enjoy these)
                                ireland <- subset(worldCup, worldCup$home_team == 'Republic of Ireland' | worldCup$away_team == 'Republic of Ireland')
                                resultsNornIorn = subset(results, home_team == 'Northern Ireland' | away_team == 'Northern Ireland')

                                
                                goals <- c(worldCup$home_score, worldCup$away_score)
                                boxplot(goals)
                                
                                #This is a surprisingly tidy boxplot 
                                # which shows some of the highest scores ever


                                  
                                  # for some reason the year column got changed along the way
                                  #include the next line if necessary 
                                  
                                  worldCup$year <- year(dmy(worldCup$date)) 
                                  
                                  class(worldCup$year)
                                  #Lets find out all the teamswho have played in the world Cup. 

#First we subset all 21 world cup finals into tournaments 
#This could have been done with an obviously function

thirty <- subset(worldCup, worldCup$year == '1930')
thirtyFour <- subset(worldCup,worldCup$year == '1934')
thirtyEight <- subset(worldCup,worldCup$year == '1938')
fifty <- subset(worldCup,worldCup$year == '1950')
fiftyFour <- subset(worldCup,worldCup$year == '1954')
fiftyEight <- subset(worldCup,worldCup$year == '1958')
sixtyTwo <- subset(worldCup,worldCup$year == '1962')
sixtySix <- subset(worldCup,worldCup$year == '1966')
seventy <- subset(worldCup,worldCup$year == '1970')
seventyFour <- subset(worldCup,worldCup$year == '1974')
seventyEight <- subset(worldCup,worldCup$year == '1978')
eightyTwo <- subset(worldCup,worldCup$year == '1982')
eightySix <- subset(worldCup,worldCup$year == '1986')
ninety <- subset(worldCup,worldCup$year == '1990')
ninetyFour <- subset(worldCup,worldCup$year == '1994')
ninetyEight <- subset(worldCup,worldCup$year == '1998')
twoKTwo <- subset(worldCup,worldCup$year == '2002')
twoKSix <- subset(worldCup,worldCup$year == '2006')
twentyTen <- subset(worldCup,worldCup$year == '2010')
twentyFourteen <- subset(worldCup,worldCup$year == '2014')
twentyEighteen <- subset(worldCup,worldCup$year == '2018')

#Then we make a list of all all the worldCups       
#This list was orkword af


eachWorldCup <- list(thirty,
                     thirtyFour,
                     thirtyEight,
                     fifty, 
                     fiftyFour, 
                     fiftyEight, 
                     sixtyTwo,
                     sixtySix,
                     seventy,
                     seventyFour,
                     seventyEight,
                     eightyTwo,
                     eightySix,
                     ninety,
                     ninetyFour,
                     ninetyEight,
                     twoKTwo,
                     twoKSix,
                     twentyTen,
                     twentyFourteen,
                     twentyEighteen)

                            table(thirty$home_team, thirty$away_team)
                            
                            as.factor(worldCup$year) #change the world cup years back into a factor
                            
                            write.csv(twentyEighteen, file = '2018 world cup results.csv')

############################################################
                          
                            ############################################################
                            
                            ############################################################
# #Here is the code for generating a complete list of teams who 
# have played in any  world cup

                            teamsH <- as.vector(unique(worldCup$home_team)) #all the 'home' teams
                            teamsA <- as.vector(unique(worldCup$away_team)) #all the 'away' teams
                            teams <- c(teamsH,teamsA) #merged into one vector
                            #note that this append function doesn't work for factors, 
                            # so I had to convert the vectors to characters
                            teams <-unique(teams)

write.csv (teams, file = 'world cup participants.csv', row.names=FALSE)
#notice that this list also provides the order in which teams qualified. 

#let's write a function for this to apply to all of the tournaments
# so that we can have a list of who played in what tournament

testFunc <- function(dfrm1){
  x <- as.vector(unique(dfrm1$home_team))
  y <- as.vector(unique(dfrm1$away_team))
  z <- c(x,y)
  z <- unique(z)
}

participantsByYear <- lapply(eachWorldCup, testFunc) #apply the function to all elemnts of the list (each world cup)

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#Now it's time to get info on the qualifying tournaments using the above techniques
qualifiersAll <- subset(results, results$tournament == 'FIFA World Cup qualification') #This is all the world cup qualifiers
head(qualifiersAll)
tail(qualifiersAll)
nrow(qualifiersAll)


write.csv (qualifiersAll, file = 'All world cup qualifiers.csv', row.names=FALSE)

# for some reason the year column got changed along the way


#here's a list of all the teams who have tried to qualify for the world cup, in order:

write.csv(x, file = 'all teams qualifying for the world cup, in order.csv', row.names = F, col.names = F)
#Lets find out all the teamswho have tried to qualify for the world Cup. 

                        #First we subset all world cups into tournaments 
                        as.numeric(qualifiersAll$year)
                        class(qualifiersAll$year)
                        
                        qualifiersAll$year[20:300] #just checking to see if there were any like abandoned qualifiers during WWII 

Qthirty <- subset(qualifiersAll, qualifiersAll$year == '1930' |qualifiersAll$year =='1930')
QthirtyFour <- subset(qualifiersAll,qualifiersAll$year == '1932' | qualifiersAll$year =='1933'|qualifiersAll$year == '1934')
QthirtyEight <- subset(qualifiersAll,qualifiersAll$year== '1936' | qualifiersAll$year== '1937' | qualifiersAll$year == '1938')
Qfifty <- subset(qualifiersAll,qualifiersAll$year == '1948' | qualifiersAll$year== '1949' |qualifiersAll$year== '1950')
QfiftyFour <- subset(qualifiersAll,qualifiersAll$year == '1952' 
                     | qualifiersAll$year== '1953' |qualifiersAll$year== '1954')
QfiftyEight <- subset(qualifiersAll,qualifiersAll$year== '1956' | qualifiersAll$year==  '1957' |qualifiersAll$year== '1958')
QsixtyTwo <- subset(qualifiersAll,qualifiersAll$year == '1964' | qualifiersAll$year== '1963' |qualifiersAll$year== '1962')
QsixtySix <- subset(qualifiersAll,qualifiersAll$year == '1966' | qualifiersAll$year== '1965' | qualifiersAll$year== '1965' )
Qseventy <- subset(qualifiersAll,qualifiersAll$year == '1970'| qualifiersAll$year== '1969' | qualifiersAll$year== '1968' )
QseventyFour <- subset(qualifiersAll,qualifiersAll$year == '1974'| qualifiersAll$year== '1973' | qualifiersAll$year== '1972' )
QseventyEight <- subset(qualifiersAll,qualifiersAll$year == '1978'| qualifiersAll$year== '1977' | qualifiersAll$year== '1976' )
QeightyTwo <- subset(qualifiersAll,qualifiersAll$year == '1982'| qualifiersAll$year== '1981' | qualifiersAll$year== '1980' )
QeightySix <- subset(qualifiersAll,qualifiersAll$year == '1986'| qualifiersAll$year== '1985' | qualifiersAll$year== '1984' )
Qninety <- subset(qualifiersAll,qualifiersAll$year == '1990'| qualifiersAll$year== '1989' | qualifiersAll$year== '1988' )
QninetyFour <- subset(qualifiersAll,qualifiersAll$year == '1994'| qualifiersAll$year== '1993' | qualifiersAll$year== '1992' )
QninetyEight <- subset(qualifiersAll,qualifiersAll$year == '1998'| qualifiersAll$year== '1997' | qualifiersAll$year== '1996' )
QtwoKTwo <- subset(qualifiersAll,qualifiersAll$year == '2002'| qualifiersAll$year== '2001' | qualifiersAll$year== '2000' )
QtwoKSix <- subset(qualifiersAll,qualifiersAll$year == '2006'| qualifiersAll$year== '2005' | qualifiersAll$year== '2004' )
QtwentyTen <- subset(qualifiersAll,qualifiersAll$year == '2010'| qualifiersAll$year== '2009' | qualifiersAll$year== '2008' )
QtwentyFourteen <- subset(qualifiersAll,qualifiersAll$year == '2014'| qualifiersAll$year== '2013' | qualifiersAll$year== '2012' )
QtwentyEighteen <- subset(qualifiersAll,qualifiersAll$year == '2018'| qualifiersAll$year== '2017' | qualifiersAll$year== '2016' )

#Then we make a list of all all the worldCup qualifiers           


write.csv(Qthirty, file = 'Qthirty.csv')
write.csv(numberQualifiermatches[[2]], file = 'QthirtyFour.csv')

eachQualifier <- list(Qthirty ,
                      QthirtyFour ,
                      QthirtyEight ,
                      Qfifty ,
                      QfiftyFour ,
                      QfiftyEight ,
                      QsixtyTwo ,
                      QsixtySix ,
                      Qseventy,
                      QseventyFour ,
                      QseventyEight,
                      QeightyTwo ,
                      QeightySix ,
                      Qninety ,
                      QninetyFour ,
                      QninetyEight ,
                      QtwoKTwo ,
                      QtwoKSix ,
                      QtwentyTen ,
                      QtwentyFourteen ,
                      QtwentyEighteen )



                              as.factor(worldCup$year) #change the world cup years back into a factor
                              write.csv(irelandMatches, file = 'irelandMatches')
                              
                              irelandMatches <- subset(qualifiersAll, qualifiersAll$home_team == 'Republic of Ireland' | qualifiersAll$away_team == 'Republic of Ireland') 
# #Here is the code for generating a complete list of teams who 
# have played in any  qualifier 

numberQualifiermatches <- sapply(eachQualifier, nrows)

participantsByYear <- sapply(eachQualifier, testFunc) 
f <- print(testFunc(QthirtyEight)) #I don't know why Q38 is giving so much HOSS, Alas

#This lists all the unique countries who tried to qualify
# This puts them all onto CSV files for 
# I couldn't get them all onto one file cos there are a list and write.csv doesn't like lists
# note that the difficulties Cem had in listing teams and goals means that the following 21 lines of code 
#will be a lot less hassle than the nonsense he had to do in Excel

write.csv(participantsByYear[[2]], file = 'Q1934.csv', row.names = F)
write.csv(f, file = 'Q1938.csv', row.names = F)
write.csv(participantsByYear[[3]], file = 'Q1938.csv', row.names = F)
write.csv(participantsByYear[[4]], file = 'Q1950.csv', row.names = F)
write.csv(participantsByYear[[5]], file = 'Q1954.csv', row.names = F)
write.csv(participantsByYear[[6]], file = 'Q1958.csv', row.names = F)
write.csv(participantsByYear[[7]], file = 'Q1962.csv', row.names = F)
write.csv(participantsByYear[[8]], file = 'Q1966.csv', row.names = F)
write.csv(participantsByYear[[9]], file = 'Q1970.csv', row.names = F)
write.csv(participantsByYear[[10]], file = 'Q1974.csv', row.names = F)

write.csv(participantsByYear[[11]], file = 'Q1978.csv', row.names = F)
write.csv(participantsByYear[[12]], file = 'Q1982.csv', row.names = F)
write.csv(participantsByYear[[13]], file = 'Q1986.csv', row.names = F)
write.csv(participantsByYear[[14]], file = 'Q1990.csv', row.names = F)
write.csv(participantsByYear[[15]], file = 'Q1994.csv', row.names = F)
write.csv(participantsByYear[[16]], file = 'Q1998.csv', row.names = F)
write.csv(participantsByYear[[17]], file = 'Q2002.csv', row.names = F)
write.csv(participantsByYear[[18]], file = 'Q2006.csv', row.names = F)
write.csv(participantsByYear[[19]], file = 'Q2010.csv', row.names = F)
write.csv(participantsByYear[[20]], file = 'Q2014.csv', row.names = F)
write.csv(participantsByYear[[21]], file = 'Q2018.csv', row.names = F)

write.csv(participantsByYear[[3]], file = 'Q1938.csv', row.names = F)
class(participantsByYear)


                            install.packages('xlsx')
                            library('xlsx')
                            write.csv(participantsByYear, file = 'participantsByYear.xlsx')
                            
                            write.xlsx(mydata, "c:/mydata.xlsx")

write.csv (teams, file = 'world cup participants.csv', row.names=FALSE)
#notice that this list also provides the order in which teams qualified. 


#######
#######
#######
#######
#######
#######
#######
#######
#######
#######
#######
#######
#######
#######
#let's write a function for this to apply to all of the tournaments
# so that we can have a list of who played in what tournament


games <- print( nrows(QthirtyEight))
testFunc <- function(dfrm1){
  x <- as.vector(unique(dfrm1$home_team))
  y <- as.vector(unique(dfrm1$away_team))
  z <- c(x,y)
  z <- unique(z)
}

participantsByYear <- print(lapply(eachWorldCup, testFunc))
participantsByYear <- print(sapply(eachWorldCup, testFunc))
class(participantsByYear)

write.csv(participantsByYear, file = 'all teams in every world cup.csv') #This doesn't work at all
#see how many rows there are

participantsByYear <- print(lapply(eachWorldCup, nrows)) #this prints it all but in an ugly list
participantsByYear <- print(sapply(eachWorldCup, nrows)) #this is a vector. 

x <- testFunc(QthirtyFour)
QthirtyFour <- subset(qualifiersAll,qualifiersAll$year == 1932 | 1933 |1934)
nrow(QthirtyFour)
#######
#######
#######
#######
#######

#This functions tells us how many rows are in a dfrm 
# it will be useful for enumerating the 
nrows <- function(x){
  y <- nrow(x)
}

#######
#######
#######
#######
#######
#######
#######


# lets work out the number of goals scored in each game in the world cup

nrow(thirty)
head(thirty)
sum(thirty$home_score, thirty$away_score)/nrow(thirty)

avGoals <- function(dfrm2){
  x <- sum(dfrm2$home_score, dfrm2$away_score)/nrow(dfrm2)
}

avGoalsWC <- lapply(eachWorldCup, avGoals)

wcGoals <- as.numeric(avGoalsWC)
barplot(wcGoals)
class(wcGoals)
class(worldCup$year)
as.factor(worldCup$year)
is.factor(worldCup$year)

tournamentYears <- c(1930, 1934, 1938, 1950, 1954, 1958, 1962, 
                     1966, 1970, 1974, 1978, 1982, 1986, 1990, 
                     1994, 1998, 2002, 2006, 2010, 2014, 2018)

averageGoalsPerTournament <- data.frame(tournamentYears, avGoalsWC)

#this function renders all the info we want but produces a problematic list which is hard to read

write.csv(averageGoalsPerTournament, file = 'average goals per tournament.csv')
levels(worldCup$year)
lapply(eachWorldCup, write, "participantsByYear.txt", append=TRUE, ncolumns=1000)

lapply(participantsByYear, write, "participantsByYear.txt", append=TRUE, ncolumns=1000)



#######
#######
#######
#######
#######
#######
#######

# Time to write the code for the bubble plots

# https://www.r-graph-gallery.com/portfolio/ggplot2-package/
# for help galore
######
#######
#######
#######
#######
#######
#######



# library
library(tidyverse)


# A basic scatterplot = relationship between 2 values:
ggplot(avGoals, aes(x=avGoals$more.than.3, y=avGoals$goals, size = avGoals$goals)) +
  geom_point()


install.packages('tidyverse')

setwd('/Users/cexsligo/Documents/Data Analytics Nci')
#importing data for emmental chart
emmental1962 <- read.csv(file = '/Users/cexsligo/Documents/Data Analytics Nci/Emmental 1962-2018.csv', col.names = F)
emmental1930 <- read.csv(file = '/Users/cexsligo/Documents/Data Analytics Nci/EMMENTAL GOALS 1930-1958.csv')
shapiro.test(emmental1962$X69) # test for nomality 1962-2018

boxplot(emmental1962$X69)

barplot(avGoals$more.than.3, horiz = F)
plot(emmental1962)

# This is the Emmantal plot 1962 with labels for the years

ggplot(emmental1962, aes(x=emmental1962$X1, y=emmental1962$X2, size = emmental1962$X3)) +
  geom_point(alpha = 1) + 
  scale_size_continuous(range = c(1, 28)) + 
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  xlab ("Goals scored by the winning team") +
  ylab ("Goals scored by the losing team") +
  labs (size = "Number of games 
        with each scoreline") + 
  theme_classic() + 
  annotate("segment", x = 0, xend = 4, y = 0, yend = 4, 
           colour = "#FEC310", size=3, alpha=0.6, arrow=arrow()) + 
  annotate("text", x = c(1.5,8.5, 8.5, 8.5), y = c(3.2,0.7, 4.2, 3.6), 
           label = c("Draws", "Wins", 'Scorelines', '1962-2018') , 
           color=c("#FEC310", "Black", 'Black', 'Black'), size=12, fontface="bold") + 
  theme(axis.ticks = element_line(size = 2, color="#FEC310") , 
        axis.ticks.length = unit(.4, "cm")) + 
  expand_limits(x = c(0,10), y = c(0, 5))

# This is the Emmantal plot 1930 with labels for the years

ggplot(emmental1930, aes(x=emmental1930$X1, y=emmental1930$X2, size = emmental1930$X3)) +
  geom_point(alpha = 1) + 
  scale_size_continuous(range = c(1, 28)) + scale_x_continuous(breaks = seq(0, 10, 1)) +
  xlab ("Goals scored by the winning team") +
  ylab ("Goals scored by the losing team") +
  labs (size = "Number of games 
        with each scoreline") +
  theme_classic() + 
  annotate("segment", x = 0, xend = 4, y = 0, yend = 4, 
           colour = "#FEC310", size=3, alpha=0.6, arrow=arrow()) + 
  annotate("text", x = c(1.5,8.5, 8.5, 8.5), y = c(3.2,0.7, 4.2, 3.6), 
           label = c("Draws", "Wins", 'Scorelines', '1930-1958') , 
           color=c("#FEC310", "Black", 'Black', 'Black'), size=12, fontface="bold") + theme(
             axis.ticks = element_line(size = 2, color="#FEC310") , 
             axis.ticks.length = unit(.4, "cm")
           ) + expand_limits(x = c(0,10), y = c(0, 5))

#
#######
#######
#######
#######
#######
#######
#######

# And now for the bar chart with average lines annotated on it 
######
#######
#######
#######
#######
#######
#######



library(ggplot2)
theme_set(theme_classic())

# Plot
g <- ggplot(avGoals, aes(avGoals$tournament, avGoals$goals), horizontal = TRUE)
g + geom_bar(stat="identity", width = , fill="#1077c3") + 
  labs(title="    Average goals 3.08
       
       Average Goals 1930-1958: 4.27
       Mean goals post-1962: 2.61") + 
  xlab ("Tournament") +
  ylab ("Goals scored") +  scale_x_continuous(breaks = seq(1930, 2018, 1) +
                                                annotate("segment", x = 1960, xend = 2020, y = 2.606655, yend = 2.606655, 
                                                         colour = "#FEC310", size=0.75, alpha=1) +
                                                annotate("segment", x = 1928, xend = 2020, y = 3.08, yend = 3.08, 
                                                         colour = "#FEC310", size=2, alpha=1)
                                              
                                              
                                        
      
            
                                              
