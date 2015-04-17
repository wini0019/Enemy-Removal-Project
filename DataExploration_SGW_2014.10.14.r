
### set a new working directory
getwd()
setwd("C:/Users/Winnie/Documents/1_Grad School/UMN/Courses/Fall 2014/Foundations/Enemy Removal Project")
#Read in e001 data
d <- read.csv("E244Data_PlanFilev1 (1).csv", header=T, strip.white=T)
summary(d)
d

e<-subset(d, d$Insecticide==0 & d$FoliarFungicide==0,)
e<-e[,-c(5,7)] #for dataset e show me all the rows and all columns except 5 and 7
e

f<-subset(d, d$Insecticide==0 & d$FoliarFungicide==0 & d$PlantSpNum==1,)
f<-f[,-c(5,7)]
f

monoplot<-f$E244Plot #Vector with E244 plot number IDs for monoculture plots extracted from dataset f
monoplot

abvbio<-read.csv("conman-data-2013-11-08-abvgrnd-biomass.csv")
#g<-subset(abvbio, abvbio$Plot==f$E244Plot,) #Does not workT

#Two ways to subset the target based on your identification of plots with only 1 species present
#Single For-Loop
soilplots<-logical(0) #creates a vector to be filled by the "append" function later. 
for(i in abvbio$Plot){soilplots<-append(soilplots,any(i==monoplot))} #i is a value in the dataset "abvbio" and column "Plot". Act on the vector "soilplots" by appending to the end of "soilplots" if i is equal to any value in monoplots 
abvbio1<-abvbio[soilplots,]
View(abvbio1)

#Two For-Loops and an If-statement
rownum<-numeric() #vector to get row numbers of plots of interest
## this loop adds the row number for rows which have plots of interest 
##for each row i from 1 to the end of the datasheet "abvbio" and j from row 1 to the end of the datasheet "f" ...
##If i==j then add the entire row i into the vector "rownum" 
for(i in 1:length(abvbio$Plot)){
  for(j in 1:length(e$E244Plot)){
  if(abvbio$Plot[i]==e$E244Plot[j]) 
    rownum<-c(rownum, i)
}}
summary(rownum)

abvbio2<-abvbio[c(rownum),] #subsetting rows of interest from "abvbio" where the rows all the values in "rownum", and all columns are included  
abvbio2

?merge
names(abvbio2)
names(e)
names(abvbio2)[4]<- "E244Plot"
abvbio3<-merge(abvbio2, e, by="E244Plot")
abvbio3

abvbio3<-abvbio3[order(abvbio3$Year, abvbio3$E244Plot),] #Since order is a function you use parentheses to tell it which columns to act upon in the dataset
abvbio3


##################################################################################


#
table(d$Year, is.na(d$Moisture)) #Shows the data that exists for Moisture. In 1982 there 162 data points for which there is no data (True) and zero points for which there is data (False)
is.na(c(NA,2)) #Shows the NA value as True
!is.na(c(NA,2))#Shows the NA value as False and the existing value as True

#Is species diversity or biomass a better predictor of soil N? --> Multiple Linear Regression: SoilN~Diversity*Biomass
#How does N addition and biomass (or diversity) predict Soil Moisture?
#How does the relationship between biomass and soil moisture change with the level of Nitrogen addition to the plot?
  #Is this different depending on the years of N addition?

#ID what the variables are using the structure command
str(d) #Note that "int" means integer value, "factor" is categorical, "logi" is logical values, "num" is numerical

#Determine the earliest year for which we have BOTH biomass and moisture data
table(d$Year, is.na(d$Moisture))

#New data frame: Keep every row where moisture and total biomass is not missing. Comma means to keep all the columns in the new data frame
#d[1:10,] gives rows 1-10 for all columns
#d[1:10,1:2] gives rows 1-10 for columns 1-2
tmp<-d[!is.na(d$Moisture) & !is.na(d$TotBio) & (d$NAdd=="27.2"|d$NAdd=="9.52"|d$NAdd=="3.4"|d$NAdd=="0"),]
summary(tmp)

#Visualize how many values for which Moisture and TotBio data exist per year
years<-table(tmp$Year, !is.na(tmp$Moisture) & !is.na(tmp$TotBio))
years

#Subsetting: All the rows for 1987 OR 2004, and all the columns for those rows
tmp.1987<-tmp[tmp$Year==1987,]
summary(tmp.1987)

tmp.2004<-tmp[tmp$Year==2004,]
summary(tmp.2004)

#Install "ggplot" which is a quick way to do plots
install.packages('ggplot2')
install.packages('Hmisc') # has nice helper functions
library(ggplot2)
library(Hmisc)

#Linear Model and Normality Test Graphs for Linear Regression
lm.1987<-lm(Moisture~log(TotBio)*NAdd, data=tmp.1987)
lm.1987
summary(lm.1987)
plot(lm.1987)

lm.2004<-lm(Moisture~log(TotBio)*NAdd, data=tmp.2004)
lm.2004
summary(lm.2004)
plot(lm.2004)
#Assumption that variabnce is constant. Residuals = distance between each point and the fitted line. Plot of Residuals against fitted values should be flat line.
#Assumption that residuals are normal. QQ = straight line
#Residuals vs. Leverage: shows data point (row) that has a big effect on results

#Regression Plot#
#quick plot of the X value, the Y value, using data=____, use colors for the factor ___) + insert lines on the plot (use our linear model for the lines, Do not include SE envelopes)
qplot(log(TotBio), Moisture, data=tmp.1987, col=as.factor(NAdd))+geom_smooth(method="lm", se=FALSE)

qplot(log(TotBio), Moisture, data=tmp.2004, col=as.factor(NAdd))+geom_smooth(method="lm", se=FALSE)

#Interaction Interpretation: For TotBio as you increase NAdd soil Moisture gets progressively less.


