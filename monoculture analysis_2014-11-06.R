rm(list = ls())

master <- read.csv('ER_foundations_master_2014-11-11.csv', na.strings= c(".", "NA"))[,-1]
str(master)   #shows the structure of the dataset

#This dataset has all datapoints with both above and belowground biomass, collected by year. NAs correspond to missing values of either above or below.

#Subset out all the 32 species plots
sixteen<-master[master$rich.trt!=32,]   #!= means not equal to
head(sixteen)
View(sixteen)
#Subset data to only have intended species data
intent<-sixteen[sixteen$intent.sp==T,]   #all rows with intent.sp=T can stay
head(intent)
#Remove rows that are NA's
intent<-intent[is.na(intent$year)==F,] #Is wherever there are not NA's the data can stay
View(intent)
#subset out soil drench fungicide treatment
soildrench<-intent[intent$treatment=="SoilDrenchFungicide"| intent$treatment=="Control",]

#Create a dataframe in which to analyze your data
#get rid of columns that we no longer need 
soildrench<-soildrench[,-c(14,15,16)]   #keep all rows for columns NOT including 4 and 16
head(soildrench)
str(soildrench)

#Line up control and treatment next to one another for each response variable
#merge  by year, species, and rich.trt. DO NOT include plot because no control and treatment plot will be the same. 
#all=T: if there is information where one number is missing, then put a number in one and NA in the missing number
soildrench2<-merge(soildrench, soildrench, by=c("year", "species", "rich.trt"), all=T)   #merging so that "soil Drench" and "Control" can become columns
soildrench2<-subset(soildrench2,(treatment.x=="Control" & treatment.y=="SoilDrenchFungicide")) #right now we have different line for each treatment, so we are subsetting out control and soil fungicide treatments. 
table(soildrench2$leaf.N.x) #check to see that you have data in the response variable you are interested in

#Winnie: looking at N:P ratios for Control vs. Trt across diversity gradient
#Remove all rows without leaf N and P measurements (NA or ".")
#  soildrench2<-soildrench2[soildrench2$leaf.N.x!=,]
soildrench3<-soildrench2[is.na(soildrench2$leaf.N.x)==F & is.na(soildrench2$leaf.N.y)==F & is.na(soildrench2$leaf.P.x)==F & is.na(soildrench2$leaf.P.y)==F,]
head(soildrench3)

#calculate new variable from existing variables.
 #First, you must make sure that the factors you are using are identified as numeric
 #leaf.P.x<-as.numeric(soildrench3$leaf.P.x)
is.numeric(leaf.P.x)
#Next, convert percent leaf N and P to moles N and P
 #Example: a sample is 4% N and 1% P. In 10 grams of sample there will be 4g N and 1g P. 
          #There will be 4g N/(14g N/mol)= 4/14 mol N
soildrench3$mol.N.x<-with(soildrench3, (leaf.N.x/14.00))
soildrench3$mol.P.x<-with(soildrench3, (leaf.P.x/30.97))
soildrench3$mol.N.y<-with(soildrench3, (leaf.N.y/14.00))
soildrench3$mol.P.y<-with(soildrench3, (leaf.P.y/30.97))
#Next, calculate the N:P ratio for control and treatment leaves
soildrench3$NP.ctl<-with(soildrench3,(mol.N.x/mol.P.x))
soildrench3$NP.trt<-with(soildrench3,(mol.N.y/mol.P.y))
#Determine the treatment effect (c/T). A number > 1 means mutualism, a numer < 1 means pathogenic relation to fungi
soildrench3$NP.res<-with(soildrench3, (NP.ctl/NP.trt))
#Plot the data with N:P ratio as the response and plot diversity as the predictor for all 4 species
library(ggplot2)
qplot(x=rich.trt,y=NP.res,data=soildrench3,color=species,size=2,
      shape=species)
#Determine the treatment effect for Percent N and P in leaves (c/T)
#Plot the percent N and percent P with plot diversity as the predictor for all 4 species
soildrench3$leaf.N.res<-with(soildrench3, (leaf.N.x/leaf.N.y))
soildrench3$leaf.P.res<-with(soildrench3, (leaf.P.x/leaf.P.y))

qplot(x=rich.trt,y=leaf.N.res,data=soildrench3,color=species,size=2,
      shape=species, position="jitter")
qplot(x=rich.trt,y=leaf.P.res,data=soildrench3,color=species,size=2,
      shape=species, position="jitter")

###################################################################################################
#Try merging the data in a different way.
#Continue from line 23 ...
#Begin by subsetting so that you just have rows where leaf N and P are present
soildrench2<-soildrench[is.na(soildrench$leaf.N)==F & is.na(soildrench$leaf.P)==F,]
head(soildrench2)
#Then merge by "year", "species", and "rich.trt" or richness treatment
soildrench3<-merge(soildrench2, soildrench2, by=c("year","plot.bigbio", "rich.trt", "species"), all=T)   
soildrench3<-subset(soildrench3,(treatment.x=="Control" & treatment.y=="SoilDrenchFungicide"))  
#Calculate treatment effect (c/T). x>1==net fungal mutualism. x<1==net fungal pathogen
soildrench3$leaf.N.res<-with(soildrench3, (leaf.N.x/leaf.N.y))
soildrench3$leaf.P.res<-with(soildrench3, (leaf.P.x/leaf.P.y))

library(ggplot2)
qplot(x=rich.trt,y=leaf.N.res,data=soildrench3,color=species,size=2,
      shape=species, position="jitter")
qplot(x=rich.trt,y=leaf.P.res,data=soildrench3,color=species,size=2,
      shape=species, position="jitter")

qplot(x=rich.trt,y=log(leaf.P.res),data=soildrench3, facets=~species,
      main="Soil Fungicide Treatment", 
      xlab="Planted Species Richness",
      ylab="Log(%P Control / %P Treatment)")
qplot(x=rich.trt,y=log(leaf.N.res),data=soildrench3, facets=~species,
      main="Soil Fungicide Treatment", 
      xlab="Planted Species Richness", 
      ylab="Log(%N Control / %N Treatment)")

# Change the dataset from long form to short form
library(reshape2)
# use the "melt" function
melt.soildrench3 <- melt(data=soildrench3, id.vars=c("year", "plot.bigbio", "rich.trt", "species"),
     measure.vars=c("leaf.N.res", "leaf.P.res"))
# plot the dataset with differnt colors for N and P response.
qplot(x=rich.trt,y=log(value),data=melt.soildrench3, facets=~species,
      color=variable,
      main="Soil Fungicide Treatment", 
      xlab="Planted Species Richness", 
      ylab="Log(Control /Treatment)")

###################################################################################################
#Try looking at percent carbon in the same way
#Begin at line 23 ...
soildrench4<-soildrench[is.na(soildrench$leaf.C)==F,]
head(soildrench4)
#Then merge by "year", "species", and "rich.trt" or richness treatment
soildrench5<-merge(soildrench4, soildrench4, by=c("year","plot.bigbio", "rich.trt", "species"), all=T)   
soildrench5<-subset(soildrench5,(treatment.x=="Control" & treatment.y=="SoilDrenchFungicide"))  
#Calculate treatment effect (c/T). x>1==net fungal mutualism. x<1==net fungal pathogen
soildrench5$leaf.C.res<-with(soildrench5, (leaf.C.x/leaf.C.y))

qplot(x=rich.trt,y=leaf.C.res,data=soildrench5,color=species,size=2,
      shape=species, position="jitter")

qplot(x=rich.trt,y=log(leaf.C.res),data=soildrench5, facets=~species,
      main="Soil Fungicide Treatment", 
      xlab="Planted Species Richness", 
      ylab="Log(%C Control / %C Treatment)")

###################################################################################################
#Try looking at percent cover in the same way
#Begin at line 23 ...
soildrench6<-soildrench[is.na(soildrench$cover)==F,]
head(soildrench6)
soildrench7<-merge(soildrench6, soildrench6, by=c("year","plot.bigbio", "rich.trt", "species"), all=T)   
soildrench7<-subset(soildrench7,(treatment.x=="Control" & treatment.y=="SoilDrenchFungicide"))  

soildrench7$cover.res<-with(soildrench7, (cover.x/cover.y))
qplot(x=rich.trt,y=cover.res,data=soildrench7,color=species,size=2,
      shape=species, position="jitter")

#add functional groupings and plot again
spname<-read.csv("e120 Species Names.csv")
names(spname)[names(spname) == "species"] <- "gen.spp" 
names(spname)[names(spname) == "spcode"] <- "species" 
soildrench8<-merge(soildrench7, spname, by=c("species"), all=T)
#remove NA's for the response variable
soildrench8<-soildrench8[is.na(soildrench8$year)==F,]

qplot(x=rich.trt,y=log(cover.res),data=soildrench8,color=fun.group,size=2, position="jitter")
qplot(x=rich.trt,y=log(cover.res),data=soildrench8,color=species, facets=~fun.group, position="jitter")
qplot(x=rich.trt,y=log(cover.res),data=soildrench8,color=fun.group, facets=~species)

###################################################################################################
#####################Look at the same variables in Foliar Fungicide Trt#####################

# Use the sixteen dataset that contain only plots with 1 to 16 species
View(sixteen)
# Use the intent dataset that only contains species of interest to us
View(intent)
# Subset out foliar fungicide treatment
ffungi<-intent[intent$treatment=="FoliarFungicide"| intent$treatment=="Control",]
#Create a dataframe in which to analyze your data
#get rid of columns that we no longer need 
ffungi<-ffungi[,-c(14,15,16)]   #keep all rows for columns NOT including 4 and 16
head(ffungi)
str(ffungi)

##### Looking at Leaf N, Leaf P, and Leaf C #####
#Begin by subsetting so that you just have rows where leaf N and P are present
ffungi2<-ffungi[is.na(ffungi$leaf.N)==F & is.na(ffungi$leaf.P)==F & is.na(ffungi$leaf.C)==F,]
head(ffungi2)
#Then merge by "year", "species", and "rich.trt" or richness treatment
ffungi3<-merge(ffungi2, ffungi2, by=c("year","plot.bigbio", "rich.trt", "species"), all=T)   
ffungi3<-subset(ffungi3,(treatment.x=="Control" & treatment.y=="FoliarFungicide"))  
#Calculate treatment effect (c/T). x>1==net fungal mutualism. x<1==net fungal pathogen
ffungi3$leaf.N.res<-with(ffungi3, (leaf.N.x/leaf.N.y))
ffungi3$leaf.P.res<-with(ffungi3, (leaf.P.x/leaf.P.y))
ffungi3$leaf.C.res<-with(ffungi3, (leaf.C.x/leaf.C.y))

library(ggplot2)

qplot(x=rich.trt,y=log(leaf.P.res),data=ffungi3, facets=~species, 
      main="Foliar Fungicide Treatment", 
      xlab="Planted Species Richness", 
      ylab="Log(%P Control / %P Treatment)")
qplot(x=rich.trt,y=log(leaf.N.res),data=ffungi3, facets=~species,
      main="Foliar Fungicide Treatment", 
      xlab="Planted Species Richness", 
      ylab="Log(%N Control / %N Treatment)")
qplot(x=rich.trt,y=log(leaf.C.res),data=ffungi3, facets=~species,
      main="Foliar Fungicide Treatment", 
      xlab="Planted Species Richness", 
      ylab="Log(%C Control / %C Treatment)")

# plot the dataset with differnt colors for N and P response.
melt.ffungi3 <- melt(data=ffungi3, id.vars=c("year", "plot.bigbio", "rich.trt", "species"),
                         measure.vars=c("leaf.N.res", "leaf.P.res", "leaf.C.res"))
qplot(x=rich.trt,y=log(value),data=melt.ffungi3, facets=~species,
      color=variable,
      main="Foliar Fungicide Treatment", 
      xlab="Planted Species Richness", 
      ylab="Log(Control /Treatment)")

###################################################################################################
##############Same variables in Insecticide Treatment##############

# Use the sixteen dataset that contain only plots with 1 to 16 species
View(sixteen)
# Use the intent dataset that only contains species of interest to us
View(intent)
# Subset out foliar fungicide treatment
insect<-intent[intent$treatment=="Insecticide"| intent$treatment=="Control",]
#Create a dataframe in which to analyze your data
#get rid of columns that we no longer need 
insect<-insect[,-c(14,15,16)]   #keep all rows for columns NOT including 4 and 16
head(insect)
str(insect)

##### Looking at Leaf N, Leaf P, and Leaf C #####
#Begin by subsetting so that you just have rows where leaf N and P are present
insect2<-insect[is.na(insect$leaf.N)==F & is.na(insect$leaf.P)==F & is.na(insect$leaf.C)==F,]
head(insect2)
#Then merge by "year", "species", and "rich.trt" or richness treatment
insect3<-merge(insect2, insect2, by=c("year","plot.bigbio", "rich.trt", "species"), all=T)   
insect3<-subset(insect3,(treatment.x=="Control" & treatment.y=="Insecticide"))  
#Calculate treatment effect (c/T). x>1==net fungal mutualism. x<1==net fungal pathogen
insect3$leaf.N.res<-with(insect3, (leaf.N.x/leaf.N.y))
insect3$leaf.P.res<-with(insect3, (leaf.P.x/leaf.P.y))
insect3$leaf.C.res<-with(insect3, (leaf.C.x/leaf.C.y))

# plot the dataset with differnt colors for N and P response.
melt.insect3 <- melt(data=insect3, id.vars=c("year", "plot.bigbio", "rich.trt", "species"),
                     measure.vars=c("leaf.N.res", "leaf.P.res", "leaf.C.res"))
qplot(x=rich.trt,y=log(value),data=melt.insect3, facets=~species,
      color=variable,
      main="Insecticide Treatment", 
      xlab="Planted Species Richness", 
      ylab="Log(Control /Treatment)")

###################################################################################################
###################################################################################################
#################### Means ####################
library(plyr)
# Average for each plot across years


# Average for each diversity level across plots


