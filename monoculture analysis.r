data0 <- read.csv('NoIdea.csv')[,-1]
str(data0)

#Adding in belowground biomass information to the dataset
mass.b <- read.csv('conman-data-2013-11-08-blwgrnd-biomass.csv',strip.white=T)
names(mass.b)[] <- tolower(names(mass.b)) #changes names to lower case
mass.b <- subset(mass.b,experiment=='E244')[,-c(2,3,5,6)] #subsetting by E244 and by removing columns 2, 3, 5, 6
names(mass.b)[names(mass.b) == "mass.g.m.2."] <- "mass.below" #among the columns in mass.b, rename the header mass.g.m.2 to mass.b
str(mass.b)


#This dataset has all datapoints with both above and belowground biomass, collected by year. NAs correspond to missing values of either above or below.
#Only use this dataset for analysis of monoculture plots
data_ab <- merge(data0,mass.b,by=c('year','plot'),all=T) #all=T means if there is information where one number is missing, then put a number in one and NA in another

data1 <- subset(data_ab,(SpNum==1 & (treatment=='Control' | treatment=='SoilDrenchFungicide')))

data2 <- merge(data1,data1,by=c('year','species')) #merging so that "soil Drench" and "Control" become columns
data2 <- subset(data2,(treatment.x=='Control' & treatment.y=='SoilDrenchFungicide')) #right now we have different line for each treatment, so we are subsetting out control and soil fungicide treatments. 
names(data2)[names(data2) == "plot.x"] <- "plot.c" 
names(data2)[names(data2) == "plot.y"] <- "plot.f"
names(data2)[names(data2) == "mass.above.x"] <- "mass.c"
names(data2)[names(data2) == "mass.above.y"] <- "mass.f"

names(data2)[names(data2) == "mass.below.x"] <- "bmass.c"
names(data2)[names(data2) == "mass.below.y"] <- "bmass.f"

names(data2)[names(data2) == "plot.bigbio.x"] <- "plot.bigbio"
data2 <- data2[,c('year','species','plot.bigbio','plot.c','mass.c','bmass.c','plot.f','mass.f','bmass.f')]

#Adding in C:N data
bigbio.cn <- read.csv('e120_Plant aboveground biomass carbon and nitrogen.csv')
str(bigbio.cn)
#interested only in columns 2,3,32,33, and only in year 2006 since it is the most recent to the start of ER
bigbio.cn <- bigbio.cn[bigbio.cn$Year==2006,c(2,3,32,33)]
names(bigbio.cn)[3:4] <- c('perN','perC')
data2 <- merge(data2,bigbio.cn[,-1],by.x=c('plot.bigbio'),by.y=c('Plot'),all.x=T)

#Calculating Response to Fungi
data2$frr <- with(data2,(mass.c-mass.f)/mass.c)
data2$species <- factor(data2$species)
data2$lrr <- with(data2,log(mass.c/mass.f))
which(data2$frr < (-50)) #Identifies the row for the outlier that is less than -50
data2$b.frr <- with(data2,(bmass.c-bmass.f)/bmass.c)
data2$b.lrr <- with(data2,log(bmass.c/bmass.f))
data2$CNratio <-with(data2, (perC/perN))

spnames <- read.csv('e120 Species Names.csv')
data3 <- merge(data2,spnames[,-2],by.x='species',by.y='spcode') #in data set x (data2) the column is called "Species" in y (spnames) the column is called "spcode"

names(data3)
write.csv(data3,file="EnemyRemovalMonoculture_SoilFungi_Biomass+C+N.csv")

library(ggplot2)
#view by species
qplot(x=year,y=lrr,color=species,data=data3[-36,],geom='line')
qplot(x=species,y=lrr,data=data3[-36,],shape=as.factor(year))

qplot(x=year,y=b.lrr,color=species,data=data3,geom='line')
qplot(x=species,y=b.lrr, data=data3,shape=as.factor(year))

#view by functional group
qplot(x=year,y=lrr,facets=~fun.group,color=species,data=data3[-36,],geom='line')
qplot(x=species,y=lrr,data=data3[-36,],color=fun.group,size=2,
      shape=as.factor(year))

qplot(x=year,y=b.lrr,facets=~fun.group,color=species,data=data3,geom='line')
qplot(x=species,y=b.lrr,data=data3,color=fun.group,size=2,
      shape=as.factor(year))

qplot(x=CNratio,y=b.lrr,data=data3,color=fun.group,size=2, facets=.~year)
qplot(x=CNratio,y=b.lrr,data=data3,color=fun.group,size=2, facets=year~.)
