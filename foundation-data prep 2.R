#EEB Foundations Project data analysis - Winnie, Mayank and Siddharth
#This code combines the plans of E244 and E120, and then uses that to subset aboveground biomass data from Conman, removing data of species that were not planted into the plots
# This version is 14 Oct 2014

#Depends on the following files 



##### READ IN PLAN FOR 244 AND BIGBIO, MAKE COMBINED PLAN #####

plan.244<-read.csv("E244Data_PlanFilev1.csv",strip.white=T,na.strings=".")
names(plan.244)[names(plan.244) == "ER244.Plot"] <- "plot"
names(plan.244)[names(plan.244) == "BigBioPlot"] <- "plot.bigbio"
names(plan.244)[names(plan.244) == "PlantSpNum"] <- "rich.trt"
names(plan.244)[names(plan.244) == "TreatmentName"] <- "treatment"
plan.244$experiment <- "E244"
plan.244 <- plan.244[c("plot", "plot.bigbio", "rich.trt", "treatment")]
#plan.244 <- subset(plan.244,rich.trt!=32) #Remove plots with 32 species because we don't have corresponding BigBio planting information

#Read in BigBio dataset, to get which species was planted in each plot
bigb<-read.csv("e120_Plant aboveground biomass data.csv", strip.white=T,na.strings="")
names(bigb)[names(bigb) == "Plot"] <- "plot.bigbio"
plotnums <- sort(unique(plan.244$plot.bigbio)) #these are the plots that we actually want info of
bigb7 <- subset(bigb,Year==2007) #Choosing year 2007 because our experiment started then
rownums = numeric(0)
for (i in plotnums) { #Choosing the first big bio row that matches our plotnums of interest
  rownums <- append(rownums,which(bigb7$plot.bigbio == i)[1])
}
rownums <- rownums[!is.na(rownums)]
bigb.plan <- bigb7[rownums,-c(1:3,5:8,11:17,36:37)] #Removes columns we're not interested in for the plan file
rownames(bigb.plan) <- NULL ; rm(rownums,bigb7,plotnums,i) #Cleaning up
#I think we should be using SpNum instead of NumSp for the species richness of the plot, that is what seems to match for the 2 spp case - something to ask Eric about
#the 32 species plots are not represented in the bigbio biomass dataset. They are added with NAs for the species information
plan <- merge(plan.244,bigb.plan,by='plot.bigbio',all.x=T) #This is the final merged plan dataset
rm(plan.244,bigb.plan,bigb)

#Reading in species names and shortforms
spnames <- read.csv('e120 Species Names.csv')
#Removing Petca, Petvi, Solri, Bargr - they are not on the BigBio plan
spnames <- spnames[-c(6,14,16,21),]
names(plan)[names(plan) == "Amocan"] <- "Amoca" #Renaming a discrepancy between bigb plan and species names sheet



#####   MERGING ALL DATASETS TO CREATE MASTER DATASET  ######

#colnames - year,plot,plot.bigbio,rich.trt, treatment,SpNum,species,fun.grp,mass.above,mass.below
#cover,bigb.C,bigb.N,leaf.C,leaf.N,leaf.P

#######  BIOMASS DATA    #######
data.a<-read.csv("conman-data-2013-11-08-abvgrnd-biomass_SGWedit-2014-11-05.csv", strip.white=T)
names(data.a)[] <- tolower(names(data.a))
data.a <- data.a[data.a$experiment == "E244",] #ignoring the old field plots
data.a$mass.g. <- 10*data.a$mass.g. #Mass is not converted in orginal file
data.a$mass.above <- data.a$mass.g.m.2.
data.a$species <- toupper(data.a$species)
data.a <- data.a[grep("LITTER", data.a$species, invert=T),]
data.a <- data.a[grep("FUNGI", data.a$species, invert=T),]
data.a <- data.a[c("year", "plot", "mass.above", "species")]
#Now go through the species named in the aboveground biomass data and change only planted species names to short forms
for(i in c(1:length(spnames$species))) {
  data.a$species[grep(spnames$species[i],data.a$species,ignore.case=T)] <- as.character(spnames$spcode[i])
}
data.b <- data.a

#### BELOWGROUND BIOMASS  ###
data.a <- read.csv('conman-data-2013-11-08-blwgrnd-biomass.csv',strip.white=T)
names(data.a)[] <- tolower(names(data.a))                                       #change headers to lowercase
data.a <- subset(data.a,experiment=='E244')[,-c(2,3,5,6)]
names(data.a)[names(data.a) == "mass.g.m.2."] <- "mass.below"
str(data.a)
#This dataset has all datapoints with both above and belowground biomass, collected by year. NAs correspond to missing values of either above or below.
#Only use this dataset for analysis of monoculture plots
data.b <- merge(data.b,data.a,by=c('year','plot'),all=T);rm(data.a)

#######  COVER DATA    #######
data.a<-read.csv("conman-data-2013-09-05-cover_SGWedit-2014-11-05.csv", strip.white=T)
names(data.a)[] <- tolower(names(data.a))
data.a <- data.a[data.a$exp == "E244",]
data.a$species <- toupper(data.a$species)
names(data.a)[names(data.a) == "percent.cover"] <- "cover"
data.a <- data.a[grep("FUNGI", data.a$species, invert=T),]
data.a$species[grep("BARE", data.a$species)] <- 'BARE GROUND'
data.a <- data.a[c("year", "plot", "cover", "species")]
#Now go through the species named and change only planted species names to short forms
for(i in c(1:length(spnames$species))) {
  data.a$species[grep(spnames$species[i],data.a$species,ignore.case=T)] <- as.character(spnames$spcode[i])
}
data.b <- merge(data.b,data.a,all=T) ; rm(data.a)
sort(unique(data.b$species))

#### LEAF C:N DATA
er.cn <- read.csv('ER2011-CN-7.19.12-etb.csv')
names(er.cn) <- tolower(names(er.cn))
er.cn$species <- as.character(er.cn$species)
er.cn$species[er.cn$species=='LESC'] <- 'Lesca'
er.cn$species[er.cn$species=='SCHS'] <- 'Schsc'
er.cn$species[er.cn$species=='LUPINE'] <- 'Luppe'
er.cn$species[er.cn$species=='ANDGE'] <- 'Andge'
er.cn <- subset(er.cn,species!='SCHS-APH')[,c(3,4,7,8)]
er.cn$species <- as.factor(er.cn$species)
str(er.cn)
names(er.cn)[c(1,3,4)] <- c('plot','leaf.C','leaf.N')
er.cn$year <- 2011
data.b <- merge(data.b,er.cn,all=T); rm(er.cn)

#### LEAF P DATA
er.p <- read.csv('ER-P-analysis-9.27.2012.csv')
names(er.p) <- tolower(names(er.p))
er.p <- er.p[,c(2,3,4,16)]
er.p <- subset(er.p,(species!='apple' & species != 'nist apple' & er244.plot != 'chk75'))
er.p$species <- factor(er.p$species)
er.p$species <- toupper(er.p$species)
er.p <- with(er.p,aggregate(pct.p,by=list(er244.plot,species),mean))
names(er.p) <- c('plot','species','leaf.P')
er.p$species <- as.character(er.p$species)
er.p$species[er.p$species=='LESC'] <- 'Lesca'
er.p$species[er.p$species=='SCHS'] <- 'Schsc'
er.p$species[er.p$species=='LUPINE'] <- 'Luppe'
er.p$species[er.p$species=='ANDGE'] <- 'Andge'
er.p$species <- as.factor(er.p$species)
str(er.p)
er.p$year <- 2011
data.b <- merge(data.b,er.p,all=T) ; rm(er.p)

data.b <- merge(plan[,c(1:4,6)],data.b,by='plot'); 

#### BIGBIO C:N DATA
#this is at the level of the bigbio plot
bigbio.cn <- read.csv('e120_Plant aboveground biomass carbon and nitrogen.csv')
#interested only in columns 2,3,32,33, and only in year 2006 since it is the most recent to the start of ER
bigbio.cn <- bigbio.cn[bigbio.cn$Year==2006,c(2,3,32,33)]
names(bigbio.cn)[3:4] <- c('bigb.N','bigb.C')
str(bigbio.cn)
data.b <- merge(data.b,bigbio.cn[,-1],by.x=c('plot.bigbio'),by.y=c('Plot'),all.x=T)
str(data.b); rm(bigbio.cn)




## THIS CODE CHECKS IF SPECIES IS MEANT TO BE IN PLOT OR NOT
#Go through data.b row by row - Get ER plot number from each row
#use plot no to extract species present (binary) vector, take only those names from spnames
#match between the species whose biomass is measured and the ones planted, store row number if true
rownames(data.b) <- NULL
shorts <- as.character(spnames$spcode) #sp. name shortforms
sps <- character(0) ; sp.pres <- logical(0) #initialising containers, just to make sure
data.b$intent.sp <- F #This column is logical - was the species intended to be there or not
#Main loop, this decides if species is intended to be there or not
for (i in c(1:length(data.b$species))) {
  sp.pres <- as.logical(plan[plan$plot==data.b$plot[i],7:24]) #vector of species planted into this plot
  if (any(is.na(sp.pres))) {data.b$intent.sp[i] <- NA;next} #This line accounts for the plots with 32 species
  sps <- shorts[sp.pres] #names of species planted in this plot
  if (any(data.b$species[i]==sps)) {data.b$intent.sp[i] <- T} #count this plot only if biomass species matches with any of above
}
rm(shorts,sp.pres,sps,i) #cleaning up - comment out if debugging

write.csv(data.b,file='ER_foundations_master.csv')

#Check that the master dataset has no repeat names#
#master <- read.csv("~/1_Grad School/UMN/Courses/Fall 2014/Foundations/Enemy Removal Project/ER_foundations_master.csv", strip.white=TRUE)  
#View(master)
#unique(master$species)
#sort(unique(master$species))

