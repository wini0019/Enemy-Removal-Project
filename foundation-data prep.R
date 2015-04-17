#EEB Foundations Project data analysis - Winnie, Mayank and Siddharth
#This code combines the plans of E244 and E120, and then uses that to subset aboveground biomass data from Conman, removing data of species that were not planted into the plots
# This version is 14 Oct 2014

#clear the environment
rm(list = ls())

##### READ IN PLAN FOR 244 AND BIGBIO, MAKE COMBINED PLAN #####
#Isolate the information necessary to merge or subset dataset for data analysis
#Links ER plot to Big Bio plot and ER Treatment and Plant Species planted

plan.244<-read.csv("E244Data_PlanFilev1.csv",strip.white=T,na.strings=".")
names(plan.244)[names(plan.244) == "ER244.Plot"] <- "plot"
names(plan.244)[names(plan.244) == "BigBioPlot"] <- "plot.bigbio"
names(plan.244)[names(plan.244) == "PlantSpNum"] <- "rich.trt"
names(plan.244)[names(plan.244) == "TreatmentName"] <- "treatment"
plan.244$experiment <- "E244"
plan.244 <- plan.244[c("plot", "plot.bigbio", "rich.trt", "treatment")]
plan.244 <- subset(plan.244,rich.trt!=32) #Remove plots with 32 species because we don't have corresponding BigBio planting information
head(plan.244)
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
plan <- merge(plan.244,bigb.plan,by='plot.bigbio') #This is the final merged plan dataset

head(plan)

#####   PROCESSING BIOMASS DATA TO ONLY RETAIN PLANTED SPECIES   ######
#Reading in species names and shortforms
spnames <- read.csv('e120 Species Names.csv')
#Removing Petca, Petvi, Solri, Bargr - they are not on the BigBio plan
spnames <- spnames[-c(6,14,16,21),]
names(plan)[names(plan) == "Amocan"] <- "Amoca" #Renaming a discrepancy between bigb plan and species names sheet


#Merge Biomass, Percent Cover, Leaf C, Leaf N, and Leaf P into a single dataset#

####### READ IN Aboveground BIOMASS DATA ####### Mostly from Eric's code #####
data.a<-read.csv("conman-data-2013-11-08-abvgrnd-biomass.csv", strip.white=T)
names(data.a)[] <- tolower(names(data.a))
data.a <- data.a[data.a$experiment == "E244",]
data.a$mass.g. <- 10*data.a$mass.g. #Mass is not converted in orginal file
data.a$mass.above <- data.a$mass.g.m.2.
data.a$species <- toupper(data.a$species)
data.a <- data.a[grep("LITTER", data.a$species, invert=T),]
data.a <- data.a[grep("FUNGI", data.a$species, invert=T),]
data.a <- data.a[c("year", "plot", "mass.above", "species")]
sort(unique(data.a$species))


#Now go through the species named in the aboveground biomass data and change only planted species names to short forms
for(i in c(1:length(spnames$species))) {
  data.a$species[grep(spnames$species[i],data.a$species,ignore.case=T)] <- as.character(spnames$spcode[i])
}

## THIS CODE SELECTS ONLY RELEVANT SPECIES FOR EACH PLOT - brief method below
## This is dependent upon the existing structure of the plan matrix

#Go through data.a row by row - Get ER plot number from each row
#use plot no. to extract species present (binary) vector, take only those names from spnames
#match between the species whose biomass is measured and the ones planted, store row number if true
rownames(data.a) <- NULL
shorts <- as.character(spnames$spcode)
rowkeep <- numeric(0) ; sps <- character(0) ; sp.pres <- logical(0) #initialising containers, just to make sure
#Main loop, this does all the work
for (i in c(1:length(data.a$species))) {
  sp.pres <- as.logical(plan[plan$plot==data.a$plot[i],7:24]) #vector of species planted into this plot
  if (any(is.na(sp.pres))) {next} #This line accounts for the plots with 32 species
  sps <- shorts[sp.pres] #names of species planted in this plot
  if (any(data.a$species[i]==sps)) {rowkeep <- append(rowkeep,i)} #count this plot only if biomass species matches with any of above
}
data.b <- data.a[rowkeep,] #subset to create final data
rownames(data.b) <- NULL
rm(rowkeep,shorts,sp.pres,sps,i) #cleaning up - comment out if debugging
#Add in richness and treatment information from the plan
data.b <- merge(data.b,plan[,c(1:4,6)],by='plot')
head(data.b)



write.csv(data.b,file='NoIdea.csv')

###################################################################################
##########          OLD CODE         ##############################################

#To see how much data we have from monoculture plots, and how many species.
plan.sp1 <- subset(plan.244,(rich.trt==1 & (treatment=="SoilDrenchFungicide"|treatment=='Control')))
sp1e <- plan.sp1$plot
sp1b <- plan.sp1$plot.bigbio
abv.07 <- subset(data.a,Year==2007)

#to select only our plots of interest
soilplots <- logical(0)
for (i in abv.07$Plot) {soilplots <- append(soilplots,any(i==sp1))}
abv.07_us <- abv.07[soilplots,]
View(abv.07_us)

data.a[1:10,]
data.a$sps_short[data.a$species=='AGROPYRON REPENS'] <- 'AGRRE'

######## Winnie Exploration: Species Names Across Dataset ########
#Load the data you want to look at
above <- read.csv("~/1_Grad School/UMN/Courses/Fall 2014/Foundations/Enemy Removal Project/conman-data-2013-11-08-abvgrnd-biomass.csv", strip.white=TRUE)  #"strip.white=TRUE" tells R to remove spaces before and after the values in each cell
View(above)
cover <- read.csv("~/1_Grad School/UMN/Courses/Fall 2014/Foundations/Enemy Removal Project/conman-data-2013-09-05-cover.csv", strip.white=TRUE)
View(cover)
#write the data to a csv file. now the data will not have spaces before or after entries
write.csv(cover, "conman-data-2013-09-05-cover_SGWedit-2014-11-05.csv")
write.csv(above, "conman-data-2013-11-08-abvgrnd-biomass_SGWedit-2014-11-05.csv")

#Re-load data that has been corrected for mis-spellings
above <- read.csv("~/1_Grad School/UMN/Courses/Fall 2014/Foundations/Enemy Removal Project/conman-data-2013-11-08-abvgrnd-biomass_SGWedit-2014-11-05.csv", strip.white=TRUE)  
View(above)
cover <- read.csv("~/1_Grad School/UMN/Courses/Fall 2014/Foundations/Enemy Removal Project/conman-data-2013-09-05-cover_SGWedit-2014-11-05.csv", strip.white=TRUE)
View(cover)
#Alphabetically Sort the unique species names in each dataset to ID any repeats#
unique(above$Species)
sort(unique(above$Species))
unique(cover$Species)
sort(unique(cover$Species))
