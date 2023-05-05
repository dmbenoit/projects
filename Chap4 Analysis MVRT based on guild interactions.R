#Chapter 4 Analysis
#Steps:
#1.) Place all species in BsM dataset into thermal guilds
#2.) Filter for lakes that have all three guilds
#3.) Create interaction scores for guilds
#4.) Multivariate regression tree with envr variables on interaction scores

##########################################################################################################################
#Step 1: Load BsM data and place all species into guilds
setwd("C:/Users/David/OneDrive/Desktop/Chapter 4")
library(tidyr)
library(dplyr)

#load BsM data for all lakes
BSM <- read.csv("BsMCycle1_IndividualFishData.csv", header=TRUE, sep=",", stringsAsFactors = TRUE)

BSM[,"Guild"] <- NA

#generate species list
spec_list <- BSM %>%
  group_by(SpecName) %>%
  summarise()

#add thermal guild to each species 
#Salmon/trout/whitefish family = cold
BSM$Guild[which(BSM$SpecCode >= 70 & BSM$SpecCode <= 107)] <- "Cold"
#alewife = cold
BSM$Guild[which(BSM$SpecCode == 61)] <- "Cold"
#Banded killifish = cool
BSM$Guild[which(BSM$SpecCode == 261)] <- "Cool"
#catfishes = warm
BSM$Guild[which(BSM$SpecCode >= 231 & BSM$SpecCode <= 244)] <- "Warm"
#white and black crappie = cool
BSM$Guild[which(BSM$SpecCode >= 318 & BSM$SpecCode <= 319)] <- "Cool"
#blackchin shiner = warm
BSM$Guild[which(BSM$SpecCode == 199)] <- "Warm"
#blcknose dace = cool
BSM$Guild[which(BSM$SpecCode == 210)] <- "Cool"
#blacknose shiner = warm
BSM$Guild[which(BSM$SpecCode == 200)] <- "Warm"
#green sunfish to largemouth bass = warm
BSM$Guild[which(BSM$SpecCode >= 312 & BSM$SpecCode <= 317)] <- "Warm"
#bluntnose minnow = warm
BSM$Guild[which(BSM$SpecCode == 208)] <- "Warm"
#bowfin = warm
BSM$Guild[which(BSM$SpecCode == 51)] <- "Warm"
#sticklebacks = cold
BSM$Guild[which(BSM$SpecCode >= 281 & BSM$SpecCode <= 284)] <- "Cold"
#bridle shiner = cool
BSM$Guild[which(BSM$SpecCode == 197)] <- "Cool"
#brook silverside = warm
BSM$Guild[which(BSM$SpecCode == 361)] <- "Warm"
#burbot = cool
BSM$Guild[which(BSM$SpecCode == 271)] <- "Cool"
#central mudminnow = warm
BSM$Guild[which(BSM$SpecCode == 141)] <- "Warm"
#common carp = warm
BSM$Guild[which(BSM$SpecCode == 186)] <- "Warm"
#common shiner = cool
BSM$Guild[which(BSM$SpecCode == 198)] <- "Cool"
#creek chub = cool
BSM$Guild[which(BSM$SpecCode == 212)] <- "Cool"
#sculpins = cold
BSM$Guild[which(BSM$SpecCode >= 381 & BSM$SpecCode <= 384)] <- "Cold"
#emerald shiner = cool
BSM$Guild[which(BSM$SpecCode == 196)] <- "Cool"
#fallfish = cool
BSM$Guild[which(BSM$SpecCode == 213)] <- "Cool"
#fathead minnow = warm
BSM$Guild[which(BSM$SpecCode == 209)] <- "Warm"
#finescale dace = cool
BSM$Guild[which(BSM$SpecCode == 183)] <- "Cool"
#freshwater drum = warm
BSM$Guild[which(BSM$SpecCode == 371)] <- "Warm"
#gizzard shad = cool
BSM$Guild[which(BSM$SpecCode == 63)] <- "Cool"
#golden redhorse = warm
BSM$Guild[which(BSM$SpecCode == 170)] <- "Warm"
#golden shiner = cool
BSM$Guild[which(BSM$SpecCode == 194)] <- "Cool"
#goldeye/mooneye = warm
BSM$Guild[which(BSM$SpecCode >= 151 & BSM$SpecCode <= 152)] <- "Warm"
#grass pickerel = warm
BSM$Guild[which(BSM$SpecCode == 133)] <- "Warm"
#greater redhorse = warm
BSM$Guild[which(BSM$SpecCode == 172)] <- "Warm"
#hornyhead chub = cool
BSM$Guild[which(BSM$SpecCode == 192)] <- "Cool"
#iowa darter = cool
BSM$Guild[which(BSM$SpecCode == 338)] <- "Cool"
#johnny darter = cool
BSM$Guild[which(BSM$SpecCode == 341)] <- "Cool"
#lake chub = cold
BSM$Guild[which(BSM$SpecCode == 185)] <- "Cold"
#lake sturgeon = cool
BSM$Guild[which(BSM$SpecCode == 31)] <- "Cool"
#logperch = warm
BSM$Guild[which(BSM$SpecCode == 342)] <- "Warm"
#longnose dace = cool
BSM$Guild[which(BSM$SpecCode == 211)] <- "Cool"
#gar = warm
BSM$Guild[which(BSM$SpecCode >= 41 & BSM$SpecCode <= 44)] <- "Warm"
#longnose sucker = cold
BSM$Guild[which(BSM$SpecCode == 162)] <- "Cold"
#mimic shiner = warm
BSM$Guild[which(BSM$SpecCode == 206)] <- "Warm"
#muskellunge = warm
BSM$Guild[which(BSM$SpecCode == 132)] <- "Warm"
#northern pike = cool
BSM$Guild[which(BSM$SpecCode == 131)] <- "Cool"
#northern redbelly dace = warm
BSM$Guild[which(BSM$SpecCode == 182)] <- "Warm"
#pearl dace = cool
BSM$Guild[which(BSM$SpecCode == 214)] <- "Cool"
#quillback = cool
BSM$Guild[which(BSM$SpecCode == 161)] <- "Cool"
#rainbow smelt = cold
BSM$Guild[which(BSM$SpecCode == 121)] <- "Cold"
#river redhorse = cool
BSM$Guild[which(BSM$SpecCode == 173)] <- "Cool"
#rock bass = cool
BSM$Guild[which(BSM$SpecCode == 311)] <- "Cool"
#rosyface shiner = warm
BSM$Guild[which(BSM$SpecCode == 202)] <- "Warm"
#round goby = cool
BSM$Guild[which(BSM$SpecCode == 366)] <- "Cool"
#sand shiner = warm
BSM$Guild[which(BSM$SpecCode == 204)] <- "Warm"
#sauger = cool
BSM$Guild[which(BSM$SpecCode == 332)] <- "Cool"
#shorthead redhorse = warm
BSM$Guild[which(BSM$SpecCode == 171)] <- "Warm"
#silver lamprey = warm
BSM$Guild[which(BSM$SpecCode == 13)] <- "Warm"
#silver redhorse = cool
BSM$Guild[which(BSM$SpecCode == 168)] <- "Cool"
#spotfin shiner = warm
BSM$Guild[which(BSM$SpecCode == 203)] <- "Warm"
#spottail shiner = cool
BSM$Guild[which(BSM$SpecCode == 201)] <- "Cool"
#striped shiner = cool
BSM$Guild[which(BSM$SpecCode == 217)] <- "Cool"
#trout-perch = cold
BSM$Guild[which(BSM$SpecCode == 291)] <- "Cold"
#walleye = cool
BSM$Guild[which(BSM$SpecCode == 334)] <- "Cool"
#white bass = warm
BSM$Guild[which(BSM$SpecCode == 302)] <- "Warm"
#white sucker = cool
BSM$Guild[which(BSM$SpecCode == 163)] <- "Cool"
#yellow perch = cool 
BSM$Guild[which(BSM$SpecCode == 331)] <- "Cool"


#remove species where guild is NA
#these are unknown species, hybrids, and large groups 
BSM <- BSM %>%
  filter(Guild != "NA")

##############################################################################################################
#Step 2: filter for lakes that have all three guilds
#filter for lakes with more than one Guild
multispecies <- BSM %>%
  group_by(WbyLID, Guild) %>%
  summarise(number =n())

multispecies <- spread(multispecies, Guild, number)

#remove any rows that have NA values (zero fish of that guild)
multispecies <- na.omit(multispecies)

#remove lakes
BSM <- BSM %>%
  filter(WbyLID %in% multispecies$WbyLID)

#filter for lakes with more than one depth strata
strata <- BSM %>%
  group_by(WbyLID, DepthStratum) %>%
  summarise(number=n())

strata <- spread(strata, DepthStratum, number)

#remove lakes that only have one strata
strata <- as.data.frame(strata)
strata <- strata %>%
  filter(strata$`3` != "NA")

lakes <- strata[1]

BSM <- BSM %>% 
  filter(WbyLID %in% lakes$WbyLID)

###############################################################################################################
#Create interaction scores for each guild pair across lakes
#create interaction matrices
Freq <- BSM %>%
  group_by(WbyLID, DepthStratum, Guild) %>%
  summarize(Frequency=n())

#create a dataframe for each lake by name
list_df <- split(Freq, Freq$WbyLID)

#remove empty dataframes
list_df <- list_df[sapply(list_df, function(x) dim(x)[1]) > 0]

list2env(list_df, envir = .GlobalEnv)

library(reshape)
#for loop to create co-occurence matrix for each dataset
for (i in 1:length(list_df)){
  list_df[[i]] <- cast(list_df[[i]], DepthStratum ~ Guild, value = "Frequency", FUN = mean)
  list_df[[i]] <- as.data.frame(list_df[[i]])
  list_df[[i]][is.na(list_df[[i]])] <- 0
  list_df[[i]] <- as.matrix(list_df[[i]])
}

#remove frist column from all matrices (effortsamplenum column)
list_df <- lapply(list_df, "[", TRUE, -c(1))

#create interaction matrix for each lake
library(spaa)

interactions <- c()

for (i in 1:length(list_df)){
  interactions[[i]] <- as.matrix(niche.overlap(list_df[[i]], method =c("morisita")))
  
}

library(gtools)
test <- c()
m <- c()
y <- c()
w <- c()
for (i in 1:length(interactions)){
  m[[i]] <- interactions[i][[1]]
  m[[i]] <- as.dist(m[[i]])
  y[[i]] <- dist2list(m[[i]])
  y[[i]][4] <- paste(y[[i]]$col, y[[i]]$row, sep = " x ")
  y[[i]][1] <- y[[i]][4]
  y[[i]][4] <- NULL
  y[[i]][2] <- NULL
  w[[i]] <- t(y[[i]])
  w[[i]] <- as.data.frame(w[[i]])
}


for (i in 1:length(w)){
  colnames(w[[i]]) <- unlist(w[[i]][row.names(w[[i]])=="col",])
  w[[i]] <- w[[i]][-c(1),]
}

new_interaction_df <- smartbind(list = w)
no_duplicates <- t(new_interaction_df)
no_duplicates <- unique(no_duplicates)
new_interaction_df <- t(no_duplicates)
new_interaction_df <- as.data.frame(new_interaction_df)

new_interaction_df <- new_interaction_df[-c(1)]

interactions <- cbind(lakes, new_interaction_df)

interactions$WbyLID <- paste("WbyLID",interactions$WbyLID, sep="")

#####################################################################################################################
#Step 4: Multivariate regression tree with envr variables
#setwd("C:/Users/Jackson Lab/Desktop/PhD/Chapter 4")
library(stringr)

#load environmental variables
envr_data <- read.csv("May2021_BsM_Cycle1_Lakedata.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)

envr_data$Date.of.Primary.Netting <- as.Date(envr_data$Date.of.Primary.Netting, "%d-%b-%y")
#convert dates of sampling to julian day
envr_data$Date.of.Primary.Netting <- format(envr_data$Date.of.Primary.Netting, "%j")


#filter for envr columns we want
envr_data <- envr_data[c(3,4,5,6,7,8,9,11,12,13,25,30,31,32)]


#load summer secchi (as it is in a different file)
summer_secchi <- read.csv("May2021_BsM_Cycle1_SummerSecchi.csv", header=TRUE, sep=",", stringsAsFactors = TRUE)

#remove unnecessary columns
summer_secchi <- summer_secchi[c(4,13)]
colnames(summer_secchi) <- c("Wby_LID", "Secchi_Summer")


#filter from summer_secchi to make them even for merging
summer_secchi2 <- summer_secchi %>%
  filter(Wby_LID %in% envr_data$Wby_LID)

#merge envr_data2 and summer_secchi
envr <- merge(envr_data, summer_secchi2)

#make sure waterbody LIDs match
envr$Wby_LID <- paste("WbyLID", envr$Wby_LID, sep="")
#envr$Wby_LID <- gsub("-", ".", envr$Wby_LID)
envr$Wby_LID <- as.factor(envr$Wby_LID)

#add epibenthic area (first add Deptherm)
envr<- transform(envr, DepTherm = 3.26 * (Area^0.109)*(Depmn^0.213)*exp(1)^(-0.0263*MAT_8110))
envr <- transform(envr, pArea_epi = 1-(1-(DepTherm/Depmax))^(Depmax/(Depmn-1)))

#remove empty lake 
envr <- envr[-c(1),]

envr$Date.of.Primary.Netting <- as.numeric(envr$Date.of.Primary.Netting)

#create newenvr file for maps and then remove lat and long
envr3 <- envr
envr <- envr[-c(2:3)]

#test to see if there is a difference with lakes sampled in July and August
#envr <- envr %>%
 # filter(Date.of.Primary.Netting > 181 & Date.of.Primary.Netting < 260)

#determine if any of the environmental variables are strongly correlated
library(corrplot)
abiotic <- envr[-c(1)]
abiotic <- na.omit(abiotic)
M <- cor(abiotic)
corrplot(M, method="number", type="upper")

#Remove strong correlations (>0.8)
#Strong correlation between Depmax and Depmn - remove depmn
#Strong correlation between DD5 and MAT - remove MAT
#Strong correlation between pArea_epi and Depmn (Depmn already removed)
#Strong correlation between TDS and conductivity - remove conductivity 
#Strong correlation between Thermo_Pred and Deptherm - remove ThermoPred

envr <- envr[-c(5,8,9,12)]

envr$pArea_epi[is.nan(envr$pArea_epi)] <- NA

colnames(interactions) <- c("Wby_LID", "Cool x Cold", "Warm x Cold", "Warm x Cool")

#filter envr data for lakes that have all three guilds and more than onde depth stratum
envr2 <- envr %>%
  filter(Wby_LID %in% interactions$Wby_LID)

#data for mvpart
mvpart_data <- merge(interactions,envr2)

#make characters numeric
mvpart_data[c(2:4)] <- sapply(mvpart_data[c(2:4)],as.numeric)

library(mvpart)
mrt <- mvpart(data.matrix(mvpart_data[,2:4]) ~ Date.of.Primary.Netting + Area + Depmax + TDS + DD5_8110 + TP + pH + Secchi_Summer + DepTherm + pArea_epi, data = mvpart_data, xv="p", all.leaves=T, xval=nrow(mvpart_data), xvmult=100)


####################################################################################################################
#create a map to see where groups lie
#use full envr dataset from above
#envr3
#filter envr data for lakes that have all three guilds and more than onde depth stratum
envr4 <- envr3 %>%
  filter(Wby_LID %in% interactions$Wby_LID)

shallow <- envr4 %>%
  filter(Depmax >= 8.75 & Depmax < 17.05)

v_shallow <- envr4 %>%
  filter(Depmax < 8.75)

deep_warm <- envr4 %>%
  filter(Depmax >= 17.05 & DD5_8110 >= 1606)

deep_cold_clear <- envr4 %>%
  filter(Depmax >= 17.05 & DD5_8110 < 1606 & Secchi_Summer >= 1.76)

deep_cold_unclear <- envr4 %>%
  filter(Depmax >= 17.05 & DD5_8110 < 1606 & Secchi_Summer < 1.76)

#maps
library(maps)
library(mapdata)
library(raster)
library(tidyr)
library(maptools)
library(dplyr)



canada <- getData("GADM",country="CAN",level=1)

ontario <- c("Ontario")
ontario.ca <- canada[canada$NAME_1 %in% ontario,]

plot(ontario.ca)

map.scale(-94, 42.5,
          ratio=FALSE,
          relwidth=0.12,
          cex=0.8)



#add points of each cluster

points(shallow$Long, shallow$Lat, pch = 19, col = "black", cex=0.75)

points(v_shallow$Long, v_shallow$Lat, pch = 19, col = "black", cex=0.75)

points(deep_warm$Long, deep_warm$Lat, pch = 19, col = "black", cex=0.75)

points(deep_cold_clear$Long, deep_cold_clear$Lat, pch = 19, col = "black", cex=0.75)

points(deep_cold_unclear$Long, deep_cold_unclear$Lat, pch = 19, col = "black", cex=0.75)


library(GISTools)
north.arrow(xb=-93.5, yb=44, len=0.05, lab="N")

###############################################################################################################
#for boxplots

shallow <- mvpart_data %>%
  filter(Depmax >= 8.75 & Depmax < 17.05)

shallow_long <- gather(shallow, guild_pair, interaction_score, "Cool x Cold":"Warm x Cool", factor_key = TRUE)

p <- ggplot(shallow_long, aes(x = guild_pair, y = interaction_score, fill=guild_pair)) + geom_boxplot(notch = FALSE)
p + scale_fill_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) + theme_classic() + xlab("Leaf 4 (n = 81)") + ylab(NULL) +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), legend.position = "none", text = element_text(size=35))


boxplot(shallow_long$interaction_score~ shallow_long$guild_pair, col=c("blue", "yellow", "red"))

v_shallow <- mvpart_data %>%
  filter(Depmax < 8.75)

v_shallow_long <- gather(v_shallow, guild_pair, interaction_score, "Cool x Cold":"Warm x Cool", factor_key = TRUE)
p2 <- ggplot(v_shallow_long, aes(x = guild_pair, y = interaction_score, fill=guild_pair)) + geom_boxplot(notch = FALSE)
p2 + scale_fill_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) + theme_classic() + xlab("Leaf 5 (n = 18)") + ylab(NULL) +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), legend.position = "none", text = element_text(size=35))+ scale_y_continuous(limits =c(0,1),breaks=seq(from=0, to=1, by=0.25))



boxplot(v_shallow_long$interaction_score~ v_shallow_long$guild_pair, col=c("blue", "yellow", "red"))

deep_warm <- mvpart_data %>%
  filter(Depmax >= 17.05 & DD5_8110 >= 1606)

deep_warm_long <- gather(deep_warm, guild_pair, interaction_score, "Cool x Cold":"Warm x Cool", factor_key = TRUE)

boxplot(deep_warm_long$interaction_score~ deep_warm_long$guild_pair, col=c("blue", "yellow", "red"))

p3 <- ggplot(deep_warm_long, aes(x = guild_pair, y = interaction_score, fill=guild_pair)) + geom_boxplot(notch = FALSE)
p3 + scale_fill_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) + theme_classic() + xlab("Leaf 1 (n = 221)") + ylab("Index of spatial overlap") + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), legend.position = "none", text = element_text(size=35))


deep_cold_clear <- mvpart_data %>%
  filter(Depmax >= 17.05 & DD5_8110 < 1606 & Secchi_Summer >= 1.76)

deep_cold_clear_long <- gather(deep_cold_clear, guild_pair, interaction_score, "Cool x Cold":"Warm x Cool", factor_key = TRUE)

boxplot(deep_cold_clear_long$interaction_score~ deep_cold_clear_long$guild_pair, col=c("blue", "yellow", "red"))
p4 <- ggplot(deep_cold_clear_long, aes(x = guild_pair, y = interaction_score, fill=guild_pair)) + geom_boxplot(notch = FALSE)
p4 + scale_fill_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) + theme_classic() + xlab("Leaf 2 (n = 103)") + ylab(NULL) +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), legend.position = "none", text = element_text(size=35))



deep_cold_unclear <- mvpart_data %>%
  filter(Depmax >= 17.05 & DD5_8110 < 1606 & Secchi_Summer < 1.76)

deep_cold_unclear_long <- gather(deep_cold_unclear, guild_pair, interaction_score, "Cool x Cold":"Warm x Cool", factor_key = TRUE)

boxplot(deep_cold_unclear_long$interaction_score~ deep_cold_unclear_long$guild_pair, col=c("blue", "yellow", "red"))

p4 <- ggplot(deep_cold_unclear_long, aes(x = guild_pair, y = interaction_score, fill=guild_pair)) + geom_boxplot(notch = FALSE)
p4 + scale_fill_manual(values = c("#7570B3", "#1B9E77", "#D95F02")) + theme_classic() + xlab("Leaf 3 (n = 8)") +  ylab(NULL) +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), legend.position = "none", text = element_text(size=35)) + scale_y_continuous(limits =c(0,1),breaks=seq(from=0, to=1, by=0.25))

#####################################################################################################################################
#Identify most abundant species from each guild in each leaf
BSM$WbyLID <- paste("WbyLID", BSM$WbyLID, sep="")
colnames(BSM)[3] <- "Wby_LID"

leaf1 <- BSM %>%
  filter(BSM$Wby_LID %in% deep_warm$Wby_LID)

counts1 <- leaf1 %>%
  group_by(Guild,SpecName) %>%
  summarise(n=n())


leaf2 <- BSM %>%
  filter(BSM$Wby_LID %in% deep_cold_clear$Wby_LID)

counts2 <- leaf2 %>%
  group_by(Guild,SpecCode) %>%
  summarise(n=n())


leaf3 <- BSM %>%
  filter(BSM$Wby_LID %in% deep_cold_unclear$Wby_LID)

counts3 <- leaf3 %>%
  group_by(Guild,SpecCode) %>%
  summarise(n=n())


leaf4 <- BSM %>%
  filter(BSM$Wby_LID %in% shallow$Wby_LID)

counts4 <- leaf4 %>%
  group_by(Guild,SpecCode) %>%
  summarise(n=n())

leaf5 <- BSM %>%
  filter(BSM$Wby_LID %in% v_shallow$Wby_LID)

counts5 <- leaf5 %>%
  group_by(Guild,SpecCode) %>%
  summarise(n=n())
