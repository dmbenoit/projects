#Code for updated Lake Nipissing Size Spectrum Model
#Including four fishing scenarios

library(mizer)

#Lake Nipissing Model
#Based on avegare FWIN values across decades
#interaciton matrix from BsM

setwd("C:/Users/Jackson Lab/Desktop/PhD/Nipissing Data/FWIN_UPDATE")
#Load mizer details
species_params2 <- read.csv("Nipissing_FWIN_paramslowdt2.csv", header=TRUE, sep =",")

setwd("C:/Users/Jackson Lab/Desktop/PhD/Nipissing Data")
#interaction matrix
inter <- read.csv("Nip_interaction2.csv", header = TRUE, sep = ",", row.names = NULL, stringsAsFactors = TRUE )
inter <- inter[-c(1)]
colnames(inter) <- c("Cisco", "Forage Fish", "Northern Pike", "Walleye", "Yellow Perch")
rownames(inter) <- colnames(inter)
inter <- as.matrix(inter)

effort <- c(0.154425,0,0.52554398, 0.3163, 0.154425)

params <- MizerParams(species_params = species_params2, interaction = inter, kappa = 150, w_pp_cutoff = 2)
#params@species_params$erepro <- 0.1

sim <- project(params, effort = effort, t_max=200, dt=0.1, t_save=1)
plot(sim)

##############################################################################################################################
#Scenario 1: Increase fishing effort of Walleye
setwd("C:/Users/Jackson Lab/Desktop/PhD/Nipissing Data/FWIN_UPDATE")

gear_params <- data.frame(species = species_params2$species,
                          gear = species_params2$species,
                          sel_func = "sigmoid_length",
                          l25 = species_params2$l25,
                          l50= species_params2$l50)

f_history <- read.csv("f_history_scenario1.csv", header= TRUE, sep =",", row.names=1)
colnames(f_history) <- c("Cisco", "Forage Fish", "Northern Pike", "Walleye", "Yellow Perch")
f_history <- as.matrix(f_history)

params <- MizerParams(species_params2,
                      interaction = inter,
                      kappa = 150,
                      gear_params = gear_params,
                      w_pp_cutoff = 2)

names(dimnames(f_history)) <- c("time", "gear")

sim <- project(params, effort=f_history, dt=0.1)
plot(sim)
plotBiomass(sim, start_time=2010, end_time = 2100)

#get mean slope for years preceding change (2011-2021)
slope <- as.data.frame(getCommunitySlope(sim)[91:101,,])
colMeans(slope)
colSds(as.matrix(slope))

#get mean slope for final years (2090-2100)
slope <- as.data.frame(getCommunitySlope(sim)[170:180,,])
colMeans(slope)
colSds(as.matrix(slope))

#get mean biomass preceding change
bio1 <- as.data.frame(getBiomass(sim))
bio1[,6] <- rowSums(bio1)
bio2 <- bio1[91:101,]
colMeans(bio2)

#mean biomass in final ten years
bio3 <- bio1[170:180,]
colMeans(bio3)



p1 <- plotBiomass(sim, start_time=2010, end_time = 2100) + theme(axis.title.x = element_blank()) + geom_vline(xintercept = 2022, linetype = "dashed") 


###############################################################################################################################
#Scenario 2: Decrease fishing effort of Walleye
gear_params <- data.frame(species = species_params2$species,
                          gear = species_params2$species,
                          sel_func = "sigmoid_length",
                          l25 = species_params2$l25,
                          l50= species_params2$l50)

f_history <- read.csv("f_history_scenario2.csv", header= TRUE, sep =",", row.names=1)
colnames(f_history) <- c("Cisco", "Forage Fish", "Northern Pike", "Walleye", "Yellow Perch")
f_history <- as.matrix(f_history)

params <- MizerParams(species_params2,
                      interaction = inter,
                      kappa = 150,
                      gear_params = gear_params,
                      w_pp_cutoff = 2)

params@species_params$erepro <- 0.1

names(dimnames(f_history)) <- c("time", "gear")

sim2 <- project(params, effort=f_history, dt=0.1, t_max=2100)
plot(sim2)
plotBiomass(sim2, start_time=2015, end_time = 2100)

#get mean slope for years preceding change (2011-2021)
slope <- as.data.frame(getCommunitySlope(sim2)[91:101,,])
colMeans(slope)
colSds(as.matrix(slope))

#get mean slope for final years (2090-2100)
slope <- as.data.frame(getCommunitySlope(sim2)[170:180,,])
colMeans(slope)
colSds(as.matrix(slope))

#get mean biomass preceding change
bio1 <- as.data.frame(getBiomass(sim2))
bio1[,6] <- rowSums(bio1)
bio2 <- bio1[91:101,]
colMeans(bio2)

#mean biomass in final ten years
bio3 <- bio1[170:180,]
colMeans(bio3)

p2 <- plotBiomass(sim2, start_time=2010, end_time = 2100) + theme(legend.position = "none") + geom_vline(xintercept = 2022, linetype = "dashed")

#############################################################################################################################
#Scenario 3: Increase fishing effort of Yellow Perch
gear_params <- data.frame(species = species_params2$species,
                          gear = species_params2$species,
                          sel_func = "sigmoid_length",
                          l25 = species_params2$l25,
                          l50= species_params2$l50)

f_history <- read.csv("f_history_scenario3.csv", header= TRUE, sep =",", row.names=1)
colnames(f_history) <- c("Cisco", "Forage Fish", "Northern Pike", "Walleye", "Yellow Perch")
f_history <- as.matrix(f_history)

params <- MizerParams(species_params2,
                      interaction = inter,
                      kappa = 150,
                      r_pp = 10,
                      gear_params = gear_params,
                      w_pp_cutoff = 2)

names(dimnames(f_history)) <- c("time", "gear")

sim3 <- project(params, effort=f_history, dt=0.1)
plot(sim3)
plotBiomass(sim3, start_time=2015, end_time = 2100)

#get mean slope for final years (2090-2100)
slope <- as.data.frame(getCommunitySlope(sim3)[170:180,,])
colMeans(slope)
colSds(as.matrix(slope))

#get mean biomass preceding change
bio1 <- as.data.frame(getBiomass(sim3))
bio1[,6] <- rowSums(bio1)
bio2 <- bio1[91:101,]
colMeans(bio2)

#mean biomass in final ten years
bio3 <- bio1[170:180,]
colMeans(bio3)

p3 <- plotBiomass(sim3, start_time=2010, end_time = 2100) + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank()) + geom_vline(xintercept = 2022, linetype = "dashed")


############################################################################################################################
#Scenario 4: Decrease fishing effort for Yellow Perch
gear_params <- data.frame(species = species_params2$species,
                          gear = species_params2$species,
                          sel_func = "sigmoid_length",
                          l25 = species_params2$l25,
                          l50= species_params2$l50)

f_history <- read.csv("f_history_scenario4.csv", header= TRUE, sep =",", row.names=1)
colnames(f_history) <- c("Cisco", "Forage Fish", "Northern Pike", "Walleye", "Yellow Perch")
f_history <- as.matrix(f_history)

params <- MizerParams(species_params2,
                      interaction = inter,
                      kappa = 150,
                      gear_params = gear_params,
                      w_pp_cutoff = 2)

names(dimnames(f_history)) <- c("time", "gear")

sim4 <- project(params, effort=f_history, dt=0.1)
plot(sim4)
plotBiomass(sim4, start_time=2015, end_time = 2100)

#get mean slope for final years (2090-2100)
slope <- as.data.frame(getCommunitySlope(sim4)[170:180,,])
colMeans(slope)
colSds(as.matrix(slope))

#get mean biomass preceding change
bio1 <- as.data.frame(getBiomass(sim4))
bio1[,6] <- rowSums(bio1)
bio2 <- bio1[91:101,]
colMeans(bio2)

#mean biomass in final ten years
bio3 <- bio1[170:180,]
colMeans(bio3)

p4 <- plotBiomass(sim4, start_time=2010, end_time = 2100) + theme(legend.position = "none", xlab=NULL, axis.title.y = element_blank())+ geom_vline(xintercept = 2022, linetype = "dashed")


#############################################################################################################################
#Scenario 5: Large increase for Walleye followed by decrease to assess recovery
gear_params <- data.frame(species = species_params2$species,
                          gear = species_params2$species,
                          sel_func = "sigmoid_length",
                          l25 = species_params2$l25,
                          l50= species_params2$l50)

f_history <- read.csv("f_history_scenario5.csv", header= TRUE, sep =",", row.names=1)
colnames(f_history) <- c("Cisco", "Forage Fish", "Northern Pike", "Walleye", "Yellow Perch")
f_history <- as.matrix(f_history)

params <- MizerParams(species_params2,
                      interaction = inter,
                      kappa = 150,
                      gear_params = gear_params,
                      w_pp_cutoff = 2)

names(dimnames(f_history)) <- c("time", "gear")

sim5 <- project(params, effort=f_history, dt=0.1)
plot(sim5)
plotBiomass(sim5, start_time=2015, end_time = 2100)

#metrics for time to recovery
biomass5 <- as.data.frame(getBiomass(sim5))
biomass5$Community <- rowSums(biomass5)
#in case year needs to be a row
Year <- rownames(biomass5)
#rownames(biomass5) <- NULL
#biomass5 <- cbind(Year, biomass5)

#baseline average (2017-2021) prior to change in effort
baseline5 <- biomass5[97:101,]
baseline5 <- colMeans(baseline5)
baseline5 <-as.vector(baseline5)

#calculate five year rolling average biomass for each species and community as a whole
roll_mean <- as.data.frame(rollmean(biomass5, k=5, align="right", fill=NA))
#divide by baseline to track changes
changes <- sweep(roll_mean, 2, baseline5, "/")
changes <- cbind(Year, changes)

#look from years after 2021
changes2 <- changes[101:180,]

data_long <- gather(changes2, Species, Relative_biomass, "Cisco":"Community", factor_key = TRUE)
data_long$Year <- as.numeric(data_long$Year)

p <- ggplot(data=data_long, aes(x=Year, y=Relative_biomass, group=Species)) + geom_line(aes(color=Species)) 

p + geom_hline(yintercept = 1.05, linetype = "dashed") + geom_hline(yintercept = 0.95, linetype = "dashed")


#################################################################################################################################
#Scenario 6: Large decrease for Walleye followed by increase to assess recovery
gear_params <- data.frame(species = species_params2$species,
                          gear = species_params2$species,
                          sel_func = "sigmoid_length",
                          l25 = species_params2$l25,
                          l50= species_params2$l50)

f_history <- read.csv("f_history_scenario6.csv", header= TRUE, sep =",", row.names=1)
colnames(f_history) <- c("Cisco", "Forage Fish", "Northern Pike", "Walleye", "Yellow Perch")
f_history <- as.matrix(f_history)

params <- MizerParams(species_params2,
                      interaction = inter,
                      kappa = 150,
                      gear_params = gear_params,
                      w_pp_cutoff = 2)

names(dimnames(f_history)) <- c("time", "gear")

sim6 <- project(params, effort=f_history, dt=0.1)
plot(sim6)
plotBiomass(sim6, start_time=2015, end_time = 2150)

#metrics for time to recovery
biomass6 <- as.data.frame(getBiomass(sim6))
biomass6$Community <- rowSums(biomass6)
#in case year needs to be a row
Year <- rownames(biomass6)
#rownames(biomass5) <- NULL
#biomass5 <- cbind(Year, biomass5)

#baseline average (2017-2021) prior to change in effort
baseline6 <- biomass6[97:101,]
baseline6 <- colMeans(baseline6)
baseline6 <-as.vector(baseline6)

#calculate five year rolling average biomass for each species and community as a whole
roll_mean <- as.data.frame(rollmean(biomass6, k=5, align="right", fill=NA))
#divide by baseline to track changes
changes <- sweep(roll_mean, 2, baseline6, "/")
changes <- cbind(Year, changes)

#look from years after 2021
changes2 <- changes[101:200,]

data_long <- gather(changes2, Species, Relative_biomass, "Cisco":"Community", factor_key = TRUE)
data_long$Year <- as.numeric(data_long$Year)

p <- ggplot(data=data_long, aes(x=Year, y=Relative_biomass, group=Species)) + geom_line(aes(color=Species)) 

p + geom_hline(yintercept = 1.05, linetype = "dashed") + geom_hline(yintercept = 0.95, linetype = "dashed")

##############################################################################################################################
#Scenario 7: Realistic increase for Walleye followed by decrease to assess recovery
gear_params <- data.frame(species = species_params2$species,
                          gear = species_params2$species,
                          sel_func = "sigmoid_length",
                          l25 = species_params2$l25,
                          l50= species_params2$l50)

f_history <- read.csv("f_history_scenario7.csv", header= TRUE, sep =",", row.names=1)
colnames(f_history) <- c("Cisco", "Forage Fish", "Northern Pike", "Walleye", "Yellow Perch")
f_history <- as.matrix(f_history)

params <- MizerParams(species_params2,
                      interaction = inter,
                      kappa = 150,
                      gear_params = gear_params,
                      w_pp_cutoff = 2)

names(dimnames(f_history)) <- c("time", "gear")

sim7 <- project(params, effort=f_history, dt=0.1)
plot(sim7)
plotBiomass(sim7, start_time=2015, end_time = 2100)

f1 <- plotBiomass(sim7, start_time=2010, end_time = 2100) 


#metrics for time to recovery
biomass7 <- as.data.frame(getBiomass(sim7))
biomass7$Community <- rowSums(biomass7)
#in case year needs to be a row
Year <- rownames(biomass7)
#rownames(biomass5) <- NULL
#biomass5 <- cbind(Year, biomass5)

#baseline average (2017-2021) prior to change in effort
baseline7 <- biomass7[97:101,]
baseline7 <- colMeans(baseline7)
baseline7 <-as.vector(baseline7)

#calculate five year rolling average biomass for each species and community as a whole
roll_mean <- as.data.frame(rollmean(biomass7, k=5, align="right", fill=NA))
#divide by baseline to track changes
changes <- sweep(roll_mean, 2, baseline7, "/")
changes <- cbind(Year, changes)

#look from years after 2021
changes2 <- changes[101:200,]

data_long <- gather(changes2, Species, Relative_biomass, "Cisco":"Community", factor_key = TRUE)
data_long$Year <- as.numeric(data_long$Year)

p <- ggplot(data=data_long, aes(x=Year, y=Relative_biomass, group=Species)) + geom_line(aes(color=Species)) 

p + geom_hline(yintercept = 1.05, linetype = "dashed") + geom_hline(yintercept = 0.95, linetype = "dashed")

##################################################################################################################################
#Scenario 8: Realistic decrease for Walleye followed by increase to assess recovery
gear_params <- data.frame(species = species_params2$species,
                          gear = species_params2$species,
                          sel_func = "sigmoid_length",
                          l25 = species_params2$l25,
                          l50= species_params2$l50)

f_history <- read.csv("f_history_scenario8.csv", header= TRUE, sep =",", row.names=1)
colnames(f_history) <- c("Cisco", "Forage Fish", "Northern Pike", "Walleye", "Yellow Perch")
f_history <- as.matrix(f_history)

params <- MizerParams(species_params2,
                      interaction = inter,
                      kappa = 150,
                      gear_params = gear_params,
                      w_pp_cutoff = 2)

names(dimnames(f_history)) <- c("time", "gear")

sim8 <- project(params, effort=f_history, dt=0.1)
plot(sim8)
plotBiomass(sim8, start_time=2015, end_time = 2100)

f2 <- plotBiomass(sim8, start_time=2010, end_time = 2100) + theme(legend.position = "none", axis.title.y = element_blank())


#metrics for time to recovery
biomass8 <- as.data.frame(getBiomass(sim8))
biomass8$Community <- rowSums(biomass8)
#in case year needs to be a row
Year <- rownames(biomass8)
#rownames(biomass5) <- NULL
#biomass5 <- cbind(Year, biomass5)

#baseline average (2017-2021) prior to change in effort
baseline8 <- biomass8[97:101,]
baseline8 <- colMeans(baseline8)
baseline8 <-as.vector(baseline8)

#calculate five year rolling average biomass for each species and community as a whole
library(zoo)
roll_mean <- as.data.frame(rollmean(biomass8, k=5, align="right", fill=NA))
#divide by baseline to track changes
changes <- sweep(roll_mean, 2, baseline8, "/")
changes <- cbind(Year, changes)

#look from years after 2021
changes2 <- changes[101:200,]

data_long <- gather(changes2, Species, Relative_biomass, "Cisco":"Community", factor_key = TRUE)
data_long$Year <- as.numeric(data_long$Year)

p <- ggplot(data=data_long, aes(x=Year, y=Relative_biomass, group=Species)) + geom_line(aes(color=Species)) 

p + geom_hline(yintercept = 1.05, linetype = "dashed") + geom_hline(yintercept = 0.95, linetype = "dashed")

################################################################################################################################
#Scenario 9: Increase fishing effort for Northern Pike
gear_params <- data.frame(species = species_params2$species,
                          gear = species_params2$species,
                          sel_func = "sigmoid_length",
                          l25 = species_params2$l25,
                          l50= species_params2$l50)

f_history <- read.csv("f_history_scenario9.csv", header= TRUE, sep =",", row.names=1)
colnames(f_history) <- c("Cisco", "Forage Fish", "Northern Pike", "Walleye", "Yellow Perch")
f_history <- as.matrix(f_history)

params <- MizerParams(species_params2,
                      interaction = inter,
                      kappa = 150,
                      gear_params = gear_params,
                      w_pp_cutoff = 2)

names(dimnames(f_history)) <- c("time", "gear")

sim9 <- project(params, effort=f_history, dt=0.1)
plot(sim9)
plotBiomass(sim9, start_time=2015, end_time = 2100)

#get mean slope for final years (2090-2100)
slope <- as.data.frame(getCommunitySlope(sim9)[170:180,,])
colMeans(slope)
colSds(as.matrix(slope))

#get mean biomass preceding change
bio1 <- as.data.frame(getBiomass(sim9))
bio1[,6] <- rowSums(bio1)
bio2 <- bio1[91:101,]
colMeans(bio2)

#mean biomass in final ten years
bio3 <- bio1[170:180,]
colMeans(bio3)

p5 <- plotBiomass(sim9, start_time=2010, end_time = 2100) + theme(legend.position = "none", xlab=NULL, axis.title.y = element_blank(), axis.title.x = element_blank()) + geom_vline(xintercept = 2022, linetype = "dashed")


###################################################################################################################################
#Scenario 10: Decrease fishing effort for Northern Pike
gear_params <- data.frame(species = species_params2$species,
                          gear = species_params2$species,
                          sel_func = "sigmoid_length",
                          l25 = species_params2$l25,
                          l50= species_params2$l50)

f_history <- read.csv("f_history_scenario10.csv", header= TRUE, sep =",", row.names=1)
colnames(f_history) <- c("Cisco", "Forage Fish", "Northern Pike", "Walleye", "Yellow Perch")
f_history <- as.matrix(f_history)

params <- MizerParams(species_params2,
                      interaction = inter,
                      kappa = 150,
                      gear_params = gear_params,
                      w_pp_cutoff = 2)

names(dimnames(f_history)) <- c("time", "gear")

sim10 <- project(params, effort=f_history, dt=0.1)
plot(sim10)
plotBiomass(sim10, start_time=2015, end_time = 2200)

#get mean slope for final years (2090-2100)
slope <- as.data.frame(getCommunitySlope(sim10)[170:180,,])
colMeans(slope)
colSds(as.matrix(slope))

#get mean biomass preceding change
bio1 <- as.data.frame(getBiomass(sim10))
bio1[,6] <- rowSums(bio1)
bio2 <- bio1[91:101,]
colMeans(bio2)

#mean biomass in final ten years
bio3 <- bio1[170:180,]
colMeans(bio3)

p6 <- plotBiomass(sim10, start_time=2010, end_time = 2100) + theme(legend.position = "none", xlab=NULL, axis.title.y = element_blank()) + geom_vline(xintercept = 2022, linetype = "dashed")


#################################################################################################################################
#Scenario 11: Increase fishing effort for Cisco
gear_params <- data.frame(species = species_params2$species,
                          gear = species_params2$species,
                          sel_func = "sigmoid_length",
                          l25 = species_params2$l25,
                          l50= species_params2$l50)

f_history <- read.csv("f_history_scenario11.csv", header= TRUE, sep =",", row.names=1)
colnames(f_history) <- c("Cisco", "Forage Fish", "Northern Pike", "Walleye", "Yellow Perch")
f_history <- as.matrix(f_history)

params <- MizerParams(species_params2,
                      interaction = inter,
                      kappa = 150,
                      gear_params = gear_params,
                      w_pp_cutoff = 2)

names(dimnames(f_history)) <- c("time", "gear")

sim11 <- project(params, effort=f_history, dt=0.1)
plot(sim11)
plotBiomass(sim11, start_time=2015, end_time = 2100)

#get mean slope for final years (2090-2100)
slope <- as.data.frame(getCommunitySlope(sim11)[170:180,,])
colMeans(slope)
colSds(as.matrix(slope))

#get mean biomass preceding change
bio1 <- as.data.frame(getBiomass(sim11))
bio1[,6] <- rowSums(bio1)
bio2 <- bio1[91:101,]
colMeans(bio2)

#mean biomass in final ten years
bio3 <- bio1[170:180,]
colMeans(bio3)

p7 <- plotBiomass(sim11, start_time=2010, end_time = 2100) + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank()) + geom_vline(xintercept = 2022, linetype = "dashed")
#################################################################################################################################
#Scenario 12: Decrease fishing effort for cisco
gear_params <- data.frame(species = species_params2$species,
                          gear = species_params2$species,
                          sel_func = "sigmoid_length",
                          l25 = species_params2$l25,
                          l50= species_params2$l50)

f_history <- read.csv("f_history_scenario12.csv", header= TRUE, sep =",", row.names=1)
colnames(f_history) <- c("Cisco", "Forage Fish", "Northern Pike", "Walleye", "Yellow Perch")
f_history <- as.matrix(f_history)

params <- MizerParams(species_params2,
                      interaction = inter,
                      kappa = 150,
                      gear_params = gear_params,
                      w_pp_cutoff = 2)

names(dimnames(f_history)) <- c("time", "gear")

sim12 <- project(params, effort=f_history, dt=0.1)
plot(sim12)
plotBiomass(sim12, start_time=2015, end_time = 2100)

#get mean slope for final years (2090-2100)
slope <- as.data.frame(getCommunitySlope(sim12)[170:180,,])
colMeans(slope)
colSds(as.matrix(slope))

#get mean biomass preceding change
bio1 <- as.data.frame(getBiomass(sim12))
bio1[,6] <- rowSums(bio1)
bio2 <- bio1[91:101,]
colMeans(bio2)

#mean biomass in final ten years
bio3 <- bio1[170:180,]
colMeans(bio3)

p8 <- plotBiomass(sim12, start_time=2010, end_time = 2100) + theme(legend.position = "none", xlab=NULL, axis.title.y = element_blank()) + geom_vline(xintercept = 2022, linetype = "dashed")

################################################################################################################################
#Scenario 13: All fishing stopped
gear_params <- data.frame(species = species_params2$species,
                          gear = species_params2$species,
                          sel_func = "sigmoid_length",
                          l25 = species_params2$l25,
                          l50= species_params2$l50)

f_history <- read.csv("f_history_scenario13.csv", header= TRUE, sep =",", row.names=1)
colnames(f_history) <- c("Cisco", "Forage Fish", "Northern Pike", "Walleye", "Yellow Perch")
f_history <- as.matrix(f_history)

params <- MizerParams(species_params2,
                      interaction = inter,
                      kappa = 150,
                      gear_params = gear_params,
                      w_pp_cutoff = 2)

names(dimnames(f_history)) <- c("time", "gear")

sim13 <- project(params, effort=f_history, dt=0.1)
plot(sim13)
plotBiomass(sim13, start_time=2015, end_time = 2200)

#get mean slope for years preceding change (2011-2021)
slope <- as.data.frame(getCommunitySlope(sim13)[91:101,,])
colMeans(slope)
colSds(as.matrix(slope))

#get mean slope for final years (2090-2100)
slope <- as.data.frame(getCommunitySlope(sim13)[170:180,,])
colMeans(slope)
colSds(as.matrix(slope))

#get mean biomass preceding change
bio1 <- as.data.frame(getBiomass(sim13))
bio1[,6] <- rowSums(bio1)
bio2 <- bio1[91:101,]
colMeans(bio2)

#mean biomass in final ten years
bio3 <- bio1[170:180,]
colMeans(bio3)

############################################################################################################################
#Scenario 14: Dynamic fishing with increase in walleye effort & decrease in perch effort
gear_params <- data.frame(species = species_params2$species,
                          gear = species_params2$species,
                          sel_func = "sigmoid_length",
                          l25 = species_params2$l25,
                          l50= species_params2$l50)

f_history <- read.csv("f_history_scenario14.csv", header= TRUE, sep =",", row.names=1)
colnames(f_history) <- c("Cisco", "Forage Fish", "Northern Pike", "Walleye", "Yellow Perch")
f_history <- as.matrix(f_history)

params <- MizerParams(species_params2,
                      interaction = inter,
                      kappa = 150,
                      gear_params = gear_params,
                      w_pp_cutoff = 2)

names(dimnames(f_history)) <- c("time", "gear")

sim14 <- project(params, effort=f_history, dt=0.1)
plot(sim14)
plotBiomass(sim14, start_time=2015, end_time = 2100)

################################################################################################################################
#Scenario 15: Dynamic fishing with decrease in walleye effort and increas in yellow perch effort
gear_params <- data.frame(species = species_params2$species,
                          gear = species_params2$species,
                          sel_func = "sigmoid_length",
                          l25 = species_params2$l25,
                          l50= species_params2$l50)

f_history <- read.csv("f_history_scenario15.csv", header= TRUE, sep =",", row.names=1)
colnames(f_history) <- c("Cisco", "Forage Fish", "Northern Pike", "Walleye", "Yellow Perch")
f_history <- as.matrix(f_history)

params <- MizerParams(species_params2,
                      interaction = inter,
                      kappa = 150,
                      gear_params = gear_params,
                      w_pp_cutoff = 2)

names(dimnames(f_history)) <- c("time", "gear")

sim15 <- project(params, effort=f_history, dt=0.1)
plot(sim15)
plotBiomass(sim15, start_time=2015, end_time = 2100)

#################################################################################################################################
#Scenario 16: Dynamic fishing with increase in walleye effort & decrease in pike
gear_params <- data.frame(species = species_params2$species,
                          gear = species_params2$species,
                          sel_func = "sigmoid_length",
                          l25 = species_params2$l25,
                          l50= species_params2$l50)

f_history <- read.csv("f_history_scenario16.csv", header= TRUE, sep =",", row.names=1)
colnames(f_history) <- c("Cisco", "Forage Fish", "Northern Pike", "Walleye", "Yellow Perch")
f_history <- as.matrix(f_history)

params <- MizerParams(species_params2,
                      interaction = inter,
                      kappa = 150,
                      gear_params = gear_params,
                      w_pp_cutoff = 2)

names(dimnames(f_history)) <- c("time", "gear")

sim16 <- project(params, effort=f_history, dt=0.1)
plot(sim16)
plotBiomass(sim16, start_time=2015, end_time = 2100)

#get mean slope for years preceding change (2011-2021)
slope <- as.data.frame(getCommunitySlope(sim16)[91:101,,])
colMeans(slope)
colSds(as.matrix(slope))

#get mean slope for final years (2090-2100)
slope <- as.data.frame(getCommunitySlope(sim16)[170:180,,])
colMeans(slope)
colSds(as.matrix(slope))

#get mean biomass preceding change
bio1 <- as.data.frame(getBiomass(sim16))
bio1[,6] <- rowSums(bio1)
bio2 <- bio1[91:101,]
colMeans(bio2)

#mean biomass in final ten years
bio3 <- bio1[170:180,]
colMeans(bio3)

g2 <- plotBiomass(sim16, start_time=2010, end_time = 2100) + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank()) + geom_vline(xintercept = 2022, linetype = "dashed")


#################################################################################################################################
#Scenario 17: Dynamic fishing with DECREASE in walleye effort & INCREASE in pike
gear_params <- data.frame(species = species_params2$species,
                          gear = species_params2$species,
                          sel_func = "sigmoid_length",
                          l25 = species_params2$l25,
                          l50= species_params2$l50)

f_history <- read.csv("f_history_scenario17.csv", header= TRUE, sep =",", row.names=1)
colnames(f_history) <- c("Cisco", "Forage Fish", "Northern Pike", "Walleye", "Yellow Perch")
f_history <- as.matrix(f_history)

params <- MizerParams(species_params2,
                      interaction = inter,
                      kappa = 150,
                      gear_params = gear_params,
                      w_pp_cutoff = 2)

names(dimnames(f_history)) <- c("time", "gear")

sim17 <- project(params, effort=f_history, dt=0.1)
plot(sim17)
plotBiomass(sim17, start_time=2015, end_time = 2100)

#get mean slope for years preceding change (2011-2021)
slope <- as.data.frame(getCommunitySlope(sim17)[91:101,,])
colMeans(slope)
colSds(as.matrix(slope))

#get mean slope for final years (2090-2100)
slope <- as.data.frame(getCommunitySlope(sim17)[170:180,,])
colMeans(slope)
colSds(as.matrix(slope))

#get mean biomass preceding change
bio1 <- as.data.frame(getBiomass(sim17))
bio1[,6] <- rowSums(bio1)
bio2 <- bio1[91:101,]
colMeans(bio2)

#mean biomass in final ten years
bio3 <- bio1[170:180,]
colMeans(bio3)

g3 <- plotBiomass(sim17, start_time=2010, end_time = 2100) + theme(legend.position = "none", axis.title.y = element_blank()) + geom_vline(xintercept = 2022, linetype = "dashed")


######################################################################################################################################
#Scenario 18: Increase effort for both pike and walleye

gear_params <- data.frame(species = species_params2$species,
                          gear = species_params2$species,
                          sel_func = "sigmoid_length",
                          l25 = species_params2$l25,
                          l50= species_params2$l50)

f_history <- read.csv("f_history_scenario18.csv", header= TRUE, sep =",", row.names=1)
colnames(f_history) <- c("Cisco", "Forage Fish", "Northern Pike", "Walleye", "Yellow Perch")
f_history <- as.matrix(f_history)

params <- MizerParams(species_params2,
                      interaction = inter,
                      kappa = 150,
                      gear_params = gear_params,
                      w_pp_cutoff = 2)

names(dimnames(f_history)) <- c("time", "gear")

sim18 <- project(params, effort=f_history, dt=0.1)
plot(sim18)
plotBiomass(sim18, start_time=2015, end_time = 2100)

#get mean slope for years preceding change (2011-2021)
slope <- as.data.frame(getCommunitySlope(sim18)[91:101,,])
colMeans(slope)
colSds(as.matrix(slope))

#get mean slope for final years (2090-2100)
slope <- as.data.frame(getCommunitySlope(sim18)[170:180,,])
colMeans(slope)
colSds(as.matrix(slope))

#get mean biomass preceding change
bio1 <- as.data.frame(getBiomass(sim18))
bio1[,6] <- rowSums(bio1)
bio2 <- bio1[91:101,]
colMeans(bio2)

#mean biomass in final ten years
bio3 <- bio1[170:180,]
colMeans(bio3)

g1 <- plotBiomass(sim18, start_time=2010, end_time = 2100) + theme(axis.title.x = element_blank()) + geom_vline(xintercept = 2022, linetype = "dashed")


#################################################################################################################################
#Scenario 19: Decrease effort for both pike and walleye

gear_params <- data.frame(species = species_params2$species,
                          gear = species_params2$species,
                          sel_func = "sigmoid_length",
                          l25 = species_params2$l25,
                          l50= species_params2$l50)

f_history <- read.csv("f_history_scenario19.csv", header= TRUE, sep =",", row.names=1)
colnames(f_history) <- c("Cisco", "Forage Fish", "Northern Pike", "Walleye", "Yellow Perch")
f_history <- as.matrix(f_history)

params <- MizerParams(species_params2,
                      interaction = inter,
                      kappa = 150,
                      gear_params = gear_params,
                      w_pp_cutoff = 2)

names(dimnames(f_history)) <- c("time", "gear")

sim19 <- project(params, effort=f_history, dt=0.1)
plot(sim19)
plotBiomass(sim19, start_time=2015, end_time = 2100)

#get mean slope for years preceding change (2011-2021)
slope <- as.data.frame(getCommunitySlope(sim19)[91:101,,])
colMeans(slope)
colSds(as.matrix(slope))

#get mean slope for final years (2090-2100)
slope <- as.data.frame(getCommunitySlope(sim19)[170:180,,])
colMeans(slope)
colSds(as.matrix(slope))

#get mean biomass preceding change
bio1 <- as.data.frame(getBiomass(sim19))
bio1[,6] <- rowSums(bio1)
bio2 <- bio1[91:101,]
colMeans(bio2)

#mean biomass in final ten years
bio3 <- bio1[170:180,]
colMeans(bio3)

g4 <- plotBiomass(sim19, start_time=2010, end_time = 2100) + theme(legend.position = "none") + geom_vline(xintercept = 2022, linetype = "dashed")


#############################################################################################################################
#Scenario 22: Realistic increase for Walleye followed by decrease to assess recovery (effort increase of 50%)
gear_params <- data.frame(species = species_params2$species,
                          gear = species_params2$species,
                          sel_func = "sigmoid_length",
                          l25 = species_params2$l25,
                          l50= species_params2$l50)

f_history <- read.csv("f_history_scenario22.csv", header= TRUE, sep =",", row.names=1)
colnames(f_history) <- c("Cisco", "Forage Fish", "Northern Pike", "Walleye", "Yellow Perch")
f_history <- as.matrix(f_history)

params <- MizerParams(species_params2,
                      interaction = inter,
                      kappa = 150,
                      gear_params = gear_params,
                      w_pp_cutoff = 2)

names(dimnames(f_history)) <- c("time", "gear")

sim22 <- project(params, effort=f_history, dt=0.1)
plot(sim22)
plotBiomass(sim22, start_time=2015, end_time = 2100)

f1 <- plotBiomass(sim22, start_time=2010, end_time = 2100) + theme(axis.title.x = element_blank()) + geom_vline(xintercept = 2022, linetype = "dashed")+ geom_vline(xintercept = 2060, linetype = "dashed")


#metrics for time to recovery
biomass22 <- as.data.frame(getBiomass(sim22))
biomass22$Community <- rowSums(biomass22)
#in case year needs to be a row
Year <- rownames(biomass22)
#rownames(biomass5) <- NULL
#biomass5 <- cbind(Year, biomass5)

#baseline average (2017-2021) prior to change in effort
baseline22 <- biomass22[97:101,]
baseline22 <- colMeans(baseline22)
baseline22 <-as.vector(baseline22)

#calculate five year rolling average biomass for each species and community as a whole
roll_mean <- as.data.frame(rollmean(biomass22, k=5, align="right", fill=NA))
#divide by baseline to track changes
changes <- sweep(roll_mean, 2, baseline22, "/")
changes <- cbind(Year, changes)

#look from years after 2021
changes2 <- changes[101:200,]

data_long <- gather(changes2, Species, Relative_biomass, "Cisco":"Community", factor_key = TRUE)
data_long$Year <- as.numeric(data_long$Year)

p <- ggplot(data=data_long, aes(x=Year, y=Relative_biomass, group=Species)) + geom_line(aes(color=Species)) 

p + geom_hline(yintercept = 1.05, linetype = "dashed") + geom_hline(yintercept = 0.95, linetype = "dashed")

###############################################################################################################################################################################
#Scenario 23: Realistic increase for Northern Pike followed by decrease to assess recovery (effort increase of 50%)
gear_params <- data.frame(species = species_params2$species,
                          gear = species_params2$species,
                          sel_func = "sigmoid_length",
                          l25 = species_params2$l25,
                          l50= species_params2$l50)

f_history <- read.csv("f_history_scenario23.csv", header= TRUE, sep =",", row.names=1)
colnames(f_history) <- c("Cisco", "Forage Fish", "Northern Pike", "Walleye", "Yellow Perch")
f_history <- as.matrix(f_history)

params <- MizerParams(species_params2,
                      interaction = inter,
                      kappa = 150,
                      gear_params = gear_params,
                      w_pp_cutoff = 2)

names(dimnames(f_history)) <- c("time", "gear")

sim23 <- project(params, effort=f_history, dt=0.1)
plot(sim23)
plotBiomass(sim23, start_time=2015, end_time = 2100)

f2 <- plotBiomass(sim23, start_time=2010, end_time = 2100) + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank()) + geom_vline(xintercept = 2022, linetype = "dashed") + geom_vline(xintercept = 2060, linetype = "dashed")


#metrics for time to recovery
biomass23 <- as.data.frame(getBiomass(sim23))
biomass23$Community <- rowSums(biomass23)
#in case year needs to be a row
Year <- rownames(biomass23)
#rownames(biomass5) <- NULL
#biomass5 <- cbind(Year, biomass5)

#baseline average (2017-2021) prior to change in effort
baseline23 <- biomass23[97:101,]
baseline23 <- colMeans(baseline23)
baseline23 <-as.vector(baseline23)

#calculate five year rolling average biomass for each species and community as a whole
roll_mean <- as.data.frame(rollmean(biomass23, k=5, align="right", fill=NA))
#divide by baseline to track changes
changes <- sweep(roll_mean, 2, baseline23, "/")
changes <- cbind(Year, changes)

#look from years after 2021
changes2 <- changes[101:200,]

data_long <- gather(changes2, Species, Relative_biomass, "Cisco":"Community", factor_key = TRUE)
data_long$Year <- as.numeric(data_long$Year)

p <- ggplot(data=data_long, aes(x=Year, y=Relative_biomass, group=Species)) + geom_line(aes(color=Species)) 

p + geom_hline(yintercept = 1.05, linetype = "dashed") + geom_hline(yintercept = 0.95, linetype = "dashed")

#########################################################################################################################################################
#Scenario 24: Realistic increase for Yellow Perch followed by decrease to assess recovery (effort increase of 50%)
gear_params <- data.frame(species = species_params2$species,
                          gear = species_params2$species,
                          sel_func = "sigmoid_length",
                          l25 = species_params2$l25,
                          l50= species_params2$l50)

f_history <- read.csv("f_history_scenario24.csv", header= TRUE, sep =",", row.names=1)
colnames(f_history) <- c("Cisco", "Forage Fish", "Northern Pike", "Walleye", "Yellow Perch")
f_history <- as.matrix(f_history)

params <- MizerParams(species_params2,
                      interaction = inter,
                      kappa = 150,
                      gear_params = gear_params,
                      w_pp_cutoff = 2)

names(dimnames(f_history)) <- c("time", "gear")

sim24 <- project(params, effort=f_history, dt=0.1)
plot(sim24)
plotBiomass(sim24, start_time=2015, end_time = 2100)

f3 <- plotBiomass(sim24, start_time=2010, end_time = 2100) + theme(legend.position = "none") + geom_vline(xintercept = 2022, linetype = "dashed") + geom_vline(xintercept = 2060, linetype = "dashed")


#metrics for time to recovery
biomass24 <- as.data.frame(getBiomass(sim24))
biomass24$Community <- rowSums(biomass24)
#in case year needs to be a row
Year <- rownames(biomass24)
#rownames(biomass5) <- NULL
#biomass5 <- cbind(Year, biomass5)

#baseline average (2017-2021) prior to change in effort
baseline24 <- biomass24[97:101,]
baseline24 <- colMeans(baseline24)
baseline24 <-as.vector(baseline24)

#calculate five year rolling average biomass for each species and community as a whole
roll_mean <- as.data.frame(rollmean(biomass24, k=5, align="right", fill=NA))
#divide by baseline to track changes
changes <- sweep(roll_mean, 2, baseline24, "/")
changes <- cbind(Year, changes)

#look from years after 2021
changes2 <- changes[101:200,]

data_long <- gather(changes2, Species, Relative_biomass, "Cisco":"Community", factor_key = TRUE)
data_long$Year <- as.numeric(data_long$Year)

p <- ggplot(data=data_long, aes(x=Year, y=Relative_biomass, group=Species)) + geom_line(aes(color=Species)) 

p + geom_hline(yintercept = 1.05, linetype = "dashed") + geom_hline(yintercept = 0.95, linetype = "dashed")

##############################################################################################################################
#Scenario 25: Realistic increase for Cisco followed by decrease to assess recovery (effort increase of 50%)
gear_params <- data.frame(species = species_params2$species,
                          gear = species_params2$species,
                          sel_func = "sigmoid_length",
                          l25 = species_params2$l25,
                          l50= species_params2$l50)

f_history <- read.csv("f_history_scenario25.csv", header= TRUE, sep =",", row.names=1)
colnames(f_history) <- c("Cisco", "Forage Fish", "Northern Pike", "Walleye", "Yellow Perch")
f_history <- as.matrix(f_history)

params <- MizerParams(species_params2,
                      interaction = inter,
                      kappa = 150,
                      gear_params = gear_params,
                      w_pp_cutoff = 2)

names(dimnames(f_history)) <- c("time", "gear")

sim25 <- project(params, effort=f_history, dt=0.1)
plot(sim25)
plotBiomass(sim25, start_time=2015, end_time = 2100)

f4 <- plotBiomass(sim25, start_time=2010, end_time = 2100) + theme(legend.position = "none", axis.title.y = element_blank()) + geom_vline(xintercept = 2022, linetype = "dashed") + geom_vline(xintercept = 2060, linetype = "dashed")


#metrics for time to recovery
biomass25 <- as.data.frame(getBiomass(sim25))
biomass25$Community <- rowSums(biomass25)
#in case year needs to be a row
Year <- rownames(biomass25)
#rownames(biomass5) <- NULL
#biomass5 <- cbind(Year, biomass5)

#baseline average (2017-2021) prior to change in effort
baseline25 <- biomass25[97:101,]
baseline25 <- colMeans(baseline25)
baseline25 <-as.vector(baseline25)

#calculate five year rolling average biomass for each species and community as a whole
roll_mean <- as.data.frame(rollmean(biomass25, k=5, align="right", fill=NA))
#divide by baseline to track changes
changes <- sweep(roll_mean, 2, baseline25, "/")
changes <- cbind(Year, changes)

#look from years after 2021
changes2 <- changes[101:200,]

data_long <- gather(changes2, Species, Relative_biomass, "Cisco":"Community", factor_key = TRUE)
data_long$Year <- as.numeric(data_long$Year)

p <- ggplot(data=data_long, aes(x=Year, y=Relative_biomass, group=Species)) + geom_line(aes(color=Species)) 

p + geom_hline(yintercept = 1.05, linetype = "dashed") + geom_hline(yintercept = 0.95, linetype = "dashed")
#######
#Chapter 3 
#Figure 1
library(cowplot)
prow <- plot_grid(p2, p6, p4, p1 + theme(legend.position = "none"),p5,p3, labels=c("(a)", "(c)", "(e)", "(b)", "(d)", "(f)"), label_size = 10, hjust =-2, vjust = 22, nrow=2)

legend <- get_legend(p1 + guides(color= guide_legend(nrow = 1)) + theme(legend.position = "bottom"))

plot_grid(prow, legend, ncol=1, rel_heights = c(1,.1))

#with cisco
prow <- plot_grid(p1 + theme(legend.position = "none"),p5,p3,p7,p2,p6,p4,p8, labels=c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)", "(g)", "(h)"), label_size = 10, hjust =-2, vjust = 22, nrow=2)

legend <- get_legend(p1 + guides(color= guide_legend(nrow = 2)) + theme(legend.position = "bottom"))

plot_grid(prow, legend, ncol=1, rel_heights = c(1,.1))





#Figure 2
prow2 <- plot_grid(g1 + theme(legend.position = "none"),g2,g4,g3, labels=c("(a)", "(b)", "(c)", "(d)"), label_size = 10, hjust =-1.5, vjust = 22, nrow=2)
legend <- get_legend(g1 + guides(color= guide_legend(nrow = 1)) + theme(legend.position = "right"))

plot_grid(prow2, legend, ncol=1, rel_heights = c(1,.1))

#Figure 4
prow3 <- plot_grid(f1 + theme(legend.position = "none"),f2, f3, f4, labels=c("(a)", "(b)", "(c)", "(d)"), label_size = 10, hjust =-1, vjust = 22, nrow=2)

legend <- get_legend(f1 + guides(color= guide_legend(nrow = 1)) + theme(legend.position = "right"))
plot_grid(prow3, legend, ncol=1, rel_heights = c(1,.1))

##supplemental figure for cisco
prow4 <- plot_grid(c1+ theme(legend.position = "none"), c2, labels=c("(a)", "(b)"), label_size = 10, hjust =-1.5, vjust = 44, nrow=1)
legend <- get_legend(c1 + guides(color= guide_legend(nrow = 1)) + theme(legend.position = "bottom"))

plot_grid(prow4, legend, ncol=1, rel_heights = c(1,.1))

#supplemental plot for growth curves
 a1 <- plotGrowthCurves(sim, "Walleye") + theme(axis.title.x = element_blank())
 a2 <- plotGrowthCurves(sim, "Northern Pike") + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
 a3 <- plotGrowthCurves(sim, "Yellow Perch") + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
 a4 <- plotGrowthCurves(sim, "Cisco") + theme(legend.position = "none")
 a5 <- plotGrowthCurves(sim, "Forage Fish") + theme(legend.position = "none", axis.title.y = element_blank())

 
 prow5 <- plot_grid(a1 + theme(legend.position = "none"),a2,a3,a4,a5, labels=c("(a)", "(b)", "(c)", "(d)", "(e)"), label_size = 10, hjust =-1.5, vjust = 22, nrow=2)
 legend <- get_legend(a1 + guides(color= guide_legend(nrow = 1)) + theme(legend.position = "right"))
 
 plot_grid(prow5, legend, ncol=1, rel_heights = c(1,.1))
 