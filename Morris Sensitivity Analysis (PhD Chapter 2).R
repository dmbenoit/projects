#Morris Test
#Elemental effects screening using OAT methods for mizer model with 7 species
#Represents Lake Nipissing with R_max values estimated from body size relationship

#####################################################################################

#Case 1: Sensitivity of parameters on slope

library (mizer)
library(sensitivity)


inter <- read.csv("int_ls_new.csv", header = TRUE, sep = ",", row.names = NULL, stringsAsFactors = TRUE )
inter <- inter[-c(1)]
colnames(inter) <- c("Common Shiner", "Cisco", "Freshwater Drum", "Northern Pike", "Trout-perch", "Walleye", "Yellow Perch")
rownames(inter) <- colnames(inter)
inter <- as.matrix(inter)

#X = w_inf, w_mat, beta, sigma, R_max, k_vb
#param <- as.vector(c("w_inf", "w_mat", "beta", "sigma", "R_max", "k_vb", ))
table <- as.matrix(read.csv("morris.table.csv", header=TRUE, sep =",", row.names = 1))

mizer.model <- function(param){
  #create emptty dataframe with necessary headers
  df <- data.frame(matrix(ncol = 7, nrow=7))
  x <- c("species", "w_inf", "w_mat", "beta", "sigma", "R_max", "k_vb")
  colnames(df) <- x
  #df <- data.frame(species = NA, w_mat = NA, w_inf = NA, beta = NA, sigma = NA, R_max = NA, k_vb = NA)
  #add first species
  df$species[1] <- "Common Shiner"
  df$w_inf[1] <- param["w_inf.1"]
  df$w_mat[1] <- param["w_mat.1"]
  df$beta[1] <- param["beta.1"]
  df$sigma[1] <- param["sigma.1"]
  df$R_max[1] <- param["R_max.1"]
  df$k_vb[1] <- param["k_vb.1"]
  #add second species
  df$species[2] <- "Cisco"
  df$w_inf[2] <- param["w_inf.2"]
  df$w_mat[2] <- param["w_mat.2"]
  df$beta[2] <- param["beta.2"]
  df$sigma[2] <- param["sigma.2"]
  df$R_max[2] <- param["R_max.2"]
  df$k_vb[2] <- param["k_vb.2"]
  #add third species
  df$species[3] <- "Freshwater Drum"
  df$w_inf[3] <- param["w_inf.3"]
  df$w_mat[3] <- param["w_mat.3"]
  df$beta[3] <- param["beta.3"]
  df$sigma[3] <- param["sigma.3"]
  df$R_max[3] <- param["R_max.3"]
  df$k_vb[3] <- param["k_vb.3"]
  #add fourth species
  df$species[4] <- "Northern Pike"
  df$w_inf[4] <- param["w_inf.4"]
  df$w_mat[4] <- param["w_mat.4"]
  df$beta[4] <- param["beta.4"]
  df$sigma[4] <- param["sigma.4"]
  df$R_max[4] <- param["R_max.4"]
  df$k_vb[4] <- param["k_vb.4"]
  #add fifth species
  df$species[5] <- "Trout-perch"
  df$w_inf[5] <- param["w_inf.5"]
  df$w_mat[5] <- param["w_mat.5"]
  df$beta[5] <- param["beta.5"]
  df$sigma[5] <- param["sigma.5"]
  df$R_max[5] <- param["R_max.5"]
  df$k_vb[5] <- param["k_vb.5"]
  #add sixth species
  df$species[6] <- "Walleye"
  df$w_inf[6] <- param["w_inf.6"]
  df$w_mat[6] <- param["w_mat.6"]
  df$beta[6] <- param["beta.6"]
  df$sigma[6] <- param["sigma.6"]
  df$R_max[6] <- param["R_max.6"]
  df$k_vb[6] <- param["k_vb.6"]
  #add seventh species
  df$species[7] <- "Yellow Perch"
  df$w_inf[7] <- param["w_inf.7"]
  df$w_mat[7] <- param["w_mat.7"]
  df$beta[7] <- param["beta.7"]
  df$sigma[7] <- param["sigma.7"]
  df$R_max[7] <- param["R_max.7"]
  df$k_vb[7] <- param["k_vb.7"]
  
  #run mizer and get output
  params <- MizerParams(species_params = df, interaction = inter, kappa = 10, w_pp_cutoff = 2)
  sim <- project(params, effort = 0, t_max=500, dt=0.1, t_save=1)
  output <- getCommunitySlope(sim)[500,,]$slope
  #plot(sim)
  return(output)
  
  
}

mizer.simule <- function(X){
  Y = apply(X,1,function(param) mizer.model(param))
  return(as.matrix(Y))
}

ParaNames <- c("w_inf.1", "w_mat.1", "beta.1", "sigma.1", "R_max.1", "k_vb.1", 
               "w_inf.2", "w_mat.2", "beta.2", "sigma.2", "R_max.2", "k_vb.2",
               "w_inf.3", "w_mat.3", "beta.3", "sigma.3", "R_max.3", "k_vb.3",
               "w_inf.4", "w_mat.4", "beta.4", "sigma.4", "R_max.4", "k_vb.4",
               "w_inf.5", "w_mat.5", "beta.5", "sigma.5", "R_max.5", "k_vb.5",
               "w_inf.6", "w_mat.6", "beta.6", "sigma.6", "R_max.6", "k_vb.6",
               "w_inf.7", "w_mat.7", "beta.7", "sigma.7", "R_max.7", "k_vb.7")
morris_output <- morris(model = mizer.simule, factors = ParaNames, r = 40,
                        design = list(type = "oat", levels = 10, grid.jump = 5),
                        scale = T,
                        binf = as.vector(table["binf",]), bsup = as.vector(table["bsup",]))


print(morris_output)
plot(morris_output)



############################################################

#Case 2: Sensitivity of parameters on linearity 

#load necessary functions to calculate measure of linearity (MSE)
library(dvmisc)

getCommunitySlope2 <- function(sim, species = 1:nrow(sim@params@species_params),
                               biomass = TRUE, ...) {
  check_species(sim, species)
  size_range <- get_size_range_array(sim@params, ...)
  # set entries for unwanted sizes to zero and sum over wanted species, giving
  # array (time x size)
  total_n <-
    apply(sweep(sim@n, c(2, 3), size_range, "*")[, species, , drop = FALSE],
          c(1, 3), sum)
  # numbers or biomass?
  if (biomass)
    total_n <- sweep(total_n, 2, sim@params@w, "*")
  # previously unwanted entries were set to zero, now set them to NA
  # so that they will be ignored when fitting the linear model
  total_n[total_n <= 0] <- NA
  # fit linear model at every time and put result in data frame
  slope <- plyr::adply(total_n, 1, function(x, w) {
    lm.fit <- lm(log(x) ~ log(w))
    summary_fit <- summary(lm.fit)
    out_df <- data.frame(
      slope = summary_fit$coefficients[2, 1],
      intercept = summary_fit$coefficients[1, 1],
      r2 = summary_fit$r.squared,
      mse = get_mse(lm.fit)
    )
  }, w = sim@params@w)
  dimnames(slope)[[1]] <- slope[, 1]
  slope <- slope[, -1]
  return(slope)
}


# internal
check_species <- function(object, species){
  if (!(is(species,"character") | is(species,"numeric")))
    stop("species argument must be either a numeric or character vector")
  if (is(species,"character"))
    check <- all(species %in% dimnames(object@n)$sp)  
  if (is(species,"numeric"))
    check <- all(species %in% 1:dim(object@n)[2])
  if (!check)
    stop("species argument not in the model species. species must be a character vector of names in the model, or a numeric vector referencing the species")
  return(check)
}

mizer.linearity <- function(param){
  #create emptty dataframe with necessary headers
  df <- data.frame(matrix(ncol = 7, nrow=7))
  x <- c("species", "w_inf", "w_mat", "beta", "sigma", "R_max", "k_vb")
  colnames(df) <- x
  #df <- data.frame(species = NA, w_mat = NA, w_inf = NA, beta = NA, sigma = NA, R_max = NA, k_vb = NA)
  #add first species
  df$species[1] <- "Common Shiner"
  df$w_inf[1] <- param["w_inf.1"]
  df$w_mat[1] <- param["w_mat.1"]
  df$beta[1] <- param["beta.1"]
  df$sigma[1] <- param["sigma.1"]
  df$R_max[1] <- param["R_max.1"]
  df$k_vb[1] <- param["k_vb.1"]
  #add second species
  df$species[2] <- "Cisco"
  df$w_inf[2] <- param["w_inf.2"]
  df$w_mat[2] <- param["w_mat.2"]
  df$beta[2] <- param["beta.2"]
  df$sigma[2] <- param["sigma.2"]
  df$R_max[2] <- param["R_max.2"]
  df$k_vb[2] <- param["k_vb.2"]
  #add third species
  df$species[3] <- "Freshwater Drum"
  df$w_inf[3] <- param["w_inf.3"]
  df$w_mat[3] <- param["w_mat.3"]
  df$beta[3] <- param["beta.3"]
  df$sigma[3] <- param["sigma.3"]
  df$R_max[3] <- param["R_max.3"]
  df$k_vb[3] <- param["k_vb.3"]
  #add fourth species
  df$species[4] <- "Northern Pike"
  df$w_inf[4] <- param["w_inf.4"]
  df$w_mat[4] <- param["w_mat.4"]
  df$beta[4] <- param["beta.4"]
  df$sigma[4] <- param["sigma.4"]
  df$R_max[4] <- param["R_max.4"]
  df$k_vb[4] <- param["k_vb.4"]
  #add fifth species
  df$species[5] <- "Trout-perch"
  df$w_inf[5] <- param["w_inf.5"]
  df$w_mat[5] <- param["w_mat.5"]
  df$beta[5] <- param["beta.5"]
  df$sigma[5] <- param["sigma.5"]
  df$R_max[5] <- param["R_max.5"]
  df$k_vb[5] <- param["k_vb.5"]
  #add sixth species
  df$species[6] <- "Walleye"
  df$w_inf[6] <- param["w_inf.6"]
  df$w_mat[6] <- param["w_mat.6"]
  df$beta[6] <- param["beta.6"]
  df$sigma[6] <- param["sigma.6"]
  df$R_max[6] <- param["R_max.6"]
  df$k_vb[6] <- param["k_vb.6"]
  #add seventh species
  df$species[7] <- "Yellow Perch"
  df$w_inf[7] <- param["w_inf.7"]
  df$w_mat[7] <- param["w_mat.7"]
  df$beta[7] <- param["beta.7"]
  df$sigma[7] <- param["sigma.7"]
  df$R_max[7] <- param["R_max.7"]
  df$k_vb[7] <- param["k_vb.7"]
  
  #run mizer and get output
  params <- MizerParams(species_params = df, interaction = inter, kappa = 10, w_pp_cutoff =2)
  sim <- project(params, effort = 0, t_max=500, dt=0.1, t_save=1)
  output <- getCommunitySlope2(sim)[500,,]$mse
  #plot(sim)
  return(output)
  
}

mizer.simule <- function(X){
  Y = apply(X,1,function(param) mizer.linearity(param))
  return(as.matrix(Y))
}

ParaNames <- c("w_inf.1", "w_mat.1", "beta.1", "sigma.1", "R_max.1", "k_vb.1", 
               "w_inf.2", "w_mat.2", "beta.2", "sigma.2", "R_max.2", "k_vb.2",
               "w_inf.3", "w_mat.3", "beta.3", "sigma.3", "R_max.3", "k_vb.3",
               "w_inf.4", "w_mat.4", "beta.4", "sigma.4", "R_max.4", "k_vb.4",
               "w_inf.5", "w_mat.5", "beta.5", "sigma.5", "R_max.5", "k_vb.5",
               "w_inf.6", "w_mat.6", "beta.6", "sigma.6", "R_max.6", "k_vb.6",
               "w_inf.7", "w_mat.7", "beta.7", "sigma.7", "R_max.7", "k_vb.7")
morris_output2 <- morris(model = mizer.simule, factors = ParaNames, r = 40,
                        design = list(type = "oat", levels = 10, grid.jump = 5),
                        scale = T,
                        binf = as.vector(table["binf",]), bsup = as.vector(table["bsup",]))


print(morris_output2)
plot(morris_output2)

######################################################################################################

#Case 3: Sensitivity of parameters on total biomass

mizer.biomass <- function(param){
  #create emptty dataframe with necessary headers
  df <- data.frame(matrix(ncol = 7, nrow=7))
  x <- c("species", "w_inf", "w_mat", "beta", "sigma", "R_max","k_vb")
  colnames(df) <- x
  #df <- data.frame(species = NA, w_mat = NA, w_inf = NA, beta = NA, sigma = NA, R_max = NA, k_vb = NA)
  #add first species
  df$species[1] <- "Common Shiner"
  df$w_inf[1] <- param["w_inf.1"]
  df$w_mat[1] <- param["w_mat.1"]
  df$beta[1] <- param["beta.1"]
  df$sigma[1] <- param["sigma.1"]
  df$R_max[1] <- param["R_max.1"]
  df$k_vb[1] <- param["k_vb.1"]
  #add second species
  df$species[2] <- "Cisco"
  df$w_inf[2] <- param["w_inf.2"]
  df$w_mat[2] <- param["w_mat.2"]
  df$beta[2] <- param["beta.2"]
  df$sigma[2] <- param["sigma.2"]
  df$R_max[2] <- param["R_max.2"]
  df$k_vb[2] <- param["k_vb.2"]
  #add third species
  df$species[3] <- "Freshwater Drum"
  df$w_inf[3] <- param["w_inf.3"]
  df$w_mat[3] <- param["w_mat.3"]
  df$beta[3] <- param["beta.3"]
  df$sigma[3] <- param["sigma.3"]
  df$R_max[3] <- param["R_max.3"]
  df$k_vb[3] <- param["k_vb.3"]
  #add fourth species
  df$species[4] <- "Northern Pike"
  df$w_inf[4] <- param["w_inf.4"]
  df$w_mat[4] <- param["w_mat.4"]
  df$beta[4] <- param["beta.4"]
  df$sigma[4] <- param["sigma.4"]
  df$R_max[4] <- param["R_max.4"]
  df$k_vb[4] <- param["k_vb.4"]
  #add fifth species
  df$species[5] <- "Trout-perch"
  df$w_inf[5] <- param["w_inf.5"]
  df$w_mat[5] <- param["w_mat.5"]
  df$beta[5] <- param["beta.5"]
  df$sigma[5] <- param["sigma.5"]
  df$R_max[5] <- param["R_max.5"]
  df$k_vb[5] <- param["k_vb.5"]
  #add sixth species
  df$species[6] <- "Walleye"
  df$w_inf[6] <- param["w_inf.6"]
  df$w_mat[6] <- param["w_mat.6"]
  df$beta[6] <- param["beta.6"]
  df$sigma[6] <- param["sigma.6"]
  df$R_max[6] <- param["R_max.6"]
  df$k_vb[6] <- param["k_vb.6"]
  #add seventh species
  df$species[7] <- "Yellow Perch"
  df$w_inf[7] <- param["w_inf.7"]
  df$w_mat[7] <- param["w_mat.7"]
  df$beta[7] <- param["beta.7"]
  df$sigma[7] <- param["sigma.7"]
  df$R_max[7] <- param["R_max.7"]
  df$k_vb[7] <- param["k_vb.7"]
  
  #run mizer and get output
  params <- MizerParams(species_params = df, interaction = inter, kappa = 10, w_pp_cutoff = 2)
  sim <- project(params, effort = 0, t_max=500, dt=0.1, t_save=1)
  biomass <- as.vector(getBiomass(sim)[500,])
  output <- sum(biomass)
  #plot(sim)
  return(output)
  
  
}

mizer.simule <- function(X){
  Y = apply(X,1,function(param) mizer.biomass(param))
  return(as.matrix(Y))
}

ParaNames <- c("w_inf.1", "w_mat.1", "beta.1", "sigma.1", "R_max.1", "k_vb.1", 
               "w_inf.2", "w_mat.2", "beta.2", "sigma.2", "R_max.2", "k_vb.2",
               "w_inf.3", "w_mat.3", "beta.3", "sigma.3", "R_max.3", "k_vb.3",
               "w_inf.4", "w_mat.4", "beta.4", "sigma.4", "R_max.4", "k_vb.4",
               "w_inf.5", "w_mat.5", "beta.5", "sigma.5", "R_max.5", "k_vb.5",
               "w_inf.6", "w_mat.6", "beta.6", "sigma.6", "R_max.6", "k_vb.6",
               "w_inf.7", "w_mat.7", "beta.7", "sigma.7", "R_max.7", "k_vb.7")
morris_output3 <- morris(model = mizer.simule, factors = ParaNames, r = 40,
                        design = list(type = "oat", levels = 10, grid.jump = 5),
                        scale = T,
                        binf = as.vector(table["binf",]), bsup = as.vector(table["bsup",]))


print(morris_output3)
plot(morris_output3)

###########################################################################################################

#Case 4: Sensitivity of parameters on diversity index
library(vegan)

mizer.diversity <- function(param){
  #create emptty dataframe with necessary headers
  df <- data.frame(matrix(ncol = 7, nrow=7))
  x <- c("species", "w_inf", "w_mat", "beta", "sigma", "R_max","k_vb")
  colnames(df) <- x
  #df <- data.frame(species = NA, w_mat = NA, w_inf = NA, beta = NA, sigma = NA, R_max = NA, k_vb = NA)
  #add first species
  df$species[1] <- "Common Shiner"
  df$w_inf[1] <- param["w_inf.1"]
  df$w_mat[1] <- param["w_mat.1"]
  df$beta[1] <- param["beta.1"]
  df$sigma[1] <- param["sigma.1"]
  df$R_max[1] <- param["R_max.1"]
  df$k_vb[1] <- param["k_vb.1"]
  #add second species
  df$species[2] <- "Cisco"
  df$w_inf[2] <- param["w_inf.2"]
  df$w_mat[2] <- param["w_mat.2"]
  df$beta[2] <- param["beta.2"]
  df$sigma[2] <- param["sigma.2"]
  df$R_max[2] <- param["R_max.2"]
  df$k_vb[2] <- param["k_vb.2"]
  #add third species
  df$species[3] <- "Freshwater Drum"
  df$w_inf[3] <- param["w_inf.3"]
  df$w_mat[3] <- param["w_mat.3"]
  df$beta[3] <- param["beta.3"]
  df$sigma[3] <- param["sigma.3"]
  df$R_max[3] <- param["R_max.3"]
  df$k_vb[3] <- param["k_vb.3"]
  #add fourth species
  df$species[4] <- "Northern Pike"
  df$w_inf[4] <- param["w_inf.4"]
  df$w_mat[4] <- param["w_mat.4"]
  df$beta[4] <- param["beta.4"]
  df$sigma[4] <- param["sigma.4"]
  df$R_max[4] <- param["R_max.4"]
  df$k_vb[4] <- param["k_vb.4"]
  #add fifth species
  df$species[5] <- "Trout-perch"
  df$w_inf[5] <- param["w_inf.5"]
  df$w_mat[5] <- param["w_mat.5"]
  df$beta[5] <- param["beta.5"]
  df$sigma[5] <- param["sigma.5"]
  df$R_max[5] <- param["R_max.5"]
  df$k_vb[5] <- param["k_vb.5"]
  #add sixth species
  df$species[6] <- "Walleye"
  df$w_inf[6] <- param["w_inf.6"]
  df$w_mat[6] <- param["w_mat.6"]
  df$beta[6] <- param["beta.6"]
  df$sigma[6] <- param["sigma.6"]
  df$R_max[6] <- param["R_max.6"]
  df$k_vb[6] <- param["k_vb.6"]
  #add seventh species
  df$species[7] <- "Yellow Perch"
  df$w_inf[7] <- param["w_inf.6"]
  df$w_mat[7] <- param["w_mat.6"]
  df$beta[7] <- param["beta.6"]
  df$sigma[7] <- param["sigma.6"]
  df$R_max[7] <- param["R_max.6"]
  df$k_vb[7] <- param["k_vb.6"]
  
  
  #run mizer and get output
  params <- MizerParams(species_params = df, interaction = inter, kappa = 10, w_pp_cutoff = 2)
  sim <- project(params, effort = 0, t_max=500, dt=0.1, t_save=1)
  biomass <- getBiomass(sim)[500,]
  output <- diversity(biomass, index = "shannon")
  #plot(sim)
  return(output)
  
  
}

mizer.simule <- function(X){
  Y = apply(X,1,function(param) mizer.diversity(param))
  return(as.matrix(Y))
}


ParaNames <- c("w_inf.1", "w_mat.1", "beta.1", "sigma.1", "R_max.1", "k_vb.1", 
               "w_inf.2", "w_mat.2", "beta.2", "sigma.2", "R_max.2", "k_vb.2",
               "w_inf.3", "w_mat.3", "beta.3", "sigma.3", "R_max.3", "k_vb.3",
               "w_inf.4", "w_mat.4", "beta.4", "sigma.4", "R_max.4", "k_vb.4",
               "w_inf.5", "w_mat.5", "beta.5", "sigma.5", "R_max.5", "k_vb.5",
               "w_inf.6", "w_mat.6", "beta.6", "sigma.6", "R_max.6", "k_vb.6",
               "w_inf.7", "w_mat.7", "beta.7", "sigma.7", "R_max.7", "k_vb.7")
morris_output4 <- morris(model = mizer.simule, factors = ParaNames, r = 40,
                        design = list(type = "oat", levels = 10, grid.jump = 5),
                        scale = T,
                        binf = as.vector(table["binf",]), bsup = as.vector(table["bsup",]))


print(morris_output4)
plot(morris_output4)
