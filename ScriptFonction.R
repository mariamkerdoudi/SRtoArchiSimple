#Ce script est une série de création de  fonctions permettant d'obtenir les paramètres les plus importants d'ArchiSimple.

#On commence d'abord par installer les packages nécessaire.

library(dplyr)
library(readxl)
library(ggplot2)
library(stringr)
library(pander)
library(car)
library(archiDART)
library(ggpubr)
library(lsmeans)



#Obtaining minD without specifying the variety

get_minD <- function(yourdata) {
  # Aggregation of minimum diameter values by parent and parent_name
  Dmin <- aggregate(diameter ~ variety, data = yourdata , FUN = min)
  
  # Creation of the df Dmin_clean
  Dmin_clean <- data.frame()
  
  # Outlier filtering for all data
  cleaned_data <- diameter_outliers(Dmin)
  
  # Average of the minimum values cleaned and multiplied by 10
  mean_Dmin_clean <- mean(cleaned_data$diameter * 10, na.rm = TRUE)
  
  return(mean_Dmin_clean)
}

#MaxD

get_maxD <- function(yourdata) {
  # Retain only the nodal roots + filter outliers
  nodal_clean <- yourdata %>%
    filter(root_ontology == "Root") %>%
    diameter_outliers()
  
  # Display cleaned data
  print(nodal_clean)
  
  # Maximum diameter value multiplied by 10
  Dmax <- max(nodal_clean$diameter) * 10
  return(Dmax)
}


#IBD

get_IBDmean <- function(pumpkin.data ) {
  # IBD column is added to pumpkin.data 
  pumpkin.data  <- pumpkin.data  %>%
    mutate(IBD = ifelse(child_density != 0, 1 / child_density, NA))
  
  # Data is filtered to include only numeric IBD values
  pumpkin.data  <- pumpkin.data  %>%
    filter(!is.na(IBD) & is.numeric(IBD))
  
  # Calculate the mean IBD for the entire dataset
  IBD_mean_all <- pumpkin.data  %>%
    summarize(IBD_mean = mean(IBD, na.rm = TRUE))
  
  # Multiply the mean IBD by 10 to convert from cm to mm
  IBD_mean_all <- IBD_mean_all %>%
    mutate(IBD_mean = IBD_mean * 10)
  
  return(IBD_mean_all)
}


#RDM

get_RDM <- function(pumpkin.data ) {
  
  # Fitting a linear regression model without interacting with the variety
  mod <- lm(diameter*10~ parent_diameter, data = pumpkin.data )
  
  # Calculation of estimated average diameters
  lsm <- emmeans(mod, ~ 1)
  
  # Return the summary of estimated averages
  return(summary(lsm))
}


#CVDD
get_CVDD <- function(pumpkin.data ) {
  # Fitting a linear regression model
  mod <- lm(diameter*10 ~ parent_diameter, data = pumpkin.data )
  
  # Calculate predicted values
  predicted_values <- predict(mod)
  
  # Calculate residuals
  residuals <- residuals(mod)
  
  # Fit a linear regression model between residuals and predicted values
  residuals_mod <- lm(abs(residuals) ~ predicted_values)
  
  # Calculate the average of the absolute values of the residuals divided by the predicted values
  mean_abs_residuals <- mean(abs(residuals) / predicted_values)
  
  # Return coefficient of variation CVDD
  return(mean_abs_residuals)
}

# Utilisation de la fonction avec les données de pumpkin.data 

CVDD_result <- get_CVDD(pumpkin.data )
print(paste("Le coefficient de variation CVDD est :", CVDD_result))


get_EL <- function(merged_wheat_data) {
  # Fit a linear model with growth and diameter
  
  mod <- lm(growth ~ diameter, data = merged_wheat_data)
  
  
  # Obtain the model slope coefficient
  slope <- coef(mod)
  
  slope <- slope 
  # Return the slope of the relationship
  return(slope)
  
}

# Utilisation de la fonction avec les données primary
EL_result <- get_EL(merged_spelt_data)
print(paste("La pente de la relation entre la vitesse de croissance et le diamètre est :", EL_result))







