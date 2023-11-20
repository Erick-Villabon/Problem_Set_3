#_____________________________________________________________________________#
#                                                                             #
#                      Problem Set 3: Predicting Poverty                      #
#                                                                             #
#_____________________________________________________________________________#

#   Autores: - Erick Villabon                                                  
#            - Juan Diego Duarte
#
#  Fecha: 04/12/2023 


#___________________________________________________________
#
#                LIMPIEZA DE LAS BASES
#
#___________________________________________________________

# - Limpiar espacio de trabajo

rm(list = ls())

# - Librerias y paquetes

library(pacman)
p_load(rvest, tidyverse, ggplot2, robotstxt, psych, stargazer, boot, plotly, openxlsx, glmnet,
       rio, leaflet, rgeos, tmaptools, sf, osmdata, tidymodels, writexl, 
       units, randomForest, rattle, spatialsample)

# - Actualizar espacio de trabajo 
setwd("/Users/juandiego/Desktop/GitHub/Problem_Set_3/stores")
#setwd("C:/Users/Erick/Desktop/Problem_Set_3/stores")

getwd()
list.files()


# 1. Importar los Datos

unzip("train_personas.csv.zip")
train_p <- read.csv("train_personas.csv") 
train_h <- read.csv("train_hogares.csv")

test_p <- read.csv("test_personas.csv")
test_h <- read.csv("test_hogares.csv")

sample <- read.csv("sample_submission.csv") 

#no guardar en github el que dice train_personas.csv
####################################################

#2. Limpieza de la base

train <- left_join(train_p,train_h)
test <- left_join(test_p,test_h)

test$Pobre <- NA
test$Ingtot <- NA
test$Ingtotug <- NA

  #2.1 Limpieza de train

objetos <- c("train", "test")

for (obj in objetos) {
  
  data <- get(obj)
  
  # - 3.1 Train
  
  # - Edad
  
  data <- rename(data, c("edad" = "P6040"))
  data$edad_2 <- data$edad^2
  
  # - Género
  
  data$mujer <- ifelse(data$P6020 == 2, 1, 0)
  data$mujer[data$P6020 == 1] <- 0
  
  # - Estudia
  
  data$estudiante <- ifelse(data$P6240 == 3, 1, 0)
  data$estudiante[data$P6240 != 3] <- 0
  data$estudiante[data$P6240 == "."] <- 0
  data$estudiante[is.na(data$estudiante)] <- 0
  
  # - Primaria
  
  data$primaria <- ifelse(data$P6210 == 1, 1, 0)
  data$primaria[data$P6210 == "."] <- 0
  data$primaria[is.na(data$primaria)] <- 0
  
  # - Secundaria
  
  data$secundaria <- ifelse(data$P6210 == 4, 1, 0)
  data$secundaria[data$P6210 == "."] <- 0
  data$secundaria[is.na(data$secundaria)] <- 0
  
  # - Media
  
  data$media <- ifelse(data$P6210 == 5, 1, 0)
  data$media[data$P6210 == "."] <- 0
  data$media[is.na(data$media)] <- 0
  
  # - Superior
  
  data$superior <- ifelse(data$P6210 == 6, 1, 0)
  data$superior[data$P6210 == "."] <- 0
  data$superior[is.na(data$superior)] <- 0
  
  
  # - Experiencia trabajo actual
  
  data <- rename(data, c("exp_trab_actual" = "P6426"))

  # - Ciudad
  
  data <- rename(data, c("ciudad" = "Dominio"))
  
  assign(obj, data)
  rm(data)
  
}




