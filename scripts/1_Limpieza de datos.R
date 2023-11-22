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

#unzip("train_personas.csv.zip")
train_p <- read.csv("train_personas.csv") 
train_h <- read.csv("train_hogares.csv")

test_p <- read.csv("test_personas.csv")
test_h <- read.csv("test_hogares.csv")

sample <- read.csv("sample_submission.csv") 

##3. limpieza hogares TEST 

test_h$P5100 <- with(test_h, ifelse(is.na(P5100),0,P5100))

test_h$arriendo <- test_h$P5130
test_h$arriendo <- with(test_h, ifelse(is.na(arriendo),P5140,arriendo))

test_h$hipoteca <- 0
test_h$hipoteca <- with(test_h, ifelse(P5100!=0,1,hipoteca))

##2.1 Limpieza personas TEST

colSums(is.na(test_p))
test_p$Oc <- with(test_p, ifelse(is.na(Oc) & ((Des==1)|(Ina==1)),0,Oc))
test_p$Des <- with(test_p, ifelse(is.na(Des) & ((Oc==1)|(Ina==1)),0,Des))
test_p$Ina <- with(test_p, ifelse(is.na(Ina) & ((Oc==1)|(Des==1)),0,Ina))
colSums(is.na(test_p))

colSums(is.na(test_p))
test_p$P6090 <- with(test_p, ifelse(is.na(P6090) & ((P6100==1)|(P6100==2)|(P6100==3)),1,P6090))
test_p$P6090 <- with(test_p, ifelse(is.na(P6090) & (P6100==9),9,P6090))

test_p$P6100 <- with(test_p, ifelse(is.na(P6100) & (P6090==2),0,P6100))
test_p$P6100 <- with(test_p, ifelse(is.na(P6100) & (P6090==9),9,P6100))
colSums(is.na(test_p))

colSums(is.na(test_p))
test_p$P6210s1 <- with(test_p, ifelse(is.na(P6210s1) & (P6210==9),99,P6210s1))
test_p$P6210s1 <- with(test_p, ifelse(is.na(P6210s1) & (P6210==1),0,P6210s1))
test_p$P6210s1 <- with(test_p, ifelse(is.na(P6210s1) & (P6210==2),2,P6210s1))
test_p$P6210s1 <- with(test_p, ifelse(is.na(P6210s1) & (P6210==3),7,P6210s1))
test_p$P6210s1 <- with(test_p, ifelse(is.na(P6210s1) & (P6210==4),11,P6210s1))
test_p$P6210s1 <- with(test_p, ifelse(is.na(P6210s1) & (P6210==5),13,P6210s1))
test_p$P6210s1 <- with(test_p, ifelse(is.na(P6210s1) & (P6210==6),18,P6210s1))
colSums(is.na(test_p))



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




