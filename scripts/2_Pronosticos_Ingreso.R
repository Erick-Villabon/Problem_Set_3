#___________________________________________________________
#
#                PREDICCIONES Ingreso
#
#___________________________________________________________
#___________________________________________________________

# - Limpiar espacio de trabajo

rm(list = ls())

# - Librerias y paquetes 

library(pacman)
p_load(rvest, tidyverse, ggplot2, robotstxt, psych, stargazer, boot, plotly, openxlsx, glmnet,
       rio, leaflet, rgeos, modeldata, vtable, tmaptools, sf, osmdata, tidymodels, writexl, 
       units, randomForest, rattle, spatialsample, xgboost)

# - Revisar el espacio de trabajo
setwd("/Users/juandiego/Desktop/GitHub/Problem_Set_3/stores")
#setwd("C:/Users/Erick/Desktop/Problem_Set_3/stores")

getwd()
list.files()

# 1. Importar las bases de datos ya preparadas enteriormente
#test <- read.xlsx("test_2.xlsx")

#train <- read.xlsx("train_2.xlsx")

#submission_template <- read.csv("submission_template.csv")




##________________________________________________________________________
#
#                            Modelo Líneal
#
##________________________________________________________________________

#Modelo 1


