#_____________________________________________________________________________#
#                                                                             #
#                      Problem Set 3: Making Money with ML?                   #
#                                                                             #
#_____________________________________________________________________________#

#   Autores: - Erick Villabon                                                  
#            - Juan Diego Duarte
#
#  Fecha: 30/10/2023 


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
#setwd("/Users/juandiego/Desktop/GitHub/Problem_Set_2/stores")
setwd("C:/Users/Erick/Desktop/Problem_Set_3/stores")

getwd()
list.files()


# 1. Importar los Datos

train_p <- read.csv("") 



#revisamos los NAs
sapply(test,function(x) sum(is.na(x)))



