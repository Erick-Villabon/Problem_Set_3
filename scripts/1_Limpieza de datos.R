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
#setwd("/Users/juandiego/Desktop/GitHub/Problem_Set_2/stores")
setwd("C:/Users/Erick/Desktop/Problem_Set_3/stores")

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

