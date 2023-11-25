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
#***** test <- read.xlsx("test_2.xlsx")

#***** train <- read.xlsx("train_2.xlsx")

#***** submission_template <- read.csv("submission_template.csv")

db <- rbind(test, train)


##________________________________________________________________________
#
#                            Modelo Líneal
#
##________________________________________________________________________

#NOTA: Amortizacion_2 es una variable sobre si la casa esta hipotecada o no
#NOTA: Pagoarriendo es una variable sobre si la casa esta en arriendo o no
#Estas dos variables son sustituidas por las variables creadas

rec_1 <- recipe(Ingtot ~ edad + edad_2 + mujer + estudiante + 
                  primaria + secundaria  + media + 
                  superior + exp_trab_actual + cuartosxpersonas + 
                  num_menores + ciudad + amortizacion + arriendo1 + casapropia +
                  casahipoteca + casausufructo + casasintitulo + casaarriendo + Des +
                  Ina, data = db)%>% 
  step_dummy(all_nominal_predictors())

reglineal<-linear_reg()


workf_1<-workflow() %>% 
  add_recipe(rec_1) %>% 
  add_model(reglineal)


fit_1 <- workf_1 %>% 
  fit(data=train) 


y1 <- predict(fit_1, new_data = test) %>% 
  bind_cols(test) 

y1<- y1 %>%
  select(id, .pred, lineapobreza, Pobre)

y1 <- rename(y1, c("ingreso" = ".pred"))
y1 <- rename(y1, c("pobre" = "Pobre"))

y1$pobre <- ifelse(y1$ingreso > y1$lineapobreza, 0, 1)

y1<- y1 %>%
  select(id, pobre)

write.table(y1, file = "ML_1.csv", sep = ",", row.names = FALSE, col.names = TRUE)



