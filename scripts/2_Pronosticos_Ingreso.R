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

rec_1 <- recipe(price ~ surface_total + bathrooms + bedrooms + property_type + 
                  distancia_universidades + distancia_bus  + distancia_policia + 
                  distancia_concesionarios + distancia_parque + estrato, data = db)%>% 
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
  select(property_id, .pred)

colnames(y1)[".pred"] <- "price"

y1$price <- y1$.pred

y1 <- y1[, -which(names(y1) == ".pred")]

write.table(y1, file = "ML_1.csv", sep = ",", row.names = FALSE, col.names = TRUE)


