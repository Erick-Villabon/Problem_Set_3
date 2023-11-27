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


train$Ingtot <- with(train, ifelse(is.na(Ingtot),Ingtotug,Ingtot))
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




##________________________________________________________________________
#
#                      Regularización de Modelos Lineales
#
##________________________________________________________________________


# encontrar el lambda optimo
#*****
#*****
#*****

train_fold <- vfold_cv(train, v = 30)

# Primera receta

rec_1 <- recipe(Ingtot ~ edad + edad_2 + mujer + estudiante + 
                  primaria + secundaria  + media + 
                  superior + exp_trab_actual + cuartosxpersonas + 
                  num_menores + ciudad + amortizacion + arriendo1 + casapropia +
                  casahipoteca + casausufructo + casasintitulo + casaarriendo + Des +
                  Ina, data = db)%>% 
  step_dummy(all_nominal_predictors()) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) 


# Ridge
ridge_spec <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

# Lasso
lasso_spec <- linear_reg(penalty = tune(), mixture = 0) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

# Elastic Net
elastic_net_spec <- linear_reg(penalty = tune(), mixture = .5) %>%
  set_mode("regression") %>%
  set_engine("glmnet")


# Workflows
workflow_1.1 <- workflow() %>%
  add_recipe(rec_1) %>%
  add_model(ridge_spec)

workflow_1.2 <- workflow() %>%
  add_recipe(rec_1) %>%
  add_model(lasso_spec)

workflow_1.3 <- workflow() %>%
  add_recipe(rec_1) %>%
  add_model(elastic_net_spec)

# Penalización

penalty_grid <- grid_regular(penalty(range = c(-4, 4)), levels = 30)
penalty_grid

tune_res1 <- tune_grid(
  workflow_1.1,         
  resamples = train_fold,  
  grid = penalty_grid,        
  metrics = metric_set(rmse)
)

tune_res2 <- tune_grid(
  workflow_1.2,         
  resamples = train_fold,  
  grid = penalty_grid,        
  metrics = metric_set(rmse)
)

tune_res3 <- tune_grid(
  workflow_1.3,         
  resamples = train_fold,  
  grid = penalty_grid,        
  metrics = metric_set(rmse)
)

# Seleccionar el mejor valor de penalización
best_penalty1 <- select_best(tune_res1, metric = "rmse")
best_penalty2 <- select_best(tune_res2, metric = "rmse")
best_penalty3 <- select_best(tune_res3, metric = "rmse")

modelo_1.1 <- finalize_workflow(workflow_1.1, best_penalty1)
modelo_1.2 <- finalize_workflow(workflow_1.2, best_penalty2)
modelo_1.3 <- finalize_workflow(workflow_1.3, best_penalty3)

# Entrenamos el primer modelo con los datos de train 

fit_1.1 <- fit(modelo_1.1, data = train)
fit_1.2 <- fit(modelo_1.2, data = train)
fit_1.3 <- fit(modelo_1.3, data = train)

# Evaluacion del modelo

augment(fit_1.1, new_data = train) %>%
  mae(truth = Ingtot, estimate = .pred)

augment(fit_1.2, new_data = train) %>%
  mae(truth = Ingtot, estimate = .pred)

augment(fit_1.3, new_data = train) %>%
  mae(truth = Ingtot, estimate = .pred)

# Sacamos las predicciones sobre los datos de test 

predictiones_1.1 <- predict(fit_1.1 , new_data = test) %>%
  bind_cols(test) 

predictiones_1.1<- predictiones_1.1 %>%
  select(id, .pred, lineapobreza, Pobre)

predictiones_1.1 <- rename(predictiones_1.1, c("ingreso" = ".pred"))
predictiones_1.1 <- rename(predictiones_1.1, c("pobre" = "Pobre"))

predictiones_1.1$pobre <- ifelse(predictiones_1.1$ingreso > predictiones_1.1$lineapobreza, 0, 1)

predictiones_1.1<- predictiones_1.1 %>%
  select(id, pobre)

write.table(predictiones_1.1, file = "Ridge_1.csv", sep = ",", row.names = FALSE, col.names = TRUE)



predictiones_1.2 <- predict(fit_1.2 , new_data = test)%>%
  bind_cols(test) 

predictiones_1.2<- predictiones_1.2 %>%
  select(id, .pred, lineapobreza, Pobre)

predictiones_1.2 <- rename(predictiones_1.2, c("ingreso" = ".pred"))
predictiones_1.2 <- rename(predictiones_1.2, c("pobre" = "Pobre"))

predictiones_1.2$pobre <- ifelse(predictiones_1.2$ingreso > predictiones_1.2$lineapobreza, 0, 1)

predictiones_1.2<- predictiones_1.2 %>%
  select(id, pobre)

write.table(predictiones_1.2, file = "Lasso_1.csv", sep = ",", row.names = FALSE, col.names = TRUE)



predictiones_1.3 <- predict(fit_1.3, new_data = test) %>%
  bind_cols(test) 

predictiones_1.3<- predictiones_1.3 %>%
  select(id, .pred, lineapobreza, Pobre)

predictiones_1.3 <- rename(predictiones_1.3, c("ingreso" = ".pred"))
predictiones_1.3 <- rename(predictiones_1.3, c("pobre" = "Pobre"))

predictiones_1.3$pobre <- ifelse(predictiones_1.3$ingreso > predictiones_1.3$lineapobreza, 0, 1)

predictiones_1.3<- predictiones_1.3 %>%
  select(id, pobre)

write.table(predictiones_1.3, file = "Elastic_Net_1.csv", sep = ",", row.names = FALSE, col.names = TRUE)




##________________________________________________________________________
#
#                                 Arboles
#
##________________________________________________________________________




# Tune grid aleatorio para el modelo de árboles
tune_grid_tree <- grid_random(
  tree_depth(range = c(1, 10)),
  min_n(range = c(1, 20)),
  size = 5
)

## Modelo de arboles
tree_spec <- decision_tree(
  tree_depth = tune(),
  min_n = tune()
) %>%
  set_mode("regression")

# Tune grid aleatorio para el modelo de rf
rf_grid_random <- grid_random(  mtry(range = c(2, 4)),
                                min_n(range = c(1, 10)),
                                trees(range = c(100, 300)), size = 4)
# Agregar modelos basados en árboles
# Random Forest

# Modelo de rf
rf_spec<- rand_forest(
  mtry = tune(),              
  min_n = tune(),             
  trees = tune(),
) %>%
  set_engine("randomForest") %>%
  set_mode("regression")       

# Tune grid aleatorio para el modelo de boost
tune_grid_boost <- grid_random(
  trees(range = c(400, 600)),
  min_n(range = c(1, 3)),
  learn_rate(range = c(0.001, 0.01)), size = 4
)

# Especificación del modelo boost_tree en tidymodels
boost_spec <- boost_tree(
  trees = tune(),
  min_n = tune(),
  learn_rate = tune()
) %>%
  set_mode("regression")  

# Primera receta
rec_1 <- recipe(Ingtot ~ edad + edad_2 + mujer + estudiante + 
                  primaria + secundaria  + media + 
                  superior + exp_trab_actual + cuartosxpersonas + 
                  num_menores + ciudad + amortizacion + arriendo1 + casapropia +
                  casahipoteca + casausufructo + casasintitulo + casaarriendo + Des +
                  Ina, data = db)%>% 
  step_dummy(all_nominal_predictors())


#rec_1 <- recipe(price ~ surface_total + bathrooms + bedrooms + property_type + area_universidades + 
#                  area_comercial + area_parques + distancia_bus  +
 #                 distancia_bus + distancia_policia + estrato , data = db) %>%
  #step_interact(terms = ~ estrato:bedrooms+bathrooms:property_type) %>% 
  #step_novel(all_nominal_predictors()) %>% 
  #step_dummy(all_nominal_predictors()) %>% 
  #step_zv(all_predictors()) %>% 
  #step_normalize(all_predictors())


## para el caso de los arboles incorpora no linealidades.

workflow_1.1 <- workflow() %>%
  add_recipe(rec_1) %>%
  add_model(tree_spec)

workflow_1.2 <- workflow() %>%
  add_recipe(rec_1) %>%
  add_model(rf_spec)

workflow_1.3 <- workflow() %>%
  add_recipe(rec_1) %>%
  add_model(boost_spec)  


tune_tree <- tune_grid(
  workflow_1.1, 
  grid = tune_grid_tree,
  metrics = metric_set(mae)
)


tune_rf <- tune_grid(
  workflow_1.2, 
  grid = rf_grid_random,
  metrics = metric_set(mae)
)



tune_boost <- tune_grid(
  workflow_1.3, 
  grid = tune_grid_boost,
  metrics = metric_set(mae)
)

# Utilizar 'select_best' para seleccionar el mejor valor.
best_parms_tree <- select_best(tune_tree, metric = "mae")
best_parms_tree

# Utilizar 'select_best' para seleccionar el mejor valor.
best_parms_rf<- select_best(tune_rf, metric = "mae")
best_parms_rf

# Utilizar 'select_best' para seleccionar el mejor valor.
best_parms_boost <- select_best(tune_boost, metric = "mae")
best_parms_boost

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
tree_final <- finalize_workflow(workflow_1.1, best_parms_tree)

# Ajustar el modelo  utilizando los datos de entrenamiento
tree_final_fit <- fit(tree_final, data = test)


# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
rf_final <- finalize_workflow(workflow_1.2, best_parms_rf)

# Ajustar el modelo utilizando los datos de entrenamiento
rf_final_fit <- fit(rf_final, data = test)


# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
boost_final <- finalize_workflow(workflow_1.3, best_parms_boost)

# Ajustar el modelo  utilizando los datos de entrenamiento
boost_final_fit <- fit(boost_final, data = test)



augment(tree_final_fit, new_data = test) %>%
  mae(truth = price, estimate = .pred)


augment(rf_final_fit, new_data = test) %>%
  mae(truth = price, estimate = .pred)


augment(boost_final_fit, new_data = test) %>%
  mae(truth = price, estimate = .pred)


predictiones_2.1 <- predict(tree_final_fit, new_data = test)

subida <- data.frame(
  property_id = submission_template$property_id, 
  .price = predictiones_2.1
)
colnames(subida)[2]<-"price"

write.csv(subida,file='Arbol.csv', row.names=FALSE)


predictiones_2.2 <- predict(rf_final_fit, new_data = test)

subida <- data.frame(
  property_id = submission_template$property_id, 
  .price = predictiones_2.2
)
colnames(subida)[2]<-"price"

write.csv(subida,file='RF.csv', row.names=FALSE)



predictiones_2.3 <- predict(boost_final, new_data = test)

subida <- data.frame(
  property_id = submission_template$property_id, 
  .price = predictiones_2.3
)
colnames(subida)[2]<-"price"

write.csv(subida,file='Boost.csv', row.names=FALSE)





