#___________________________________________________________
#
#                PREDICCIONES Ingreso
#
#___________________________________________________________
#___________________________________________________________

# - Librerias y paquetes 

library(pacman)
p_load(rvest, tidyverse, ggplot2, psych, boot, glmnet, tune, rsample, parsnip, tune, Metrics, tune, rsample, parsnip,
       rio, leaflet, modeldata, vtable, sf, osmdata, tidymodels, writexl, rsample, parsnip, openxlsx, e1071,
       units, randomForest, rattle, xgboost, bst, caret, keras, discrim, plyr, dplyr, ranger, rpart)

# - Revisar el espacio de trabajo
#setwd("/Users/juandiego/Desktop/GitHub/Problem_Set_3/stores")
#setwd("C:/Users/Erick/Desktop/Problem_Set_3/stores")
setwd("E:/Problem_Set_3/stores")

getwd()
list.files()

# 1. Importar las bases de datos ya preparadas enteriormente
test <- read.xlsx("test_2.xlsx")

train <- read.xlsx("train_2.xlsx")

submission_template <- read.csv("sample_submission.csv")


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

y1 <- rename(y1, c(".pred" = "ingreso"))
y1 <- rename(y1, c("Pobre" = "pobre"))

y1$pobre <- ifelse(y1$ingreso > y1$lineapobreza, 0, 1)

y1<- y1 %>%
  select(id, pobre)

write.table(y1, file = "ML_1.csv", sep = ",", row.names = FALSE, col.names = TRUE)



######METRICAS
#####################
#####################
#####################

####ML
###Obtener RMSE
y1 <- predict(fit_1 , new_data = train) %>%
  bind_cols(train) 

y1<- y1 %>%
  select(id, .pred, lineapobreza, Pobre)

y1$pobre

y1 <- rename(y1, c(".pred" = "ingreso"))

y1$pobre <- ifelse(y1$ingreso > y1$lineapobreza, 0, 1)

y1<- y1 %>%
  select(id, pobre, Pobre)

sqrt(mean((y1$Pobre - y1$pobre)^2))

###Obtener R cuadrado
r_squared <- cor(y1$pobre, y1$Pobre)^2
r_squared

###Obtener MAE
mae <- Metrics::mae(as.vector(y1$Pobre), as.vector(y1$pobre))
mae

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

write.table(predictiones_1.1, file = "Ridge_2.csv", sep = ",", row.names = FALSE, col.names = TRUE)



predictiones_1.2 <- predict(fit_1.2 , new_data = test)%>%
  bind_cols(test) 

predictiones_1.2<- predictiones_1.2 %>%
  select(id, .pred, lineapobreza, Pobre)

predictiones_1.2 <- rename(predictiones_1.2, c("ingreso" = ".pred"))
predictiones_1.2 <- rename(predictiones_1.2, c("pobre" = "Pobre"))

predictiones_1.2$pobre <- ifelse(predictiones_1.2$ingreso > predictiones_1.2$lineapobreza, 0, 1)

predictiones_1.2<- predictiones_1.2 %>%
  select(id, pobre)

write.table(predictiones_1.2, file = "Lasso_2.csv", sep = ",", row.names = FALSE, col.names = TRUE)



predictiones_1.3 <- predict(fit_1.3, new_data = test) %>%
  bind_cols(test) 

predictiones_1.3<- predictiones_1.3 %>%
  select(id, .pred, lineapobreza, Pobre)

predictiones_1.3 <- rename(predictiones_1.3, c("ingreso" = ".pred"))
predictiones_1.3 <- rename(predictiones_1.3, c("pobre" = "Pobre"))

predictiones_1.3$pobre <- ifelse(predictiones_1.3$ingreso > predictiones_1.3$lineapobreza, 0, 1)

predictiones_1.3<- predictiones_1.3 %>%
  select(id, pobre)

write.table(predictiones_1.3, file = "Elastic_Net_2.csv", sep = ",", row.names = FALSE, col.names = TRUE)



######METRICAS
#####################
#####################
#####################

####LASSO
###Obtener RMSE
predictiones_1.2 <- predict(fit_1.2 , new_data = train) %>%
  bind_cols(train) 

predictiones_1.2<- predictiones_1.2 %>%
  select(id, .pred, lineapobreza, Pobre)

predictiones_1.2$pobre

predictiones_1.2 <- rename(predictiones_1.2, c("ingreso" = ".pred"))

predictiones_1.2$pobre <- ifelse(predictiones_1.2$ingreso > predictiones_1.2$lineapobreza, 0, 1)

predictiones_1.2<- predictiones_1.2 %>%
  select(id, pobre, Pobre)

sqrt(mean((predictiones_1.2$Pobre - predictiones_1.2$pobre)^2))

###Obtener R cuadrado
r_squared <- cor(predictiones_1.2$pobre, predictiones_1.2$Pobre)^2
r_squared

###Obtener MAE
mae <- mae(predictiones_1.2$Pobre, predictiones_1.2$pobre)
mae



####RIDGE
###Obtener RMSE
predictiones_1.1 <- predict(fit_1.1 , new_data = train) %>%
  bind_cols(train) 

predictiones_1.1<- predictiones_1.1 %>%
  select(id, .pred, lineapobreza, Pobre)

predictiones_1.1$pobre

predictiones_1.1 <- rename(predictiones_1.1, c("ingreso" = ".pred"))

predictiones_1.1$pobre <- ifelse(predictiones_1.1$ingreso > predictiones_1.1$lineapobreza, 0, 1)

predictiones_1.1<- predictiones_1.1 %>%
  select(id, pobre, Pobre)

sqrt(mean((predictiones_1.1$Pobre - predictiones_1.1$pobre)^2))

###Obtener R cuadrado
r_squared <- cor(predictiones_1.1$pobre, predictiones_1.1$Pobre)^2
r_squared

###Obtener MAE
mae <- mae(predictiones_1.1$Pobre, predictiones_1.1$pobre)
mae


####ELASTICNET
###Obtener RMSE
predictiones_1.3 <- predict(fit_1.3 , new_data = train) %>%
  bind_cols(train) 

predictiones_1.3<- predictiones_1.3 %>%
  select(id, .pred, lineapobreza, Pobre)

predictiones_1.3$pobre

predictiones_1.3 <- rename(predictiones_1.3, c("ingreso" = ".pred"))

predictiones_1.3$pobre <- ifelse(predictiones_1.3$ingreso > predictiones_1.3$lineapobreza, 0, 1)

predictiones_1.3<- predictiones_1.3 %>%
  select(id, pobre, Pobre)

sqrt(mean((predictiones_1.3$Pobre - predictiones_1.3$pobre)^2))

###Obtener R cuadrado
r_squared <- cor(predictiones_1.3$pobre, predictiones_1.3$Pobre)^2
r_squared

###Obtener MAE
mae <- mae(predictiones_1.3$Pobre, predictiones_1.3$pobre)
mae


##________________________________________________________________________
#
#                                 Arboles
#
##________________________________________________________________________
set.seed(123)

fitControl<-trainControl(method ="cv",
                         number=5)

rec_1 <- recipe(Ingtot ~ edad + edad_2 + mujer + estudiante + 
                  primaria + secundaria  + media + 
                  superior + exp_trab_actual + cuartosxpersonas + 
                  num_menores + ciudad + amortizacion + arriendo1 + casapropia +
                  casahipoteca + casausufructo + casasintitulo + casaarriendo + Des +
                  Ina, data = db) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) 


##BOOST
tree_boosted <- train(
  rec_1,
  data=train,
  method = "bstTree",
  trControl = fitControl,
  tuneGrid=expand.grid(
    mstop = c(400,500,600), #Boosting Iterations (M)
    maxdepth = c(1,2,3), # Max Tree Depth (d)
    nu = c(0.01,0.001)) # Shrinkage (lambda)
)

tree_boosted

# Ajustar el modelo  utilizando los datos de entrenamiento
boost_final_pred <- predict(tree_boosted, newdata = test) %>%
  bind_cols(test) 

boost_final_pred<- boost_final_pred %>%
  select(id, ...1, lineapobreza, Pobre)

boost_final_pred <- rename(boost_final_pred, c("Pobre" = "pobre"))
boost_final_pred <- rename(boost_final_pred, c("...1" = "ingreso"))

boost_final_pred$pobre <- ifelse(boost_final_pred$ingreso > boost_final_pred$lineapobreza, 0, 1)

boost_final_pred<- boost_final_pred %>%
  select(id, pobre)

write.table(boost_final_pred, file = "Boost_2.csv", sep = ",", row.names = FALSE, col.names = TRUE)



##Random Forest

tree_ranger <- train(
  rec_1,
  data=train,
  method = "ranger",
  trControl = fitControl,
  tuneGrid=expand.grid(
    mtry = c(1,2,3),
    splitrule = "variance",
    min.node.size = c(5,10,15))
)

# Ajustar el modelo  utilizando los datos de entrenamiento
ranger_final_pred <- predict(tree_ranger, newdata = test) %>%
  bind_cols(test) 

ranger_final_pred<- ranger_final_pred %>%
  select(id, ...1, lineapobreza, Pobre)

ranger_final_pred <- rename(ranger_final_pred, c("Pobre" = "pobre"))
ranger_final_pred <- rename(ranger_final_pred, c("...1" = "ingreso"))

ranger_final_pred$pobre <- ifelse(ranger_final_pred$ingreso > ranger_final_pred$lineapobreza, 0, 1)

ranger_final_pred<- ranger_final_pred %>%
  select(id, pobre)

write.table(ranger_final_pred, file = "Ranger_1.csv", sep = ",", row.names = FALSE, col.names = TRUE)



######METRICAS
#####################
#####################
#####################

####RANDOMFOREST
###Obtener RMSE
ranger_final_pred <- predict(tree_ranger , newdata = train) %>%
  bind_cols(train) 

ranger_final_pred<- ranger_final_pred %>%
  select(id, ...1, lineapobreza, Pobre)

ranger_final_pred$pobre

ranger_final_pred <- rename(ranger_final_pred, c("...1" = "ingreso"))

ranger_final_pred$pobre <- ifelse(ranger_final_pred$ingreso > ranger_final_pred$lineapobreza, 0, 1)

ranger_final_pred<- ranger_final_pred %>%
  select(id, pobre, Pobre)

sqrt(mean((ranger_final_pred$Pobre - ranger_final_pred$pobre)^2))

###Obtener R cuadrado
r_squared <- cor(ranger_final_pred$pobre, ranger_final_pred$Pobre)^2
r_squared

###Obtener MAE
mae <- Metrics::mae(ranger_final_pred$Pobre, ranger_final_pred$pobre)
mae

