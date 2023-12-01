# Cargar pacman (contiene la función p_load)
library(pacman) 
# Cargar las librerías listadas e instalarlas en caso de ser necesario
p_load(tidyverse, # Manipular dataframe
       tidymodels, # ML modelos
       yardstick, # Matriz de Confusión
       ggplot2) # Graficas


#Dividir los datos en train y test
set.seed(123)
split <- initial_split(db, prop = .75)
train_data <- training(split)
test_data  <- testing(split)

#Crear un recipe
receta <- recipe( ciudad ~ . , data = train_data) %>% 
  step_rm(in_sf) %>% # eliminamos la variable continua que tiene la misma info que la variable objetivo
  step_center(all_predictors()) %>% # Centramos todas las variables
  step_scale(all_predictors()) # Reescalamos



receta_lineal <- recipe(in_sf ~ ., data = train_data) %>%
  step_rm(ciudad) %>% # eliminamos la variable categórica que tiene la misma info que la variable objetivo
  step_center(all_predictors()) %>% # Centramos todas las variables
  step_scale(all_predictors()) # Reescalamos


linear_spec <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")


workflow_lineal <- workflow() %>%
  add_recipe(receta_lineal) %>%
  add_model(linear_spec)

modelo_lineal <- fit(workflow_lineal, data = train_data)


# Sacar predicciones
test_data <- test_data %>%
  mutate(predicciones_lineal = predict(modelo_lineal, test_data)$.pred)


#Logit 
modelo_logit <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

workflow_logit <- workflow() %>%
  add_recipe(receta) %>%
  add_model(modelo_logit)

modelo_logit <- workflow_logit %>%
  fit(data = train_data)


test_data <- test_data %>%
  mutate(predicciones_logit = predict(modelo_logit, test_data)$.pred_class)
matriz_logit <- conf_mat(test_data, truth = ciudad, estimate = predicciones_logit)
print(matriz_logit)


#Probit
probit_spec <- logistic_reg() %>% 
  set_engine("glm", family = stats::binomial(link = "probit")) %>%
  set_mode("classification") %>% 
  translate()

workflow_probit <- workflow() %>%
  add_recipe(receta) %>%
  add_model(probit_spec)

modelo_probit <- fit(workflow_probit, data = train_data)



test_data <- test_data %>%
  mutate(predicciones_probit = predict(modelo_probit, test_data)$.pred_class)
matriz_probit <- conf_mat(test_data, truth = ciudad, estimate = predicciones_probit)
print(matriz_probit)





