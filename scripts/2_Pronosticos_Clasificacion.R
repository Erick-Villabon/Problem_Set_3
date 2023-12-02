#___________________________________________________________
#
#                PREDICCIONES Clasificacion
#
#___________________________________________________________
#___________________________________________________________
# - Limpiar espacio de trabajo

rm(list = ls())

# - Librerias y paquetes 

library(pacman)
p_load(tidyverse, # Manipular dataframe
       tidymodels, # ML modelos
       yardstick, # Matriz de Confusión
       ggplot2,
       caret,
       rpart,
       ranger,
       tune, rsample, parsnip,
       units, randomForest, rattle, xgboost, bst, caret, keras, discrim, plyr, dplyr,
       openxlsx, adabag) 

# - Revisar el espacio de trabajo
setwd("/Users/juandiego/Desktop/GitHub/Problem_Set_3/stores")
#setwd("C:/Users/Erick/Desktop/Problem_Set_3/stores")

getwd()
list.files()

# 1. Importar las bases de datos ya preparadas enteriormente
test <- read.xlsx("test_2.xlsx")

train <- read.xlsx("train_2.xlsx")

submission_template <- read.csv("sample_submission.csv")



##Preparar las bases 

train$Ingtot <- with(train, ifelse(is.na(Ingtot),Ingtotug,Ingtot))


##Defino
pobre<-train$Pobre 
pobre_1<-test$Pobre 

#test<-test %>% mutate(Pobre=factor(Pobre,levels=c(0,1),labels=c("No","Si")))
#train<-train %>% mutate(Pobre=factor(Pobre,levels=c(0,1),labels=c("No","Si")))


db <- rbind(test, train)


rec_1 <- recipe(Pobre ~ edad + edad_2 + mujer + estudiante + 
                  primaria + secundaria  + media + 
                  superior + exp_trab_actual + cuartosxpersonas + 
                  num_menores + ciudad + amortizacion + arriendo1 + casapropia +
                  casahipoteca + casausufructo + casasintitulo + casaarriendo + Des +
                  Ina, data = db) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) 




ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)

#LOGIT
##########

set.seed(123)
mylogit_caret <- train(rec_1, 
                       data = train, 
                       method = "glm",
                       trControl = ctrl,
                       family = "binomial")

mylogit_caret

logit_pred <- predict(mylogit_caret, newdata = test) %>% 
  bind_cols(test) 

logit_pred<- logit_pred %>%
  select(id, ...1)

logit_pred$pobre<-0
logit_pred$pobre <- ifelse(logit_pred$...1 > 0.5, 1, 0)

logit_pred<- logit_pred %>%
  select(id, pobre)

write.table(logit_pred, file = "Logit_class_1.csv", sep = ",", row.names = FALSE, col.names = TRUE)



#Arbol
###############
set.seed(123)
class_arboles <- train(rec_1, 
                       data = train, 
                       method = "rpart",
                       trControl = ctrl,
                       tuneLength = 100)


class_arboles

arbol_pred <- predict(class_arboles, newdata = test) %>% 
  bind_cols(test) 

arbol_pred<- arbol_pred %>%
  select(id, ...1)

arbol_pred$pobre<-0
arbol_pred$pobre <- ifelse(arbol_pred$...1 > 0.5, 1, 0)

arbol_pred<- arbol_pred %>%
  select(id, pobre)

write.table(arbol_pred, file = "Arbol_class_1.csv", sep = ",", row.names = FALSE, col.names = TRUE)


####BOSQUES
###############

set.seed(123)

class_bosques_1 <- train(
  rec_1, 
  data=train,
  method = "ranger",
  trControl = ctrl,
  tuneGrid=expand.grid(
    mtry = c(1,2,3,4,5,6,7),
    splitrule = "gini",
    min.node.size = c(5,10,15,30))
)



##Adaboost


set.seed(123)

class_adaboost <- train(
  rec_1, 
  data=train,
  method = "AdaBoost.M1",
  trControl = ctrl,
  tuneGrid=expand.grid(
    mfinal = c(300,400,500),
    maxdepth = c(1,2,3),
    coeflearn = c('Breiman','Freund'))
)
