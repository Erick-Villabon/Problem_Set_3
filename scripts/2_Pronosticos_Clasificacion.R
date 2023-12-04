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
       ggplot2, dplyr,
       caret,
       rpart,
       ranger,
       tune, rsample, parsnip,
       units, randomForest, rattle, xgboost, bst, caret, keras, discrim, plyr, dplyr,
       openxlsx, adabag, klaR) 

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



##Preparar las bases 

train$Ingtot <- with(train, ifelse(is.na(Ingtot),Ingtotug,Ingtot))


##Defino
pobre<-train$Pobre 
pobre_1<-test$Pobre 

test<-test %>% mutate(Pobre=factor(Pobre,levels=c(0,1),labels=c("No","Si")))
train<-train %>% mutate(Pobre=factor(Pobre,levels=c(0,1),labels=c("No","Si")))


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
    mtry = c(1,2,3,4,5),
    splitrule = "gini",
    min.node.size = c(5,10,15,30))
)

class_arboles

bosque_pred <- predict(class_bosques_1, newdata = test) %>% 
  bind_cols(test) 

bosque_pred<- bosque_pred %>%
  select(id, ...1)

bosque_pred$pobre<-0
bosque_pred$pobre <- ifelse(bosque_pred$...1 == "Si", 1, 0)

bosque_pred<- bosque_pred %>%
  select(id, pobre)

write.table(bosque_pred, file = "bosque_class_1.csv", sep = ",", row.names = FALSE, col.names = TRUE)

##Adaboost


set.seed(123)

class_adaboost <- train(
  rec_1, 
  data=train,
  method = "AdaBoost.M1",
  trControl = ctrl,
  tuneGrid=expand.grid(
    mfinal = c(50,100),
    maxdepth = c(1,2,3),
    coeflearn = c('Breiman','Freund'))
)

class_adaboost 

boost_pred <- predict(class_adaboost , newdata = test) %>% 
  bind_cols(test) 

boost_pred<- boost_pred %>%
  select(id, ...1)

boost_pred$pobre<-0
boost_pred$pobre <- ifelse(boost_pred$...1 == "Si", 1, 0)

boost_pred<- boost_pred %>%
  select(id, pobre)

write.table(boost_pred, file = "boost_class_1.csv", sep = ",", row.names = FALSE, col.names = TRUE)


##Generative models

##LDA
set.seed(123)
lda_fit = train(rec_1,
                data=train,
                method="lda",
                trControl = ctrl)

lda_fit

lda_pred <- predict(lda_fit , newdata = test) %>% 
  bind_cols(test) 

lda_pred$pobre<-0
lda_pred$pobre <- ifelse(lda_pred$...1 == "Si", 1, 0)

lda_pred<- lda_pred %>%
  select(id,pobre)

write.table(lda_pred, file = "LDA_class_1.csv", sep = ",", row.names = FALSE, col.names = TRUE)


###QDA
set.seed(123)
qda_fit = train(rec_1, 
                data=train, 
                method="qda",
                trControl = ctrl)

qda_fit






#####Naive Bayes
set.seed(123)
mylogit_nb <- train(rec_1,
                    data = train, 
                    method = "nb",
                    trControl = ctrl,
                    tuneGrid=expand.grid(fL=seq(0,10,length.out = 5),
                                         usekernel=TRUE,
                                         adjust=seq(1,10,length.out = 5)))



mylogit_nb











####KNN
set.seed(123)
mylogit_knn <- train(rec_1,
                     data = train, 
                     method = "knn",
                     trControl = ctrl,
                     tuneGrid = expand.grid(k=c(3,5,7,9,11)))


mylogit_knn

