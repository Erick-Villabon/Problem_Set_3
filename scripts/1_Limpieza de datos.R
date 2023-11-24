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
#setwd("/Users/juandiego/Desktop/GitHub/Problem_Set_3/stores")
setwd("C:/Users/Erick/Desktop/Problem_Set_3/stores")

getwd()
list.files()


# 1. Importar los Datos

#unzip("train_personas.csv.zip")
train_p <- read.csv("train_personas.csv") 
train_h <- read.csv("train_hogares.csv")

test_p <- read.csv("test_personas.csv")
test_h <- read.csv("test_hogares.csv")

sample <- read.csv("sample_submission.csv") 



# 1.1 Datos que hacen falta

test_h$Pobre <- NA
test_h$Ingtot <- NA
test_h$Ingtotug <- NA

#Indicador de base
test_h <- test_h %>%
  mutate(base_i = c(0))

test_p <- test_p %>%
  mutate(base_i = c(1))

train_h <- train_h %>%
  mutate(base_i = c(2))

train_p <- train_p %>%
  mutate(base_i = c(3))

# 1.4 unir bases
train <- left_join(train_p,train_h)
test <- left_join(test_p,test_h)



# 1.2 Renombrar variables

bases <- c("test", "train")

for (bas in bases) {
  
  data <- get(bas)


  data <- data %>%
  rename(cuartos = P5000)

  data <- data %>%
  rename(cuartosxpersonas = P5010)

  data <- data %>%
  rename(vivienda_ocupada = P5090)

  data <- data %>%
  rename(amortizacion = P5100)
  data$amortizacion <- with(data, ifelse(amortizacion!=0,1,amortizacion))
  
  data <- data %>%
  rename(arriendo1 = P5130)

  data <- data %>%
  rename(arriendo2 = P5140)
  
  data$arriendo1 <- with(data, ifelse(is.na(arriendo1),arriendo2,arriendo1))

  colSums(is.na(data))
  data$Oc <- with(data, ifelse(is.na(Oc) & ((Des==1)|(Ina==1)),0,Oc))
  data$Des <- with(data, ifelse(is.na(Des) & ((Oc==1)|(Ina==1)),0,Des))
  data$Ina <- with(data, ifelse(is.na(Ina) & ((Oc==1)|(Des==1)),0,Ina))
  colSums(is.na(data))
  
  
  
  colSums(is.na(data))
  data$P6090 <- with(data, ifelse(is.na(P6090) & ((P6100==1)|(P6100==2)|(P6100==3)),1,P6090))
  data$P6090 <- with(data, ifelse(is.na(P6090) & (P6100==9),9,P6090))
  
  data$P6100 <- with(data, ifelse(is.na(P6100) & (P6090==2),0,P6100))
  data$P6100 <- with(data, ifelse(is.na(P6100) & (P6090==9),9,P6100))
  colSums(is.na(data))
  
  colSums(is.na(data))
  data$P6210s1 <- with(data, ifelse(is.na(P6210s1) & (P6210==9),99,P6210s1))
  data$P6210s1 <- with(data, ifelse(is.na(P6210s1) & (P6210==1),0,P6210s1))
  data$P6210s1 <- with(data, ifelse(is.na(P6210s1) & (P6210==2),2,P6210s1))
  data$P6210s1 <- with(data, ifelse(is.na(P6210s1) & (P6210==3),7,P6210s1))
  data$P6210s1 <- with(data, ifelse(is.na(P6210s1) & (P6210==4),11,P6210s1))
  data$P6210s1 <- with(data, ifelse(is.na(P6210s1) & (P6210==5),13,P6210s1))
  data$P6210s1 <- with(data, ifelse(is.na(P6210s1) & (P6210==6),18,P6210s1))
  colSums(is.na(data))
  
  
  # - Edad
  
  data <- rename(data, c("edad" = "P6040"))
  data$edad_2 <- data$edad^2
  
  # - Género
  
  data$mujer <- ifelse(data$P6020 == 2, 1, 0)
  data$mujer[data$P6020 == 1] <- 0
  
  # - Estudia
  
  data$estudiante <- ifelse(data$P6240 == 3, 1, 0)
  data$estudiante[data$P6240 != 3] <- 0
  data$estudiante[data$P6240 == "."] <- 0
  data$estudiante[is.na(data$estudiante)] <- 0
  
  # - Primaria
  
  data$primaria <- ifelse(data$P6210 == 1, 1, 0)
  data$primaria[data$P6210 == "."] <- 0
  data$primaria[is.na(data$primaria)] <- 0
  
  # - Secundaria
  
  data$secundaria <- ifelse(data$P6210 == 4, 1, 0)
  data$secundaria[data$P6210 == "."] <- 0
  data$secundaria[is.na(data$secundaria)] <- 0
  
  # - Media
  
  data$media <- ifelse(data$P6210 == 5, 1, 0)
  data$media[data$P6210 == "."] <- 0
  data$media[is.na(data$media)] <- 0
  
  # - Superior
  
  data$superior <- ifelse(data$P6210 == 6, 1, 0)
  data$superior[data$P6210 == "."] <- 0
  data$superior[is.na(data$superior)] <- 0
  
  
  # - Experiencia trabajo actual
  
  data <- rename(data, c("exp_trab_actual" = "P6426"))
  
  # - Ciudad
  
  data <- rename(data, c("ciudad" = "Dominio"))
  
  # - Imputación de experiencia
  
  data$exp_trab_actual <- ifelse(data$edad < 18 & 
                                   is.na(data$exp_trab_actual), 0, 
                                 data$exp_trab_actual)
  
  data <- data %>% 
    group_by(id) %>% 
    mutate(mean_exp = mean(exp_trab_actual, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(exp_trab_actual = if_else(is.na(exp_trab_actual) & data$edad >= 18, 
                                     mean_exp, data$exp_trab_actual))
  
  data <- data %>% 
    group_by(id) %>% 
    mutate(variable = ifelse(all(is.na(exp_trab_actual)), 0, 
                             exp_trab_actual)) %>% 
    ungroup() %>% 
    mutate(exp_trab_actual = if_else(is.na(exp_trab_actual), 
                                     variable, data$exp_trab_actual))
  
  
  data <- subset(data, select = c("id", "Orden", "Clase",
                                  "ciudad", "edad", "edad_2", "mujer", 
                                  "estudiante", "primaria", "secundaria",
                                  "media", "superior", "Ingtot",
                                  "Ingtotug", "exp_trab_actual", 
                                  "Pobre", "cuartosxpersonas", 
                                  "vivienda_ocupada",
                                  "amortizacion", "arriendo1"))

  
  data$num_menores <- as.numeric(data$edad < 18)
  
  
  
  data <- data %>% group_by(id) %>%
    summarize(edad = mean(edad),
              edad_2 = mean(edad_2),
              mujer = mean(mujer),
              estudiante = mean(estudiante),
              primaria = mean(primaria),
              secundaria = mean(secundaria),
              media = mean(media),
              superior = mean(superior),
              Ingtot = sum(Ingtot),
              Ingtotug = mean(Ingtotug),
              exp_trab_actual = mean(exp_trab_actual),
              Pobre = mean(Pobre),
              cuartosxpersonas = mean(cuartosxpersonas),
              num_menores = sum(num_menores),
              ciudad = first(ciudad), 
              amortizacion = sum(amortizacion),
              arriendo1 = mean(arriendo1))
  
  assign(bas, data)
  rm(data)
  
  }
