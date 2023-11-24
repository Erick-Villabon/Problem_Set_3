
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
                                "vivienda_ocupada","exp_trab_actual",
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
            exp_trab_actual = mean(exp_trab_actual),
            cuartosxpersonas = mean(cuartosxpersonas),
            num_menores = sum(num_menores),
            ciudad = first(ciudad))
