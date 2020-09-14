options(scipen = 999)

library(gmodels)
library(tidyverse)
library(extrafont)
datos_prestamo <- readRDS(gzcon(url("https://assets.datacamp.com/course/credit-risk-modeling-in-r/loan_data_ch1.rds")))

str(datos_prestamo)

names(datos_prestamo)[1] <- "estado_prestamo"
names(datos_prestamo)[2] <- "monto_prestado"
names(datos_prestamo)[3] <- "tasa_interes"
names(datos_prestamo)[4] <- "calificacion"
names(datos_prestamo)[5] <- "antiguedad_trabajo"
names(datos_prestamo)[6] <- "propiedad_vivienda"
names(datos_prestamo)[7] <- "ingreso_anual"
names(datos_prestamo)[8] <- "edad"

datos_prestamo %>% count(propiedad_vivienda)

levels(datos_prestamo$propiedad_vivienda)[levels(datos_prestamo$propiedad_vivienda) == "MORTGAGE"] <- "HIPOTECA"
levels(datos_prestamo$propiedad_vivienda)[levels(datos_prestamo$propiedad_vivienda) == "OTHER"] <- "OTRO"
levels(datos_prestamo$propiedad_vivienda)[levels(datos_prestamo$propiedad_vivienda) == "OWN"] <- "PROPIA"
levels(datos_prestamo$propiedad_vivienda)[levels(datos_prestamo$propiedad_vivienda) == "RENT"] <- "ALQUILER"

summary(datos_prestamo)

CrossTable(datos_prestamo$estado_prestamo)

CrossTable(datos_prestamo$calificacion, datos_prestamo$estado_prestamo,
           prop.r = T,
           prop.c = F,
           prop.t = F,
           prop.chisq = F
)


datos_prestamo %>%
    ggplot() +
    geom_point(aes(x = seq_along(ingreso_anual), y = ingreso_anual/1000, color = factor(estado_prestamo), alpha = 0.2)) +
    labs(x = element_blank(), 
         y = "Ingreso Anual (En miles USD)") +
    facet_wrap(~ calificacion, nrow = 2, scales = "free_y") +
    theme(legend.position = "none",
          text = element_text(size = 11, family = "Lato"))

datos_prestamo %>%
    ggplot() +
    geom_point(aes(x = seq_along(edad)/1000, y = edad, color = factor(edad))) +
    labs(x = "Conteo (En miles)", 
         y = "Edad") +
    facet_wrap(~ calificacion, nrow = 2, scales = "free") +
    theme(legend.position = "none",
          text = element_text(size = 11, family = "Lato"))

datos_prestamo_sinatp %>% 
    ggplot() +
    geom_histogram(aes(monto_prestado, fill = calificacion), alpha = 0.7) +
    labs(y = "Conteo", x = "Monto Prestado", fill = "Calificación")

datos_prestamo_sinatp %>% 
    ggplot() +
    geom_histogram(aes(tasa_interes, fill = propiedad_vivienda),
                   binwidth = 0.3,
                   alpha = 0.8) +
    labs(x = "Tasa de Interés", y = "Conteo")

datos_prestamo_sinatp %>% 
    ggplot() +
    geom_boxplot(aes(x = seq_along(calificacion), y = calificacion, fill = propiedad_vivienda)) +
    labs(x = NULL, y = "Calificación", fill = "Propiedad de la Vivienda")

datos_prestamo_sinatp %>% 
    filter(antiguedad_trabajo < 30) %>% 
    ggplot() +
    geom_histogram(aes(antiguedad_trabajo, fill = calificacion)) +
    labs(x = "Antiguedad del Trabajo", y = "Conteo", fill = "Calificación")

datos_prestamo_sinatp %>% 
    ggplot() +
    geom_boxplot(aes(x = seq_along(propiedad_vivienda), y = propiedad_vivienda, fill = propiedad_vivienda)) +
    labs(x = NULL, y = "Propiedad de la Vivienda") +
    theme(legend.position = "none")

datos_prestamo_sinatp %>% 
    filter(ingreso_anual <= 200000) %>% 
    ggplot() +
    geom_histogram(aes(ingreso_anual, fill = calificacion)) +
    labs(x = "Ingreso Anual", y = "Conteo", fill = "Calificación")

datos_prestamo_sinatp %>% 
    filter(edad < 50) %>% 
    ggplot() +
    geom_histogram(aes(edad, fill = calificacion)) +
    labs(x = "Edad", y = "Conteo", fill = "Calificación")

summary(datos_prestamo_sinatp)



datos_prestamo_sinatp_conNA <- datos_prestamo_sinatp # copia de la tabla de datos
datos_prestamo_sinatp_conNA$antiguedad_trabajo_cat <- rep(NA, length(datos_prestamo_sinatp_conNA$antiguedad_trabajo))
datos_prestamo_sinatp_conNA$ti_cat <- rep(NA, length(datos_prestamo_sinatp_conNA$tasa_interes))
datos_prestamo_sinatp_conNA$antiguedad_trabajo_cat[which(datos_prestamo_sinatp_conNA$antiguedad_trabajo<=1)] <- "0-1"
datos_prestamo_sinatp_conNA$antiguedad_trabajo_cat[which(datos_prestamo_sinatp_conNA$antiguedad_trabajo>1 & datos_prestamo_sinatp_conNA$antiguedad_trabajo<=3)] <- "1-3"
datos_prestamo_sinatp_conNA$antiguedad_trabajo_cat[which(datos_prestamo_sinatp_conNA$antiguedad_trabajo>3 & datos_prestamo_sinatp_conNA$antiguedad_trabajo<=5)] <- "3-5"
datos_prestamo_sinatp_conNA$antiguedad_trabajo_cat[which(datos_prestamo_sinatp_conNA$antiguedad_trabajo>5 & datos_prestamo_sinatp_conNA$antiguedad_trabajo<=10)] <- "5-10"
datos_prestamo_sinatp_conNA$antiguedad_trabajo_cat[which(datos_prestamo_sinatp_conNA$antiguedad_trabajo>10)] <- "10+"
datos_prestamo_sinatp_conNA$antiguedad_trabajo_cat[which(is.na(datos_prestamo_sinatp_conNA$antiguedad_trabajo))] <- "Faltantes"
datos_prestamo_sinatp_conNA$ti_cat[which(datos_prestamo_sinatp_conNA$tasa_interes<=8)] <- "0-8"
datos_prestamo_sinatp_conNA$ti_cat[which(datos_prestamo_sinatp_conNA$tasa_interes>8 & datos_prestamo_sinatp_conNA$tasa_interes<=11)] <- "8-11"
datos_prestamo_sinatp_conNA$ti_cat[which(datos_prestamo_sinatp_conNA$tasa_interes>11 & datos_prestamo_sinatp_conNA$tasa_interes<=13.5)] <- "11-13.5"
datos_prestamo_sinatp_conNA$ti_cat[which(datos_prestamo_sinatp_conNA$tasa_interes>13.5)] <- "13.5+"
datos_prestamo_sinatp_conNA$ti_cat[which(is.na(datos_prestamo_sinatp_conNA$tasa_interes))] <- "Faltantes"
datos_prestamo_sinatp_conNA$antiguedad_trabajo_cat <- as.factor(datos_prestamo_sinatp_conNA$antiguedad_trabajo_cat)
datos_prestamo_sinatp_conNA$ti_cat <- as.factor(datos_prestamo_sinatp_conNA$ti_cat)



datos_prestamo_proc <- datos_prestamo_sinatp_conNA
datos_prestamo_proc$antiguedad_trabajo <- NULL
datos_prestamo_proc$tasa_interes <- NULL
datos_prestamo_proc$estado_prestamo <- as.factor(datos_prestamo_proc$estado_prestamo)
set.seed(123)
train_idx <- sample(1:nrow(datos_prestamo_proc),2/3*nrow(datos_prestamo_proc))
datos_prestamo_train <- datos_prestamo_proc[train_idx, ]
datos_prestamo_test <- datos_prestamo_proc[-train_idx, ]

str(datos_prestamo_train)

modelo_log <- glm(estado_prestamo ~ edad, family = "binomial", data = datos_prestamo_train)
modelo_log

modelo_log_cat <- glm(estado_prestamo ~ ti_cat, family = "binomial", data = datos_prestamo_train)
modelo_log_cat

modelo_log_multi <- glm(estado_prestamo ~ edad + ti_cat + calificacion + monto_prestado + ingreso_anual, data = datos_prestamo_train, family = "binomial")
summary(modelo_log_multi)

modelo_log_peque <- glm(estado_prestamo ~ edad * ti_cat, family = "binomial", data = datos_prestamo_train)
pred_peque <- predict(modelo_log_peque, datos_prestamo_test, type = "response")

modelo_log_completo <- glm(estado_prestamo ~ ., family = "binomial", data = datos_prestamo_train)
pred_completa <- predict(modelo_log_completo, datos_prestamo_test, type = "response")
range(pred_completa)

pred_umbral_05 <- ifelse(pred_completa < 0.5, 0, 1)
umbral_05 <- table(datos_prestamo_test$estado_prestamo,pred_umbral_05)
umbral_05

pred_umbral_015 <- ifelse(pred_completa < 0.15, 0, 1)
umbral_015 <- table(datos_prestamo_test$estado_prestamo,pred_umbral_015)
umbral_015