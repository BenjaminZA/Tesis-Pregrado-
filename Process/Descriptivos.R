#Descriptivos
#Autor: Benjamín Zavala
#Fecha: 20/04/2016
#-------------------------------------------------------------------------------

pacman::p_load(tidyverse, 
               dplyr, 
               haven, 
               car, 
               magrittr,
               psych,
               readxl, 
               fastDummies,
               stargazer,
               modelsummary)

#Bases de datos-----------------------------------------------------------------


proc_CIVED_1999 <- readRDS("output/data_procesada/proc_CIVED_1999.rds")

proc_ICCS_2009 <- readRDS("output/data_procesada/proc_ICCS_2009.rds")

proc_ICCS_2016 <- readRDS("output/data_procesada/proc_ICCS_2016.rds")

proc_PACES_2019 <- readRDS("output/data_procesada/proc_PACES_2019.rds")


b1<-proc_CIVED_1999
b2<-proc_ICCS_2009 
b3<-proc_ICCS_2016
b4<-proc_PACES_2019
#Descriptivos dependiente--------------------------------------------------------
b1 <- b1 %>% 
  rename(nivel_educ = Nive_educ)

# Agrupamos las bases para que el ciclo sepa qué recorrer
lista_bases <- list(b1 = b1, b2 = b2, b3 = b3, b4 = b4)

library(summarytools)

for (i in seq_along(lista_bases)) {
  # Imprimir el nombre de la base para saber cuál estamos viendo
  cat("\n--- Estadísticos Descriptivos:", names(lista_bases)[i], "---\n")
  
  # Generar el descriptivo
  # transpose = TRUE pone las variables en las filas (más ordenado)
  print(descr(lista_bases[[i]], 
              stats = c("mean", "sd", "min", "max"), 
              transpose = TRUE), 
        headings = FALSE)
}

# Tranformar en Dummys


# 2. Definir las variables a convertir
variables_dummy <- c("libro", "nivel_educ")


for (nombre_base in names(lista_bases)) {
  
  lista_bases[[nombre_base]] <- dummy_cols(
    lista_bases[[nombre_base]], 
    select_columns = variables_dummy,
    remove_selected_columns = FALSE, # Mantiene las variables originales
    remove_first_dummy = FALSE       # Cambia a TRUE si vas a usar regresiones
  )
  
}
list2env(lista_bases, envir = .GlobalEnv)

#Recodificar expectativa de voto com dummy--------------------------------------

b1 <- b1 %>% 
  mutate(
    indice_voto = (max(indice_voto, na.rm = TRUE) + min(indice_voto, na.rm = TRUE)) - indice_voto
  )

b1 <- b1 %>% 
  mutate(
    voto = if_else(indice_voto == max(indice_voto, na.rm = TRUE), 1, 0)
  )

b2 <- b2 %>% 
  mutate(
    voto = if_else(indice_voto == max(indice_voto, na.rm = TRUE), 1, 0)
  )

b3 <- b3 %>% 
  mutate(
    voto = if_else(indice_voto == max(indice_voto, na.rm = TRUE), 1, 0)
  )

b4 <- b4 %>% 
  mutate(
    voto = if_else(indice_voto == max(indice_voto, na.rm = TRUE), 1, 0)
  )

#Selección de variable para matriz de correlación-------------------------------


b1_seleccionada <- b1 %>% 
  select(id, id_colegio, voto, libro, nivel_educ,
          clima_aula_escuela)

b2_seleccionada <- b2 %>% 
  select(id, id_colegio, voto, libro, nivel_educ, 
         clima_aula_escuela)

b3_seleccionada <- b3 %>% 
  select(id, id_colegio, voto, libro, nivel_educ, 
         clima_aula_escuela)

b4_seleccionada <- b4 %>% 
  select(id, id_colegio, voto, libro, nivel_educ,
          clima_aula_escuela)

# Calculas la correlación-------------------------------------------------------
cor(b1_seleccionada, use = "complete.obs")

cor(b2_seleccionada, use = "complete.obs")

cor(b3_seleccionada, use = "complete.obs")

cor(b4_seleccionada, use = "complete.obs")

#Modelos multinivel-------------------------------------------------------------

#CIVED 1999

library(dplyr)
library(lme4)

b1_preparada <- b1_seleccionada %>% 
  mutate(
    id_colegio = as.factor(id_colegio),
    libro = as.factor(libro),
    nivel_educ = as.factor(nivel_educ)
  )


# Si 'voto' es una variable continua:
m0_vacio <- lmer(voto ~ 1 + (1 | id_colegio), data = b1_preparada)

# Si 'voto' es binaria (0 o 1):
# m0_vacio <- glmer(voto ~ 1 + (1 | id_colegio), data = b1_preparada, family = binomial)

summary(m0_vacio)

# Si 'voto' es una variable continua:
m1_completo <- lmer(voto ~ libro + nivel_educ + clima_aula_escuela + (1 | id_colegio), 
                    data = b1_preparada)

# Si 'voto' es binaria (0 o 1):
# m1_completo <- glmer(voto ~ libro + nivel_educ + clima_aula_escuela + (1 | id_colegio), 
#                      data = b1_preparada, family = binomial)

summary(m1_completo)

library(modelsummary)

# Creamos una lista con los modelos
lista_modelos <- list(
  "M0: Vacío" = m0_vacio,
  "M1: Completo" = m1_completo
)

# Mostramos la tabla en consola o formato limpio
modelsummary(lista_modelos, 
             stars = TRUE, 
             output = "markdown") # Puedes cambiar "markdown" por "html" o "latex"


#ICCS 2009








