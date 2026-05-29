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
               fastDummies)

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
  select(voto, libro_0, libro_1, libro_2, libro_3, nivel_educ_0, nivel_educ_1, nivel_educ_2, nivel_educ_3,
         nivel_educ_4, clima_aula_escuela)

b2_seleccionada <- b2 %>% 
  select(voto, libro_0, libro_1, libro_2, libro_3, nivel_educ_0, nivel_educ_1, nivel_educ_2, nivel_educ_3,
         nivel_educ_4, clima_aula_escuela)

b3_seleccionada <- b3 %>% 
  select(voto, libro_0, libro_1, libro_2, libro_3, nivel_educ_0, nivel_educ_1, nivel_educ_2, nivel_educ_3,
         nivel_educ_4, clima_aula_escuela)

b4_seleccionada <- b4 %>% 
  select(voto, libro_0, libro_1, libro_2, libro_3, nivel_educ_0, nivel_educ_1, nivel_educ_2, nivel_educ_3,
         nivel_educ_4, clima_aula_escuela)

# Calculas la correlación
cor(b1_seleccionada, use = "complete.obs")

cor(b2_seleccionada, use = "complete.obs")

cor(b3_seleccionada, use = "complete.obs")

cor(b4_seleccionada, use = "complete.obs")



