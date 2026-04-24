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
               readxl)

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













