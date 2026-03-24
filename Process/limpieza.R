#Tesis Pregrado
#Autor: Benjamín Zavala
#Fecha: 03/02/2026
#------------------

#Libreria-----------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, 
               dplyr, 
               haven, 
               car, 
               magrittr,
               psych,
               readxl)

options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls()) # para limpiar el entorno de trabajo

#Bases de datos-----------------------------------------------------------------


CIVED_1999 <- read_sav("input/data/cs_chlf2.sav")

ICCS_2009 <- read_sav("input/data/ISGCHLC2.sav")

ICCS_2016 <- read_sas("input/data/isgchlc3.sas7bdat")

PACES_2019 <- read_excel("input/data/Base_Final_Estudiantes_1644_20_01_2020.v2+posoct.xlsx")


#Recodificación-----------------------------------------------------------------

proc_CIVED_1999 <- CIVED_1999 %>% 
  dplyr::select(id = ID,
                id_colegio = IDSCHOOL,
                libro = CSGBOOK,
                N1 = CS4N1,
                N2 = CS4N2,
                N3 = CS4N3,
                N5 = CS4N5,
                N7 = CS4N7,
                N8 = CS4N8,
                N9 = CS4N9,
                M1 = CS5M1,
                M2 = CS5M2,
                Nive_educ = CSGEDUM) 

proc_CIVED_1999

#-------------------------------------------------------------------------------

proc_ICCS_2009 <- ICCS_2009 %>% 
  dplyr::select(id = IDSTUD,
                id_colegio = IDSCHOOL,
                libro = IS2G11,
                N2 = IS2G16B,
                N3 = IS2G16C,
                N5 = IS2G16E,
                N7 = IS2G16F,
                N8 = IS2G16G,
                N9 = IS2G16D,
                M1 = IS2P32B,
                M2 = IS2P32C,
                M3 = IS2P32A, 
                Nive_educ_madre = IS2G07,
                Nive_educ_padre = IS2G09) 

proc_ICCS_2009

#-------------------------------------------------------------------------------

proc_ICCS_2016 <- ICCS_2016 %>% 
  dplyr::select(id = IDSTUD,
                id_colegio = IDSCHOOL,
                libro = IS3G11,
                N2 = IS3G17A,
                N3 = IS3G17B,
                N5 = IS3G17D,
                N7 = IS3G17E,
                N8 = IS3G17F,
                N9 = IS3G17C,
                M1 = IS3G31B,
                M2 = IS3G31C,
                M3 = IS3G31A, 
                Nive_educ_madre = IS3G07,
                Nive_educ_padre = IS3G09) 

proc_ICCS_2016

#-------------------------------------------------------------------------------

proc_PACES_2019 <- PACES_2019 %>% 
  dplyr::select(id = FOLIO,
                id_colegio = RBD,
                libro = P68,
                N1 = P49A,
                N3 = P49B,
                N5 = P49D,
                N7 = P49E,
                N8 = P49F,
                N9 = P49C,
                M1 = P31B,
                M2 = P31C,
                M3 = P31A, 
                Nive_educ_madre = P67,
                Nive_educ_padre = P66) 

proc_PACES_2019

#Homologación y limpieza de variables------------------------------------------------------
#Homologación libros------------------------------------------------------------

# 0 =0-10 libros 
# 1 =11-100 libros
# 2 =101-200 libros
# 3 =Más de 200 libros
    
proc_CIVED_1999$libro <- car::recode(proc_CIVED_1999$libro, 
                                      "1=0; 2=0; 3=1; 4=1; 5=2; 6=3",
                                      as.numeric = TRUE)

proc_ICCS_2009$libro <- car::recode(proc_ICCS_2009$libro, 
                                     "1=0; 2=1; 3=1; 4=2; 5=3; 6=3",
                                     as.numeric = TRUE)

proc_ICCS_2016$libro <- car::recode(proc_ICCS_2016$libro, 
                                     "1=0; 2=1; 3=1; 4=2; 5=3",
                                     as.numeric = TRUE)

proc_PACES_2019$libro <- car::recode(proc_PACES_2019$libro, 
                                     "1=0; 2=1; 3=1; 4=2; 5=3; 6=3",
                                     as.numeric = TRUE)



#Homologación Aula Abierta------------------------------------------------------



#Homologación Voto--------------------------------------------------------------

proc_PACES_2019 <- proc_PACES_2019 %>%
  mutate(across(c(M1, M2, M3, libro, N1, N3, N5, N7, N8, N9, 
                  Nive_educ_madre, Nive_educ_padre), 
                ~replace(., . %in% c(8, 9), NA)))

colMeans(is.na(proc_PACES_2019)) * 100

#Homologación Nivel educacioneal Madre------------------------------------------



#Homologación Nivel educacional Padre-------------------------------------------



#Escala de nivel socioeconómico-------------------------------------------------

library(dplyr)

data <- data %>%
  mutate(indice_recursos = rowSums(select(., libro_hogar, internet, computador,
                                          auto), na.rm = TRUE))


#Indice de participación--------------------------------------------------------

# Usaremos la librería 'psych' que es el estándar en sociología
install.packages("psych")
library(psych)

# 1. Seleccionamos solo los ítems de la escala
items_clima <- data %>% select(clima_1, clima_2, clima_3, clima_4)

# 2. Verificamos la fiabilidad (Alfa de Cronbach)
# Buscamos un valor mayor a 0.7
alpha_resultado <- psych::alpha(items_clima)
print(alpha_resultado$total$std.alpha)

# 3. Si el Alfa es bueno, creamos la escala por promedio
# (El promedio mantiene la unidad de medida original, ej: 1 a 4)
data$escala_clima <- rowMeans(items_clima, na.rm = TRUE)






