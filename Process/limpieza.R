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

#Bases de datos-----------------------------------------------------------------


CIVED_1999 <- read_sav("input/data/cs_chlf2.sav")

ICCS_2009 <- read_sav("input/data/ISGCHLC2.sav")

ICCS_2016 <- read_sas("input/data/isgchlc3.sas7bdat")

PACES_2019 <- read_excel("input/data/Base_Final_Estudiantes_1644_20_01_2020.v2+posoct.xlsx")


#Recodificación-----------------------------------------------------------------

#Preguntas N= Aula abierta
 #N1= Libertad de disernir
 #N2= Estimulo de desiciones propias
 #N3= Etimulo a la opinion propia x
 #N5= Pluralismo entre estudiantes x
 #N7= Autonomia cognitiva (conversar con distintos puntos de vista) x
 #N8= Profesores exponen temas desde distintos puntos de vista x
 #N9= Libertad de disernir con los profesores x

#Preguntas M= Expectativa de voto futuro
 #M1= Elecciones nacionales
 #M2= Informarse sobre los candidatos
 #M3= Elecciones locales

#Preguntas k = Aprendizajes en la escuela
 #K7= Aprender sobre la importancia del voto

#Preguntas P= Participación dentro de la escuela
  #P1= Actividad cultural o musical
  #P2= Participar de un debate organizado
  #P3= Votar por representantes
  #P4= Ser parte de las desiciones de la escuela
  #P5= Participar de una Asamblea
  #P6= Ser candidato

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
                K7 = CS4K7,
                Nive_educ = CSGEDUM,
                Peso_est = TOTWGT,
                Peso_esc = WGTADJ1) 

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
                P1 = IS2G15A,
                P2 = IS2G15B,
                P3 = IS2G15C,
                P4 = IS2G15D,
                P5 = IS2G15E,
                P6 = IS2G15F,
                Nive_educ_madre = IS2G07,
                Nive_educ_padre = IS2G09,
                Peso_est = TOTWGTS,
                Peso_esc = WGTADJ1S) 

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
                K7 = IS3G18A,
                P1 = IS3G16G,
                P2 = IS3G16A,
                P3 = IS3G16B,
                P4 = IS3G16C,
                P5 = IS3G16D,
                P6 = IS3G16E,
                Nive_educ_madre = IS3G07,
                Nive_educ_padre = IS3G09,
                Peso_est = TOTWGTS,
                Peso_esc = WGTADJ1S) 


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
                P3 = P30A,
                P5 = P30E,
                P6 = P30B,
                Nive_educ_madre = P67,
                Nive_educ_padre = P66,
                Peso_est = pond_estudiante_reg_dep_tens,
                Peso_esc = pond_esc_reg_dep_tens) 


#Homologación y limpieza de variables-------------------------------------------
#Homologación libros------------------------------------------------------------

#Categorias de repuesta cantidad de libros
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


#Homologación Voto--------------------------------------------------------------

proc_PACES_2019 <- proc_PACES_2019 %>%
  mutate(across(c(M1, M2, M3, libro, N1, N3, N5, N7, N8, N9, P3, P5, P6,
                  Nive_educ_madre, Nive_educ_padre), 
                ~replace(., . %in% c(5, 8, 9), NA)))

# CIVED y ICCS (Categorias de respuesta)
 #1= Yo desde luego no haría esto.
 #2= Yo probablemente no haría esto.
 #3= Yo probablemente haría esto.
 #4= Sin duda lo haría.

proc_CIVED_1999 <- proc_CIVED_1999 %>%
  mutate(across(c(M1, M2), ~ 5 - .)) #Para reordenar categorias de no probable a probable 

proc_ICCS_2009 <- proc_ICCS_2009 %>%
  mutate(across(c(M1, M2, M3), ~ 5 - .))

proc_ICCS_2016 <- proc_ICCS_2016 %>%
  mutate(across(c(M1, M2, M3), ~ 5 - .))


# PACES (Categorias de respuesta)
 #1=Seguro no haré esto
 #2=Tal vez haré esto
 #3=Seguro haré esto


#Homologación Nivel educacioneal de los padres----------------------------------

#Categorias de respuestas
 #0=Básica incompleta o menos
 #1=Básica completa
 #2=Media completa
 #3=Terciaria Técnica
 #4=Universitaria / Postgrado

# Homologación nivel educacional de la madre--------------

proc_CIVED_1999$Nive_educ <- car::recode(proc_CIVED_1999$Nive_educ, 
                                     "1=0; 2=1; 3=2; 4=2; 5=3; 6=4; 7=4",
                                     as.numeric = TRUE)

proc_ICCS_2009$Nive_educ_madre <- car::recode(proc_ICCS_2009$Nive_educ_madre, 
                                    "6=0; 5=1; 3=2; 4=2; 2=3; 1=4",
                                    as.numeric = TRUE)

proc_ICCS_2016$Nive_educ_madre <- car::recode(proc_ICCS_2016$Nive_educ_madre, 
                                    "5=0; 4=1; 3=2; 2=3; 1=4",
                                    as.numeric = TRUE)

proc_PACES_2019$Nive_educ_madre <- car::recode(proc_PACES_2019$Nive_educ_madre, 
                                     "1=0; 2=1; 3=2; 4=3; 5=4",
                                     as.numeric = TRUE)

#Homologación nivel educacional del padre-----------------

proc_ICCS_2009$Nive_educ_padre <- car::recode(proc_ICCS_2009$Nive_educ_padre, 
                                              "6=0; 5=1; 3=2; 4=2; 2=3; 1=4",
                                              as.numeric = TRUE)

proc_ICCS_2016$Nive_educ_padre <- car::recode(proc_ICCS_2016$Nive_educ_padre, 
                                              "5=0; 4=1; 3=2; 2=3; 1=4",
                                              as.numeric = TRUE)

proc_PACES_2019$Nive_educ_padre <- car::recode(proc_PACES_2019$Nive_educ_padre, 
                                               "1=0; 2=1; 3=2; 4=3; 5=4",
                                               as.numeric = TRUE)

#Tomamos en consideración el nivel mayor entre el padre y la madre-------

proc_ICCS_2009 <- proc_ICCS_2009 %>%
  mutate(nivel_educ = pmax(Nive_educ_madre, Nive_educ_padre, na.rm = TRUE)) %>%
  mutate(nivel_educ = ifelse(is.infinite(nivel_educ), NA, nivel_educ))

proc_ICCS_2016 <- proc_ICCS_2016 %>%
  mutate(nivel_educ = pmax(Nive_educ_madre, Nive_educ_padre, na.rm = TRUE)) %>%
  mutate(nivel_educ = ifelse(is.infinite(nivel_educ), NA, nivel_educ))

proc_PACES_2019 <- proc_PACES_2019 %>%
  mutate(nivel_educ = pmax(Nive_educ_madre, Nive_educ_padre, na.rm = TRUE)) %>%
  mutate(nivel_educ = ifelse(is.infinite(nivel_educ), NA, nivel_educ))

#Indice de participación (expectativa de voto)-----------------------------------

proc_CIVED_1999 <- proc_CIVED_1999 %>%
  mutate(indice_voto = rowMeans(select(., M1, M2), na.rm = TRUE))

proc_ICCS_2009 <- proc_ICCS_2009 %>%
  mutate(indice_voto = rowMeans(select(., M1, M2, M3), na.rm = TRUE))

proc_ICCS_2016 <- proc_ICCS_2016 %>%
  mutate(indice_voto = rowMeans(select(., M1, M2, M3), na.rm = TRUE))

proc_PACES_2019 <- proc_PACES_2019 %>%
  mutate(indice_voto = rowMeans(select(., M1, M2, M3), na.rm = TRUE))

#Homologación participación dentro de la escuela--------------------------------


#P3---------------------------------------------------
proc_ICCS_2009$P3 <- car::recode(proc_ICCS_2009$P3, 
                                              "1=1; 2=0; 3=0",
                                              as.numeric = TRUE)

proc_ICCS_2016$P3 <- car::recode(proc_ICCS_2016$P3, 
                                 "1=1; 2=0; 3=0",
                                 as.numeric = TRUE)

proc_PACES_2019$P3 <- car::recode(proc_PACES_2019$P3, 
                                 "1=1; 2=0; 3= NA",
                                 as.numeric = TRUE)
#P5-----------------------------------------------------

proc_ICCS_2009$P5 <- car::recode(proc_ICCS_2009$P5, 
                                 "1=1; 2=0; 3=0",
                                 as.numeric = TRUE)

proc_ICCS_2016$P5 <- car::recode(proc_ICCS_2016$P5, 
                                 "1=1; 2=0; 3=0",
                                 as.numeric = TRUE)

proc_PACES_2019$P5 <- car::recode(proc_PACES_2019$P5, 
                                  "1=1; 2=0; 3=NA",
                                  as.numeric = TRUE)

#Homologación de variable K7 aprendizaje sobre el voto en la escuela

proc_CIVED_1999$K7 <- car::recode(proc_CIVED_1999$K7, 
                               "1=0; 2=0; 3=1; 4=1",
                               as.numeric = TRUE)

proc_ICCS_2016$K7 <- car::recode(proc_ICCS_2016$K7, 
                                 "1=1; 2=1; 3=0; 4=0",
                                 as.numeric = TRUE)

proc_CIVED_1999 <- proc_CIVED_1999 %>%
  group_by(id_colegio) %>%
  mutate(
    perp_aprendizaje_voto_esc   = mean(K7, na.rm = TRUE),
  ) %>%
  ungroup()

proc_ICCS_2016 <- proc_ICCS_2016 %>%
  group_by(id_colegio) %>%
  mutate(
    perp_aprendizaje_voto_esc     = mean(K7, na.rm = TRUE),
  ) %>%
  ungroup()


#Creación de variables contextuales en proporción de estudiantes---------------

# --- BASE 2: ICCS 2009 ---
proc_ICCS_2009 <- proc_ICCS_2009 %>%
  group_by(id_colegio) %>%
  mutate(
    tasa_voto_escuela     = mean(P3, na.rm = TRUE),
    tasa_asamblea_escuela = mean(P5, na.rm = TRUE)
  ) %>%
  ungroup()

# --- BASE 3: ICCS 2016 ---
proc_ICCS_2016 <- proc_ICCS_2016 %>%
  group_by(id_colegio) %>%
  mutate(
    tasa_voto_escuela     = mean(P3, na.rm = TRUE),
    tasa_asamblea_escuela = mean(P5, na.rm = TRUE)
  ) %>%
  ungroup()

# --- BASE 4: PACES 2019 ---
proc_PACES_2019 <- proc_PACES_2019 %>%
  group_by(id_colegio) %>%
  mutate(
    tasa_voto_escuela     = mean(P3, na.rm = TRUE),
    tasa_asamblea_escuela = mean(P5, na.rm = TRUE)
  ) %>%
  ungroup()


#Crombah voto-------------------------------------------------------------------

#CIVED

items_participacion_1999 <- proc_CIVED_1999 %>% 
  select(M1, M2,)

resultado_alpha_1999 <- psych::alpha(items_participacion_1999)

print(resultado_alpha_1999$total$std.alpha)

#ICCS 2009

items_participacion_2009 <- proc_ICCS_2009 %>% 
  select(M1, M2, M3)

resultado_alpha_2009 <- psych::alpha(items_participacion_2009)

print(resultado_alpha_2009$total$std.alpha)

#ICCS 2016

items_participacion_2016 <- proc_ICCS_2016 %>% 
  select(M1, M2, M3)

resultado_alpha_2016 <- psych::alpha(items_participacion_2016)

print(resultado_alpha_2016$total$std.alpha)

#PACES

items_participacion_2019 <- proc_PACES_2019 %>% 
  select(M1, M2, M3)

resultado_alpha_2019 <- psych::alpha(items_participacion_2019)

print(resultado_alpha_2019$total$std.alpha)


#índice de aula abierta---------------------------------------------------------

# CIVED 1999
proc_CIVED_1999 <- proc_CIVED_1999 %>%
  mutate(indice_aula_ind = rowMeans(select(., N3, N5, N7, N8, N9), na.rm = TRUE)) %>%
  group_by(id_colegio) %>%
  mutate(clima_aula_escuela = mean(indice_aula_ind, na.rm = TRUE)) %>%
  ungroup()

# ICCS 2009
proc_ICCS_2009 <- proc_ICCS_2009 %>%
  mutate(indice_aula_ind = rowMeans(select(., N3, N5, N7, N8, N9), na.rm = TRUE)) %>%
  group_by(id_colegio) %>%
  mutate(clima_aula_escuela = mean(indice_aula_ind, na.rm = TRUE)) %>%
  ungroup()

# ICCS 2016
proc_ICCS_2016 <- proc_ICCS_2016 %>%
  mutate(indice_aula_ind = rowMeans(select(., N3, N5, N7, N8, N9), na.rm = TRUE)) %>%
  group_by(id_colegio) %>%
  mutate(clima_aula_escuela = mean(indice_aula_ind, na.rm = TRUE)) %>%
  ungroup()

# PACES 2019
proc_PACES_2019 <- proc_PACES_2019 %>%
  mutate(indice_aula_ind = rowMeans(select(., N3, N5, N7, N8, N9), na.rm = TRUE)) %>%
  group_by(id_colegio) %>%
  mutate(clima_aula_escuela = mean(indice_aula_ind, na.rm = TRUE)) %>%
  ungroup()

#Version con variables extra (N1 Y N2 en las bases donde se encuentra)

# CIVED 1999
proc_CIVED_1999 <- proc_CIVED_1999 %>%
  mutate(indice_aula_ind_ext = rowMeans(select(., N1, N2, N3, N5, N7, N8, N9), na.rm = TRUE)) %>%
  group_by(id_colegio) %>%
  mutate(clima_aula_escuela_ext = mean(indice_aula_ind_ext, na.rm = TRUE)) %>%
  ungroup()

# ICCS 2009
proc_ICCS_2009 <- proc_ICCS_2009 %>%
  mutate(indice_aula_ind_ext = rowMeans(select(., N2, N3, N5, N7, N8, N9), na.rm = TRUE)) %>%
  group_by(id_colegio) %>%
  mutate(clima_aula_escuela_ext = mean(indice_aula_ind_ext, na.rm = TRUE)) %>%
  ungroup()

# ICCS 2016
proc_ICCS_2016 <- proc_ICCS_2016 %>%
  mutate(indice_aula_ind_ext = rowMeans(select(., N2, N3, N5, N7, N8, N9), na.rm = TRUE)) %>%
  group_by(id_colegio) %>%
  mutate(clima_aula_escuela_ext = mean(indice_aula_ind_ext, na.rm = TRUE)) %>%
  ungroup()

# PACES 2019
proc_PACES_2019 <- proc_PACES_2019 %>%
  mutate(indice_aula_ind_ext = rowMeans(select(., N1, N3, N5, N7, N8, N9), na.rm = TRUE)) %>%
  group_by(id_colegio) %>%
  mutate(clima_aula_escuela_ext = mean(indice_aula_ind_ext, na.rm = TRUE)) %>%
  ungroup()

#Crombach aula abierta----------------------------------------------------------

#CIVED

items_aula_1999 <- proc_CIVED_1999 %>% 
  select(N3, N5, N7, N8, N9)

resultado_aula_1999 <- psych::alpha(items_aula_1999)

print(resultado_aula_1999$total$std.alpha)

#ICCS 2009

items_aula_2009 <- proc_ICCS_2009 %>% 
  select(N3, N5, N7, N8, N9)

resultado_aula_2009 <- psych::alpha(items_aula_2009)

print(resultado_aula_2009$total$std.alpha)

#ICCS 2016

items_aula_2016 <- proc_ICCS_2016 %>% 
  select(N3, N5, N7, N8, N9)

resultado_aula_2016 <- psych::alpha(items_aula_2016)

print(resultado_aula_2016$total$std.alpha)

#PACES

items_aula_2019 <- proc_PACES_2019 %>% 
  select(N3, N5, N7, N8, N9)

resultado_aula_2019 <- psych::alpha(items_aula_2019)

print(resultado_aula_2019$total$std.alpha)

#Crombach aula abierta (versión extendida)----------------------------------------------------------

#CIVED

items_aula_1999 <- proc_CIVED_1999 %>% 
  select(N1, N2, N3, N5, N7, N8, N9)

resultado_aula_1999 <- psych::alpha(items_aula_1999)

print(resultado_aula_1999$total$std.alpha)

#ICCS 2009

items_aula_2009 <- proc_ICCS_2009 %>% 
  select(N2, N3, N5, N7, N8, N9)

resultado_aula_2009 <- psych::alpha(items_aula_2009)

print(resultado_aula_2009$total$std.alpha)

#ICCS 2016

items_aula_2016 <- proc_ICCS_2016 %>% 
  select(N2, N3, N5, N7, N8, N9)

resultado_aula_2016 <- psych::alpha(items_aula_2016)

print(resultado_aula_2016$total$std.alpha)

#PACES

items_aula_2019 <- proc_PACES_2019 %>% 
  select(N1, N3, N5, N7, N8, N9)

resultado_aula_2019 <- psych::alpha(items_aula_2019)

print(resultado_aula_2019$total$std.alpha)

#Crombach participación escolar-------------------------------------------------

#ICCS 2009

items_aula_2009 <- proc_ICCS_2009 %>% 
  select(P3, P5)

resultado_aula_2009 <- psych::alpha(items_aula_2009)

print(resultado_aula_2009$total$std.alpha)

#ICCS 2016

items_aula_2016 <- proc_ICCS_2016 %>% 
  select(P3, P5)

resultado_aula_2016 <- psych::alpha(items_aula_2016)

print(resultado_aula_2016$total$std.alpha)

#PACES

items_aula_2019 <- proc_PACES_2019 %>% 
  select(P3, P5)

resultado_aula_2019 <- psych::alpha(items_aula_2019)

print(resultado_aula_2019$total$std.alpha)


#Guardado de base---------------------------------------------------------------

# 2. Guardar las bases en formato .sav
# Usamos haven::write_sav para mantener la compatibilidad con SPSS

saveRDS(proc_CIVED_1999, "output/data_procesada/proc_CIVED_1999.rds")
saveRDS(proc_ICCS_2009,  "output/data_procesada/proc_ICCS_2009.rds")
saveRDS(proc_ICCS_2016,  "output/data_procesada/proc_ICCS_2016.rds")
saveRDS(proc_PACES_2019,  "output/data_procesada/proc_PACES_2019.rds")

message("Bases exportadas con éxito. Verifica la carpeta output/data_procesada")

#Juntar las bases de datos------------------------------------------------------

bases_juntas <- bind_rows(
  "1999" = proc_CIVED_1999,
  "2009" = proc_ICCS_2009,
  "2016" = proc_ICCS_2016,
  "2019" = proc_PACES_2019,
  .id = "ano"
)

#ID para la base Global
bases_juntas <- bases_juntas %>%
  mutate(
    # ID única para estudiantes (Nivel 1)
    id_global = paste0(ano, "_", id),
    # ID única para colegios ( Nivel 2)
    id_colegio_global = paste0(ano, "_", id_colegio))


# 3. Intenta guardar nuevamente
haven::write_sav(bases_juntas, "output/data_procesada/bases_juntas.sav")





