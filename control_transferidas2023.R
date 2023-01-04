#Carga de paquetes

library(tidyverse)
library(readxl)
library(dplyr)
library(writexl)
library(stringr)

#Exportación de bases
Transferidas_suaci <- read.csv("C:/Users/natva/OneDrive/Escritorio/AGC_DATOS/Exportaciones/SUACI_TRANSF161222.csv")
Planilla_transferidas <- read_xlsx("C:/Users/natva/OneDrive/Escritorio/AGC_DATOS/Exportaciones/PlanillaTransf161222.xlsx", sheet = "REGISTRO")
Planilla_transferidas$NSUACI <- gsub(" |\r\n|\n|\r|", "", Planilla_transferidas$NSUACI)


#Exportacioón de suacis cerradas como transferidas en 2022
Transferidas_suaci <- Transferidas_suaci %>%
  rename(NSUACI="Número") %>%
  select("NSUACI","Estado.del.esquema")
Transferidas_suaci$NSUACI <- gsub(" |\r\n|\n|\r|", "", Transferidas_suaci$NSUACI)


#elimino dos que se enviaron en 2021 pero se cerraron en sistema en 2022
desestimar_suaci <- c("01091969/16", "00518363/21","00532766/21")
Transferidas_suaci<- filter(Transferidas_suaci,!(str_detect(NSUACI, paste(desestimar_suaci, collapse = "|"))))

#trabajo de planilla para transferencias de 2022

#Eliminar de la revisión por ser de otro período y/o cerradas con un motivo distinto a transf 
#(pueden ser rectificaciones o ccoo enviadas en forma complementaria)

desestimar <- c("00583764/21","00571925/21", "00592932/21", "00584986/21","00595833/21","00589122/21","00477929/18", "00097368/22")
Planilla_transferidas<- filter(Planilla_transferidas,!(str_detect(NSUACI, paste(desestimar, collapse = "|"))))

#Chequeo de cantidad de valores únicos
length(unique(Planilla_transferidas$NSUACI)) 

#me quedo con los valores únicos (las suaci se ingresan más de una vez cuando van a más de un área)
Planilla_transferidas_unique <- unique(Planilla_transferidas$NSUACI)
Planilla_transferidas_unique <- as.data.frame(Planilla_transferidas_unique) %>%
  rename(NSUACI="Planilla_transferidas_unique") 

Planilla_transferidas_unique <- Planilla_transferidas_unique %>%
  mutate(estado ="en planilla")



Archivo_revision_transferidas_exp <- left_join(Transferidas_suaci, Planilla_transferidas_unique, BY=NSUACI)

Archivo_revision_transferidas_exp$estado[is.na(Archivo_revision_transferidas_exp$estado)] <- 0

Archivo_revision_transferidas_exp <- Archivo_revision_transferidas_exp  %>%
  filter(estado != "en planilla")



Archivo_revision_transferidas_exp20221214 <- Archivo_revision_transferidas_exp %>%
  add_column("Acción" = NA,
             "¿Hay desvío?" = NA,
             "¿Corresponde agregar a planilla?" = NA,
             "Analista responsable desvío" = NA,
             "NOTA rectificando" = NA,
             "¿Sumado a planilla transferidos? (SI/NO)" = NA,
             "Reporte (SI/NO/PENDIENTE)" = NA,
             "Observación" = NA,
             "¿Subsanada la falta de Match?" = NA)



#Inverse_revision <- left_join(Planilla_transferidas_unique,Transferidas_suaci, BY=NSUACI)


Archivo_revision_transferidas_exp20220914 <- write_xlsx(x = Archivo_revision_transferidas_exp, path = 'C:/Users/npvazquez/Desktop/AGC_DATOS/RESULTADOS_CONTROLES/Archivo_revision_transferidas_exp20220914.xlsx')




