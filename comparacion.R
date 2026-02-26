str(datos_ETo_agos_nov_2025)
montecarlo$fecha <- as.POSIXct(montecarlo$Fecha, format="%d/%m/%Y")

#elimino el primer y ultimo dia ya que el registro es incompleto
montecarlo<-montecarlo[-c(1,85),]
datos_ETo_agos_nov_2025<-datos_ETo_agos_nov_2025[-c(1, 105),]

montecarlo$fecha<-as.Date(montecarlo$fecha)
datos_ETo_agos_nov_2025$fecha <- as.Date(datos_ETo_agos_nov_2025$fecha)

str(montecarlo)

unidos<-merge(datos_ETo_agos_nov_2025, montecarlo, by="fecha")

library(dplyr)
library(tidyr)
library(ggplot2)

unidos_largo<-pivot_longer(unidos, !( fecha |Fecha ), names_to ="parametros", values_to = "valores")

saveRDS(unidos_largo, "Unidos.rds")


unidos_largo<- unidos_largo %>% mutate( variables = case_when(parametros=="ETo" ~ "Evapotranspiración de referencia Estación FCF",
                                                               parametros=="presion"~"Presion Atmosférica Estación FCF",
                                                               parametros=="temp_max"~"Temperatura Máxima Estación FCF",
                                                               parametros=="temp_min"~"Temperatura Mínima Estación FCF",
                                                               parametros=="HRA_min"~"Humedad Relativa Mínima Estación FCF",
                                                               parametros=="HRA_max"~"Humedad Relativa Máxima Estación FCF",
                                                               parametros=="HRA"~"Humedad Relativa Media Estación FCF",
                                                               parametros=="Rad_Solar"~"Radiación solar Estación FCF",
                                                               parametros=="const_psico"~"Constante psicométrica Estación FCF",
                                                               parametros=="Vel_viento"~"Velocidad del Viento Estación FCF",
                                                               parametros=="temp_media"~"Temperatura Media Estación FCF",
                                                               parametros=="delta"~"Pendiente de la Curva de Saturación de Vapor Estación FCF",
                                                               parametros=="es_tmax"~"Presión de saturación de vapor a la temperatura Máxima (FCF)",
                                                               parametros=="es_tmin"~"Presión de saturación de vapor a la temperatura Mínima (FCF)",
                                                               parametros=="es"~"Presión de saturación de vapor media (FCF)",
                                                               parametros=="ea1"~"ea1",
                                                               parametros=="ea2"~"Presión media de Vapor (FCF)",
                                                               parametros=="vpd1"~"vpd1",
                                                               parametros=="vpd2"~"Déficit de Presión de Vapor(FCF)",
                                                               parametros=="dia_juliano"~"Día Juliano",
                                                               parametros=="dr"~"Distancia relativa inversa Tierra-Sol",
                                                               parametros=="dec_so"~"Declinación solar",
                                                               parametros=="omega_s"~"Ángulo solar al ocaso",
                                                               parametros=="Ra"~"Radiación Extraterrestre",
                                                               parametros=="Rso"~"Radiación de cielo despejado",
                                                               parametros=="Rns"~"Radiación neta de onda corta",
                                                               parametros=="T_kelvin_4"~"T_k_4",
                                                               parametros=="razon"~"Radiación Relativa de Onda Corta",
                                                               parametros=="Rnl"~"Radiación neta de onda larga",
                                                               parametros=="RNETA"~"Radiación neta",
                                                               parametros=="ETo"~"Evapotranspiración de referencia, Estación FCF",
                                                               parametros=="Radiacion_Global"~"Radiación global, Estación Montecarlo",
                                                               parametros=="DPV"~"Déficit de Presión de Vapor (Montecarlo)",
                                                               parametros=="U_2"~"Velocidad del Viento Estación Montecarlo",
                                                               parametros=="Tmax"~"Temperatura Máxima Estación Montecarlo",
                                                               parametros=="Tmin"~"Temperatura Mínima Estación Montecarlo",
                                                               parametros=="Tmed"~"Temperatura Media Estación Montecarlo",
                                                               parametros=="ETP"~"Evapotranspiración de referencia Estación Montecarlo",
                                                              parametros=="ETo2"~"Evapotranspiración de referencia Estación FCF modificado"))


ggplot(data=unidos_largo[unidos_largo$parametros=="ETo" | unidos_largo$parametros=="ETP"|
                           unidos_largo$parametros=="ETo2",],
        mapping=aes(x=fecha, y=valores, color=variables))+geom_line()+
  labs(title="Evapotranspiración de Referencia",
       x= "Fecha",
       y=expression(paste("ET"[0], " (mm"~dia^-1~")")))+theme_minimal()+
  theme(legend.position="bottom")

ggplot(data=unidos_largo[unidos_largo$parametros=="Tmed" | unidos_largo$parametros=="temp_media",],
       mapping=aes(x=fecha, y=valores, color=variables))+geom_line()+
  labs(title="Temperaturas Media diaria",
       x= "Fecha",
       y="Temperatura Media (ºC)")+theme_minimal()+
  theme(legend.position="bottom")

ggplot(data=unidos_largo[unidos_largo$parametros=="vpd2" | unidos_largo$parametros=="DPV",],
       mapping=aes(x=fecha, y=valores, color=variables))+geom_line()+
  labs(title="Déficit de Presión de Vapor promedio diario",
       x= "Fecha",
       y="Déficit de presión de vapor (kPa)")+theme_minimal()+
  theme(legend.position="bottom")


ggplot(data=unidos_largo[unidos_largo$parametros=="U_2" | unidos_largo$parametros=="Vel_viento",],
       mapping=aes(x=fecha, y=valores, color=variables))+geom_line()+
  labs(title="Velocidad del Viento promedio diaria",
       x= "Fecha",
       y=expression("Velocidad del Viento (m "~s^-1~")"))+theme_minimal()+
  theme(legend.position="bottom")

ggplot(data=unidos_largo[unidos_largo$parametros=="Radiacion_Global" | 
                           unidos_largo$parametros=="Rad_Solar" | 
                           unidos_largo$parametros=="Rso",],
       mapping=aes(x=fecha, y=valores, color=variables))+geom_line()+
  labs(title="Radiación solar diaria",
       x= "Fecha",
       y=expression("Radiación solar (MJ "~día^-1~")"))+theme_minimal()+
  theme(legend.position="bottom")


unidos <- unidos %>% mutate(
  ETo2 = (0.408*delta*RNETA+const_psico*(900/(temp_media+273))*U_2*vpd2)/(delta+const_psico*(1+0.34*U_2))
)


mean(montecarlo$ETP)
max(datos_ETo_agos_nov_2025$ETo)

mean(unidos$Vel_viento)

max(unidos$U_2)
