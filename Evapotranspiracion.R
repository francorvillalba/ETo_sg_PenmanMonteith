#SCRIPT PARA OBTENER DATOS DE EVAPOTRANSPIRACIÓN DE REFERENCIA SEGÙN PENMAN MONTEITH
#A PARTIR DE LA SALIDA DE MEMORIA DE LA ESTACIÓN PEGASUS EP2010

#1. Paquetes requeridos para el procesamiento

library(dplyr) #Facilita agrupar, resumir y transformar los datos
library(lubridate) #Simplifica el manejo de fechas

#2. Importar datos: 

datos<-read.table("EP2010_3001_M1_251120.txt", header=F, sep= "", skip=4) #Con skip=4 se omiten las lineas de encabezado.

columnas<-c("fecha", "hora", "P_ATM_(HPa)", "T_int_(gC)", 
            "Pluvio_(mm)", "Prom_Vel_vto_(kmh)", "Prom_Dir_vto_(g)", "Max_Vel_vto_(kmh)", 
            "Max_Dir_vto_(g)", "T_ext_(gC)","HRA_(%)", "Temp_Suelo_(gC)", 
            "Rad_UV_(Wm2)", "Rad_Solar_(Wm2)", "Bateria_(Vcc)", "Hum_Hoja_(min)") 
colnames(datos)<-columnas #Agrega los nombres de las columnas

#3.Agregar fecha en formato POSIXct

datos["fyh"]<-paste(datos$fecha, datos$hora, sep=" ")

datos["fechaD"]<-as.POSIXct(datos$fyh, format="%d/%m/%y %H:%M") #Se crea una columna con formato POSIXct para facilitar la manipulación según las fechas


#4. convertir columnas a formato númerico, dado que la mayoria de las variables fueron importadas en formato character ·
#se puede verificar con str(datos)

convertir<- function(vector){
  v1<-gsub(",",".",vector)
  v2<-as.numeric(v1)
  return(v2)
}

datos$`P_ATM_(HPa)`<-convertir(datos$`P_ATM_(HPa)`)
datos$`T_ext_(gC)`<-convertir(datos$`T_ext_(gC)`)
datos$`Prom_Vel_vto_(kmh)`<-convertir(datos$`Prom_Vel_vto_(kmh)`)
datos$`Max_Vel_vto_(kmh)`<-convertir(datos$`Max_Vel_vto_(kmh)`)



#5. Agrupar de manera diaria, se resumen los datos por dia, también se convierten las unidades (Radiación solar de W/m² a MJ/día, velocidad del viento de Km/h a m/s) y
#se calcula la constante psicométrica
datos_diarios <- datos %>% group_by("fecha" = as.Date(fechaD)) %>% summarise(
  presion = mean(`P_ATM_(HPa)`),
  temp_max = max(`T_ext_(gC)`), 
  temp_min = min(`T_ext_(gC)`),
  HRA_min = min(`HRA_(%)`),
  HRA_max= max(`HRA_(%)`),
  HRA = mean(`HRA_(%)`),
  Rad_Solar = sum(`Rad_Solar_(Wm2)`*900/1000000),
  const_psico = mean(`P_ATM_(HPa)`)*0.0000665,
  Vel_viento = mean(`Prom_Vel_vto_(kmh)`/3.6)
)

datos_diarios<-datos_diarios[-c(1,105),] #se eliminan el primer y ultimo día ya que el registro es incompletos

#6. Se agregan columnas con las variables relacionadas al déficit de presión de vapor
datos_diarios <- datos_diarios %>% mutate(
  temp_media = (temp_max+temp_min)/2, # Calculo de la temperatura media diaria
  delta= (4098*0.6108*exp(17.27*temp_media/(temp_media+237.3)))/(temp_media+237.3)^2, #Cálculo de la pendiente de la curva de presión de vapor
  es_tmax = 0.6108 * exp(17.27 * temp_max / (temp_max + 237.3)), #Cálculo de la presión de saturación de vapor a la temperatura máxima
  es_tmin = 0.6108 * exp(17.27 * temp_min / (temp_min + 237.3)), #Cálculo de la presión de saturación de vapor a la temperatura mínima
  es = (es_tmax + es_tmin) / 2, #Cálculo de la presión de saturación de vapor promedio diaria
  ea2=(es_tmin*(HRA_max/100)+es_tmax*(HRA_min/100))/2, #Cálculo de la presión real de vapor
  vpd2 = es - ea2 #Cálculo del déficit de presión de vapor
)

#7. Cálculo de la radiación extraterrestre (Ra) para cada día

lat_grados<--26.4 #Latitud en grados decimales (signo negativo para el hemisferio sur)
lat_rad<- lat_grados*pi/180 #Conversión de la latitud a radianes

datos_diarios<- datos_diarios %>% mutate(
  dia_juliano = yday(fecha), #se obtiene el día juliano
  dr = 1+0.033*cos(2*pi*dia_juliano/365), # Cálculo de la distancia relativa tierra-sol
  dec_so = 0.409 * sin(2 * pi * dia_juliano / 365 - 1.39),# Cálculo de la declinacion solar
  omega_s = acos(-tan(lat_rad) * tan(dec_so)), #Cálculo del ángulo a la puesta del sol
  Ra = (24 * 60 / pi) * 0.0820 * dr * (omega_s * sin(lat_rad) * sin(dec_so) + cos(lat_rad) * cos(dec_so) * sin(omega_s)) #Cálculo de la radiación extraterrestre
)

#8. Cálculo de la radiación neta (RN)
ALTITUD <- 228 # Altitud en metros
ALBEDO <- 0.23 # Fracción de radiación reflejada por un cultivo de referencia (0,23 por definición)
SIGMA <- 4.903e-09 # Constante de Stefan-Boltzmann (MJ/K^4/m2/día)

datos_diarios <- datos_diarios %>% mutate(
  Rso = Ra*(0.75+2e-05*ALTITUD), #Cálculo de la radiación de cielo despejado
  Rns = (1-ALBEDO)*Rad_Solar, #Cálculo de la RADIACION NETA DE ONDA CORTA
  T_kelvin_4 = (temp_media + 273.15)^4, # T en Kelvin elevado a la 4
  razon= Rad_Solar/Rso, #Cálculo de la radiación relativa de onda corta
  Rnl = SIGMA * T_kelvin_4 * (0.34 - 0.14 * sqrt(ea2)) * (1.35 * (razon) - 0.35), #RADIACION NETA DE ONDA LARGA
  RNETA = Rns - Rnl
)

#9. Cálculo de la Evapotranspiración de referencia (mm/día)
datos_diarios <- datos_diarios %>% mutate(
  ETo = (0.408*delta*RNETA+const_psico*(900/(temp_media+273))*Vel_viento*vpd2)/(delta+const_psico*(1+0.34*Vel_viento))
)

#10. Guardar el dataframe resultante en un archivo RDS para futuro uso. 
saveRDS(datos_diarios, "ResultadoETo.rds")





