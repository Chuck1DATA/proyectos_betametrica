TrabFinalMOD1
================
Carlos Montes Rodriguez
2024-04-19

\#TRABAJO FINAL CIENCIA DE DATOS MÒDULO I - ESTADÍSTICA PARA LA TOMA DE
DECISIONES

# CARLOS A. MONTES RODRIGUEZ

# 1. Carga de base de datos Bancos Privados.xlsx

install.packages(“dplyr”) install.packages(“openxlsx”)
install.packages(“lubridate”,dependencies = TRUE)

library(dplyr) library(openxlsx) library(lubridate)

file.choose()

data1\<-read.xlsx(“C:\Users\CAMR-pc\Desktop\PROGRAMAS
BETAMETRICA\Programa experto en ciencia de datos 2023\MODULO I.- Ciencia
de Datos Estadística para la Toma de
Decisiones\BASES_DATOS_BASES_ZS2ZD2\Bancos privados (EjCMR).xlsx”,
detectDates = TRUE)

\#Cambiando nombre a los campos de la base de datos y formateando campos
\#numéricos con separador de miles y alineación a derecha

names(data1)

names(data1)\[3\]\<-“Saldo.total.disponible”
names(data1)\[4\]\<-“Cantidad.de.clientes”

nueva.base\<-data1

View(nueva.base)

\#Convirtiendo a caracteres a datos numéricos

str(nueva.base)

nueva.base$NUMERO.DE.CUENTAS<-as.numeric(nueva.base$NUMERO.DE.CUENTAS)  
nueva.base$Saldo.total.disponible<-as.numeric(nueva.base$Saldo.total.disponible)
nueva.base$Cantidad.de.clientes<-as.numeric(nueva.base$Cantidad.de.clientes)

\#Aplicando filtros a consulta

nueva.base\<-filter(nueva.base,Cantidad.de.clientes\>=500,Cantidad.de.clientes\<=5000)

nueva.base\<-filter(nueva.base,ENTIDAD==“BP CAPITAL”)

names(nueva.base)

View(nueva.base) str(nueva.base)

\#Generar variable saldo promedio por cada cuenta:
Saldo.total.disponible / NUMERO.DE.CUENTAS
nueva.base\<-mutate(nueva.base,
Promedio.por.cuenta=as.numeric(nueva.base\$Saldo.total.disponible)/
NUMERO.DE.CUENTAS)

\#Observación de estadísticos: media de saldos totales en grupo de
entidades, \#mínimos y máximos

mean(nueva.base\$Saldo.total.disponible)

min(nueva.base\$Saldo.total.disponible)

max(nueva.base\$Saldo.total.disponible)

\#Generando reporte de Entidades

names(data1)

nueva.base\<-data1

nueva.base %\>% group_by(ENTIDAD) %\>%
summarise(prom_Cantidad.de.clientes=mean(Cantidad.de.clientes)) %\>%
arrange(prom_Cantidad.de.clientes)

names(nueva.base)

# Evaluando medidas de tendencia central y

# presentando datos en formato ancho

library(dplyr) library(tidyverse)

nueva.base %\>% group_by(ENTIDAD,NUMERO.DE.CUENTAS)%\>%
summarise(across(Saldo.total.disponible, list(maximo=max,
media=mean)))%\>% select(Saldo.total.disponible_maximo) %\>%
pivot_wider(names_from = Saldo.total.disponible_maximo, values_from
=c(Saldo.total.disponible_maximo),names_prefix =“ent\_”, values_fill =
0)

# Comparación entre entidades según vvariable creada “promedio por cuenta”

comparacion\<-nueva.base%\>% filter(ENTIDAD==“BP
BOLIVARIANO”\|ENTIDAD==“BP FINCA”)
