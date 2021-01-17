library(sp)
library(dplyr)
library(ncdf4)
library(readxl) #Excel
library(raster)

setwd("D:/HP I5 DORADA/DISCO C/UNIVERSIDAD/DECIMO CICLO/SIG APLICADO/DATOS PISCO-R/PISCO_DAILY")

#..load the folder or file
Longitud_Latitud <- read_excel("ENTRADA/Longitud_Latitud.xlsx")
View(Longitud_Latitud)

# ..load the dtad .nc
raster_pp<-raster::brick("Precipitation/PrecipDialy.nc")
View(raster_pp)

#...coordinate assignement
sp::coordinates(Longitud_Latitud)<- ~XX+YY

# match the projection of raster with point to extract

raster::projection(Longitud_Latitud)<- raster::projection(raster_pp)

#...Extract the values 
points_long_tati<- raster::extract(raster_pp[[1]], Longitud_Latitud, cellnumbers=T)[,1]
data_long_tati<-t(raster_pp[points_long_tati])
colnames(data_long_tati)<-as.character(Longitud_Latitud$NN) #assigment of subbasin colnames
View(data_long_tati)

#create my file cvs .....
?write.csv
write.csv(data_long_tati, "SALIDA/DailyRainfall.csv", quote = F)



