library(tidyverse)
library(lubridate)

setwd("D:/HP I5 DORADA/DISCO C/UNIVERSIDAD/DECIMO CICLO/SIG APLICADO/DATOS PISCO-R/PISCO_DAILY")
as.numeric(data_long_tati)
plot(as.numeric(data_long_tati))

colnames(data_long_tati)

plot(as.numeric(data_long_tati), 
     type="o",
     col="#1B9E77",
     lwd=3,
     xlab="Numero de datos",
     ylab="Precipitaciones diarias (mm)")

length(as.numeric(data_long_tati)) #numbers of data 

subbasin<-(colnames(data_long_tati))
view(data_long_tati)
subbasin[1]
for (i in 1:18) {
  title<- paste("Precipitación de la", subbasin[i])
  save_png<-paste0("SALIDA/Imagenes/", subbasin[i], ".jpg")
  data_tmp<-ts(data_long_tati[,i], start = 1981, frequency = 365)
  png(filename = save_png, width = 1200, units = "px")
  plot(data_tmp,
       xlab="Año (1981 al 2016)", 
       ylab="Precipitaciones Diarias (mm)",
       type="S",
       col="#1B9E77",
       lwd="2.5",
       main=title,
       sub="Fuente: Datos Diarios PISCOp SENAMHI")
  dev.off()
}

rm(points_long_tati)
rm(save_png)
rm(title)
rm(subbasin)
rm(i)
rm(data_tmp)
