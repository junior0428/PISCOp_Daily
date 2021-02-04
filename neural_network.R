install.packages("neuralnet")
library(neuralnet)
library(readxl)

setwd("D:/HP I5 DORADA/DISCO C/UNIVERSIDAD/DECIMO CICLO/SIG APLICADO/DATOS PISCO-R/PISCO_DAILY")

#cargamos nuestros datos
entrenamiento<-read_excel("Neural Network/dato.xlsx", sheet = "Hoja1")
View(entrenamiento)
prueba<-read_excel("Neural Network/dato.xlsx", sheet = "Hoja2")
View(prueba)
#generando red neuronal 
nr<-neuralnet(cuadales~fecha+PP, data = entrenamiento, hidden = 20000, act.fct = "logistic", linear.output = F)

plot(nr)

#prediccion 
nrprediccion<-compute(nr, prueba)
  <-nrprediccion$net.result
