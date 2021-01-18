library(readxl)

febrero1998 <- read_excel("D:/HP I5 DORADA/DISCO C/UNIVERSIDAD/DECIMO CICLO/SIG APLICADO/DATOS PISCO-R/PISCO_DAILY/SALIDA/febrero1998.xlsx")
View(febrero1998)
febrero1998<-febrero1998[-1]
View(febrero1998)
apply(febrero1998, 2, mean)

#....x = − ln[− ln[f(x)]] · α + µ 
#...α =Sx/Sn
#...µ = x(media) − (Y n ∗ α)
Yn<-0.5410
Sn<-1.1313
xmedia<-apply(febrero1998, 2, mean)
Sx<-apply(febrero1998, 2, sd)
α<-Sx/Sn
µ<-xmedia-(Yn*α)

