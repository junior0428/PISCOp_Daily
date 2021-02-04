library(readxl)

febrero1998 <- read_excel("D:/HP I5 DORADA/DISCO C/UNIVERSIDAD/DECIMO CICLO/SIG APLICADO/DATOS PISCO-R/PISCO_DAILY/SALIDA/febrero1998.xlsx")
View(febrero1998)
febrero1998<-febrero1998[-1]
View(febrero1998)
apply(febrero1998, 2, mean)

#....x = − ln[− ln[f(x)]] · α + µ 
#...α =Sx/Sn
#...µ = x(media) − (Y n ∗ α)
# x(gumbel)
Yn<-0.5410
Sn<-1.1313
xmedia<-apply(febrero1998, 2, mean)
Sx<-apply(febrero1998, 2, sd)
α<-Sx/Sn
µ<-xmedia-(Yn*α)

# Order the vector and data frame ..Example
install.packages("dplyr")
library(dplyr)
x<-c(5,6,2,1,7,10,3)
y<-c(10,50,90,41,62,12,54)
sort(x, decreasing=F) #order the vector x increasing
sort(y, decreasing = T) #order the vector y decreasing
m<-data.frame(x,y)
col<-colnames(m)
arrange(m, x)
sort(m, decreasing = T)
apply(m, 2, sort, decreasing=T)

#order the february1998 dataframe "Weibull"
Weibul<-apply(febrero1998, 2, sort, decreasing=T)
View(Weibul)

# generate data to f(x)
f<-c(1:28)
fde<-NULL
for (u in 1:28) {
  ff<-f[u]/29
  fde<-c(fde, ff)
}
fde
#to fdex
fdex<-sort(fde, decreasing = T)
#...generate gumbel to each subbasin
Gumbe<-data.frame() 
for (i in 1:18) {
  Gumbel_tem<-NULL
  for (j in 1:28) {
    gum<- -log(-log(fdex[j]))*α[i]+µ[i]
    Gumbel_tem<-c(Gumbel_tem, gum)
  }
  Gumbe[1:28, i]<-Gumbel_tem
}
View(Gumbe)

#plot of Weibull and Gumbel
nombre<-c("Weibull","Gumbel")
rm(Gumbel)
length(fdex)
length(Weibul)
plot(fdex, Weibul[,18], type="o", col="green",
     xlab="f(x)",
     ylab="Precipitaciones diarias (mm)",
     main="Distribución Weibull y Gumbel para la Sub 18",lwd = 2)
lines(fdex, Gumbe[,18], col="blue", type="o",lwd = 2)
legend("topleft", Weibull, nombre, cex = 0.6,
       col = c("green", "blue"), pch = 21:23, lty=1:3, lwd = 2)
length(α)
#Tiempo de retorno 
trs<-c(10, 30, 50, 100)
TiemRetorno<-data.frame()
for (jj in 1:4) {
  fx<-(1-(1/trs[jj]))
  tr_tem<-NULL
  for (ii in 1:18) {
    tr<- -log(-log(fx))*α[ii]+µ[ii]
    tr_tem<-c(tr_tem, tr)
  }
  TiemRetorno[1:18, jj]<-tr_tem
}

names(TiemRetorno)<-c("TR=10", "TR=30", "TR=50", "TR=100")
View(TiemRetorno)

write.csv(TiemRetorno, "Tiempo_Retorno.csv")

#export in graph 
Subcuenca<-c(colnames(febrero1998))
plot_colors<-c("green","#FFCC00", "red","blue")
max_y<-max(TiemRetorno)
plot(TiemRetorno$`TR=10`,type = "o",  ylim=c(10,max_y), axes=FALSE, ann=FALSE, col=plot_colors[1])
lines(TiemRetorno$`TR=30`, type = "o", col=plot_colors[2])
lines(TiemRetorno$`TR=50`, type = "o", col=plot_colors[3])
lines(TiemRetorno$`TR=100`, type = "o", col=plot_colors[4])
axis(1, at=1:18, lab=Subcuenca)
axis(2, las=1, at=4*0:max_y)

# Create a title with a red, bold/italic font
title(main="Tiempo de Retorno para las Subcuencas de la cuenca de rio Piura", font.main=4)

# Label the x and y axes with dark green text
title(xlab= "Centroide de las Subcuencas")
title(ylab= "Precipitaciones Maximas (mm)")

#Leyenda
legend("topright", max_y, names(TiemRetorno), cex=0.6, col=plot_colors,
       pch=21:23, lty=1:3)

