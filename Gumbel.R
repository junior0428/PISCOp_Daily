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
Weibull<-apply(febrero1998, 2, sort, decreasing=T)
View(Weibull)

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
Gumbel<-data.frame() 
for (i in 1:18) {
  Gumbel_tem<-NULL
  for (j in 1:28) {
    gum<- -log(-log(fdex[j]))*α[i]+µ[i]
    Gumbel_tem<-c(Gumbel_tem, gum)
  }
  Gumbel[1:28, i]<-Gumbel_tem
}
View(Gumbel)

#plot of Weibull and Gumbel
nu(fdex)
length(fdex)
length(Weibull)
plot(fdex, Weibull[,18], type="o")
lines(fdex, Gumbel[,18], col="blue", type="o")
legend("topleft")
?legend


