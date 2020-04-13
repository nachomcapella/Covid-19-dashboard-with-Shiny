data_2 <- read.csv(file = "./data/ccaa_covid19_casos.csv", header = T)
data_2<-data_2[1:18,3:49]
data_2<-t(data_2)
names <- c("Andalucia", "Asturias","Baleares","Canarias","Cantabria","CastillaLaMancha","CastillaYLeon","Cataluna","Ceuta","CValenciana","Extremadura","Galicia","Madrid","Melilla","Murcia","Navarra","PaisVasco","LaRioja")
colnames(data_2)<-names
data_2<-as.data.frame(rbind(rep(0,18), rep(0,18), data_2))
fecha<-data$fecha
data_2<-as.data.frame(cbind(fecha,data_2))




