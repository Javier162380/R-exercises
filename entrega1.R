#Entrega 1.
#Estrucutura de datos.
#Ejercicio 1.
##Genere las siguientes secuencias en R:
###1,2,3,1,2,3,1,2,3,1,2,3
rep(seq(1,3,by=1),3)
###10.00000 10.04545 10.09091 10.13636 10.18182 10.22727 10.27273 10.31818 10.36364 10.40909 10.45455 10.50000 
as.vector(rbind(seq(10.00000,10.45455,by=.09091),seq(10.04545,10.50000,by=0.09091)))
### "1" "2" "3" "banana" "1" "2" "3" "banana"
rep(c("1","2","3","banana"),2)


#Ejercicio 2.
##Utilizar el dataframe llamado “airquality” de la librería R “datasets” para responder a las siguientes cuestiones: 
airquality->datasets::airquality
###Cuantos campos y observaciones tiene el dataframe. Utilizar “head” y “dim”. 
dim(airquality)[2] #campos
dim(airquality)[1] #observaciones
###Evaluar el dataframe.elementos nulos.A que meses corresponden las observaciones.
summary(airquality) #presentan NAN las columnas ozono y solar.
unique(airquality["Month"]) #meses de mayo,junio,julio,agosto,septiembre.
head(airquality)
###Temperatura máxima del viento en el mes de mayo.
max((subset.data.frame(airquality,airquality["Month"]==5)[,"Temp"]))
###Media del ozono en el mes de Julio.
mean((subset.data.frame(airquality,airquality["Month"]==6)[,"Temp"]))
###Mes donde la temperatura fue mayor.
data=aggregate(airquality[,"Temp"],list(airquality[,"Month"]),mean)#el mes es agosto
head(arrange(data,desc(data$x)),n=1)
###Número de observaciones donde la temperatura fue > 90 y el ozono < 100 filtrando por mes
filterdata=subset.data.frame(airquality,airquality["Temp"]>90&airquality[,"Ozone"]<100) #es el mes de septiembre
###Haciendo un estudio de los datos, ¿Qué podemos concluir? ¿Existe alguna relación
###entre las variables Ozono, Temperatura y Radiación Solar? Se recomienda hacer la
###media mes a mes de cada variable
tempovermonth=(aggregate(airquality[,"Temp"],list(airquality[,"Month"]),mean))
Ozonoovermonth=(aggregate(airquality[,"Ozone"],list(airquality[,"Month"]),mean))
Solarrmonth=(aggregate(airquality[,"Solar.R"],list(airquality[,"Month"]),mean))
result=merge(merge(tempovermonth,Ozonoovermonth,by=c("Group.1","Group.1")),Solarrmonth,by=c("Group.1","Group.1")) #existe una correlacion postiva entre las variables

#Ejercicio 3.
###Construir la función de Fibonacci en R permitiendo introducir el valor máximo que no debe
###ser superado. Por defecto se debe indicar que este valor es 1000.
Fibonacci <- function(n) {
  x <- c(0,1)
  while (length(x) < 1000) {
    position <- length(x)
    new <- x[position] + x[position-1]
    x <- c(x,new)
  }
  return(x)
}
