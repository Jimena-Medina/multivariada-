
#______ Escalado multidimensional Métrico______

# Cargamos la matriz de datos eurodist

data.dist<-eurodist

# Transformamos los datos en matriz
data.dist<-as.matrix(data.dist)

#-----------------------------------
#  Extracción de las filas de la matriz

# Número de ciudades
n<-nrow(data.dist)

#------------------------------------
#  Escalado multidimensional clásico

# 1.- Cálculo de autovalores
# dentro del objeto mds.cities se encuentran
# almacenado los valores propios (eigenvalues) en
# mds.cities$eig

mds.cities<-cmdscale(data.dist, eig = TRUE)

# 2.- Generación del gráfico
plot(mds.cities$eig, pch=19, col="purple", 
     xlab="Números", ylab="Valores Propios",
     type="o")
abline(a=0, b=0, col="red4")

# Interpretacion: se identifican autovalores negativos
# Se considera como solución el seleccionar
# r=2 coordenadas principales.


# 3.- Medidas de precisión

m<-sum(abs(mds.cities$eig[1:2]))/sum(abs(mds.cities$eig))

#4.- Obtención de coordenadas principales fijando
# k=2 y se realice con los dos primeros autovalores.
mds.cities<-cmdscale(data.dist, eig=TRUE, k=2)

x1<-mds.cities$points[,1]
x2<-mds.cities$points[,2]

# 5.- Generación del gráfico en dos dimensiones de los
# datos con las coordenadas obtenidas
plot(x1,x2,pch=19, col="palegreen", 
     xlim = range(x1)+c(0,600))
text(x1,x2, pos=4, labels = rownames(data.dist),
     col="black")

# 6.-Se invierten los ejes del plot
x2<--x2

plot2<-plot(x1,x2,pch=19, col="powderblue", 
     xlim = range(x1)+c(0,600))
text(x1,x2, pos=4, labels = rownames(data.dist),
     col="black")
