#------------K-VECINOS MÁS CERCANOS-----------------
#1.-Instalamos la paquetería
install.packages("MASS")
library(MASS)
#2.-Cargar los datos iris
Z<-as.data.frame(iris)
colnames(Z)
#3.- Definir la matriz de datos y la variable respuesta con las clasificaciones
x<-Z[,1:4]
y<-Z[,5]
#4.-Se definen las variables y observaciones
n<-nrow(x)
p<-ncol(x)
#Grafico scatter plot
#1.- Creación de un vector de colores
y
col.iris<-c("purple","green","magenta")[y]
col.iris

pairs(x, main="Data set Iris, Setosa (morado),Versicolor (verde), Virginica (rosa)", 
      pch=19,col=col.iris)
-------------------------------------------------------------------------------
#kNN
#1.-Se llama a la paqueteria 
library(class)
#2.-Se fija una "semilla" para tener valores iguales
set.seed(1000)
#Creacion de los ciclos para k=1 hasta k=20
#Selecciona el valor de k que tenga el error más bajo.

#1.-Inicialización de una lista vacia de tamaño 20
knn.class<-vector(mode="list",length=20)
knn.tables<-vector(mode="list", length=20)
#2.-Clasificaciones erroneas
knn.mis<-matrix(NA, nrow=20, ncol=1)
knn.mis

for(k in 1:20){
  knn.class[[k]]<-knn.cv(x,y,k=k)
  knn.tables[[k]]<-table(y,knn.class[[k]])
  # la suma de las clasificaciones menos las correctas
  knn.mis[k]<- n-sum(y==knn.class[[k]])
}

knn.mis
#3.-Núumero óptimo de k-vecinos
pairs(C, col="pink", pch=19, main="matriz original")
#Se señala el k mas eficiente
#El mas eficiente es k=14
k.opt<-14

knn.cv.opt<-knn.class[[k.opt]]
knn.cv.opt
#Tabla de contingencia con las clasificaciones buenas y malas
knn.tables[[k.opt]]

# Cantidad de observaciones mal clasificadas
knn.mis[k.opt]

# Error de clasificacion (MR)
knn.mis[k.opt]/n

# Grafico de clasificaciones correctas y erroneas
col.knn.iris<-c("indianred1","black")[1*(y==knn.cv.opt)+1]
pairs(x, main="Clasificacion kNN de Iris",
      pch=19, col=col.knn.iris)




