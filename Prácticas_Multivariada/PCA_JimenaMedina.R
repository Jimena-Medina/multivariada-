#### CODIGO PCA######
# Análisis de componentes principales

## Selección de los datos para la realizaci?n del ejemplo

#1-. Se seleccion? la base llamada *"flores"*, datos extraidos de la paqueteria *"datos"*
install.packages("datos")
library(datos)
x <- (flores)

## Exploración de la matriz 

#1-. Dimensión de la matriz
dim(x)

#2-. Exploración de las variables 
str(x)

#3-. Nombre de las variables 
colnames(x)

#4-.Se verifica que no existan datos perdidos 
anyNA(x)
#Creación de un nuevo data frame solo con las variables cuantitativas
#Instalación de un paquete para la manipulación de dataframes
install.packages("dplyr") 
library(dplyr)
#1-. Creacion de un nuevo data frame 
datos2 <- select(flores, Largo.Sepalo, Ancho.Sepalo, Largo.Petalo, Ancho.Petalo)
x<-as.data.frame(datos2)

## Tratamiento de matriz
#se genera una nueva matriz **x1** con los datos de solo una especie, en este caso la especie "Versicolor"..

#1 Seleccion de las nuevas variables.
x1 <- x[51:100,1:4]
x1 <- x[51:100,1:4]

# ACP paso a paso 
#1 se transforma la nueva matriz a un data frame 

x1 <- as.data.frame(x1)

#2- Definir _n_ (individuos) y _p_ (variables)
n<-dim(x)[1]
p<-dim(x)[2]

#3- Generar gráfico 

pairs(x1,col="chartreuse", pch=19, 
      main="Variables originales")

#4.- Obtención de la media por columna  

mu<-colMeans(x1)
mu
#5-.La matriz de covarianza muestral
s<-cov(x1)
s
#6-. Obtención de los *valores* y *vectores* propios desde la matriz de covarianza muestral 

es<-eigen(s)
es
#6.1-. Se separa la matriz de valores propios

eigen.val<-es$values
eigen.val

#6.2-. Separación de matrices de vectores propios
eigen.vec<-es$vectors
eigen.vec

#7- Calcular la proporción de la variabilidad

#7.1- Para la matriz de valores propios
pro.var<-eigen.val/sum(eigen.val)
pro.var

#7.2- variabilidad acumulada

pro.var.acum<-cumsum(eigen.val)/sum(eigen.val)
pro.var.acum
#8-. obtención de la matriz de correlaciones

R<-cor(x1)
R

#9-. Obtención de los valores y vectores propios a partir de la **matriz de correlaciones**
 
eR<-eigen(R)
eR


#10-. Separación de la matriz de valores propios 

#10.1- Separación de la matriz de valores propios 

eigen.val.R<-eR$values
eigen.val.R

#10.2- Separación de matrices de vectores propios
eigen.vec.R<-eR$vectors
eigen.vec.R

#11- Cálculo de la proporción de variabilidad 
#11.1- Para la matriz de valores propios
pro.var.R<-eigen.val/sum(eigen.val)
pro.var.R

#11.2-  Acumulada

pro.var.acum.R<-cumsum(eigen.val)/sum(eigen.val)
pro.var.acum.R

##Una vez observados los valores anteriores, podemos seleccionar los primeros 2 valores, ya que cumplen con el criterio del 80% 

#12-. Calcular la media de los valores propios

mean(eigen.val.R)


##Obtencion de coeficientes 

#13-. Centrar los datos con respecto a la media 

#13.1 Construcción de la matriz centrada 

ones<-matrix(rep(1,n),nrow=n, ncol=1)


#13.2 Construcción de la matriz centrada

X.cen<-as.matrix(x1-ones%*%mu)


#14-.Construcción de la matriz diagonal de las covarianzas. 

Dx<-diag(diag(s))
Dx

#15-. Construcción de la matriz centrada multiplicada por Dx^1/2

Y<-X.cen%*%solve(Dx)^(1/2)

#16-. Conatrucción de los coeficientes o scores eigen.vec.R
scores<-Y%*%eigen.vec.R
scores[1:10]

#17-. Se nombran las columnas 
colnames(scores)<-c("PC1","PC2","PC3","PC4")

#18-. Visualización de los scores
scores[1:10]

#19-. Gráfico de los scores 
pairs(scores, main = "Scores", col = "chartreuse", pch = 19 )

# VIa sintetizada 

##A continuación se presenta la via rápida para la visualizaci?n de los componentes principales y el screeplot.

#1-. Aplicar el cálculo de la varianza a las columnas  *1=filas, 2=columnas*
apply(x, 2, var)

#2-. Centrado por la media y escalada porla desviacion standar (dividir entre sd).
acp<-prcomp(x1, center=TRUE, scale=TRUE)
acp

#3- Generación del gráfico screeplot
plot(acp, type="l")


#En el grafico se muetra como solo toma un componente principal. 