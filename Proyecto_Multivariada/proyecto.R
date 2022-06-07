#-------------Proyecto-Dendograma--------------------
# Jimena Isaura Medina Padilla 
# Cargamos la base de datos 
library(readxl)
ruta_base <- "/Users/jimenamedina/Desktop/Netflix_data.xlsx"
BDN <- read_excel(ruta_base)
# Exploración de la matriz 
# Dimensión 
dim(BDN)
# Nombre de las variables 
colnames(BDN)
#Se verifica que no existan datos perdidos
anyNA(BDN)
#Tipo de variables 
str(BDN)
#Previsualización de los primeros 6 datos de la matriz 
head(BDN)

# Calculo de la matriz de distancia de Mahalonobis (primeras 15 observaciones)
dist.BDN<-dist(BDN[,2:6])
head(dist.BDN, 15)
# Convertir los resultados del cálculo de la distancia a una matriz de datos y que indique 3 digitos.
round(as.matrix(dist.BDN)[1:6, 1:6],3)

# Cálculo del dendrograma
dend.BDN<-as.dendrogram(hclust(dist.BDN))
dend.BDN
# Cargamos librerias
installed.packages("dendextend")
library("dendextend")
# Se realiza un dendograma exploratorio con la función hclust
DN <- BDN[2:6]
N<- dist(DN) 
Nf <- hclust(N)
Nf
# Gráfico exploratorio del dendograma 
plot(Nf)
rect.hclust(Nf, k = 6,
            border = 1:6)
# Generación del dendrograma con etiquetas 
#Guardar las etiquetas en un objeto "P"
P=labels(dend.BDN)
labels(dend.BDN)=BDN$Country[P]
# Color de las etiquetas 
dend.BDN %>%
  set(what="labels_col","red4") %>%
  # Tamaño de las etiquetas 
  set(what="labels_cex",0.9) %>%
  plot(main= "Dendograma Países")

# Dendograma circular 
library("circlize")

circlize_dendrogram(dend.BDN,labels_track_height=NA,
                    dend_track_height=0.1)


        

