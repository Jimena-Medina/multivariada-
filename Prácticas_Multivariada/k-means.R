
#__________ K-MEANS____________

# Cargar la matriz de datos.

X<-as.data.frame(state.x77)

#     Transformación de datos

#1.- Transformación de las variables x1,x3 y x8
# con la función de logaritmo.

X[,1]<-log(X[,1])
colnames(X)[1]<-"Log-Population"

X[,3]<-log(X[,3])
colnames(X)[3]<-"Log-Illiteracy"

X[,8]<-log(X[,8])
colnames(X)[8]<-"Log-Area"

#    Metodo k-means

#1.- Separación de filas y columnas.

dim(X)
n<-dim(X)[1]
p<-dim(X[2])
View(X)

# 2.- Estandarización univariante.
X.s<-scale(X)

# 3.- Algoritmo k-medias (3 grupos) cantidad de subconjuntos aleatorios que se escogen
# para realizar los calculos de algoritmo.
Kmeans.3<-kmeans(X.s, 3, nstart=25)

# Centroides
Kmeans.3$centers

# Cluster de pertenencia
Kmeans.3$cluster


# 4.- SCDG
SCDG<-sum(Kmeans.3$withinss)
SCDG

# 5.- Clusters
cl.kmeans<-Kmeans.3$cluster
cl.kmeans

# 6.- Scatter plot con la division de grupos
# obtenidos (se utiliza la matriz de datos centrados).
col.cluster<-c("yellow", "orange", "green")[cl.kmeans]
pairs(X.s, col=col.cluster, main="k-means", pch=19)
#-----------------------------------------
#  Visualización con las dos componentes principales

library(cluster) 

clusplot(X.s, cl.kmeans, 
         main="Dos primeras componentes principales")

text(princomp(X.s)$score[,1:2],
     labels=rownames(X.s), pos=1, col="red")
#--------------------------------------
#  Silhouette
# Representación grafica de la eficacia de
# clasificación de una observacion dentro de un
# grupo.

# 1.- Generación de los calculos
dist.Euc<-dist(X.s, method = "euclidean")
Sil.kmeans<-silhouette(cl.kmeans, dist.Euc)

#2.- Generación del gráfico
plot(Sil.kmeans, main="Silhouette for k-means", 
col="slateblue1")
