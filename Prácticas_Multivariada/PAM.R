
#_______ PARTITION AROUND MEDOIDS (PAM)_____

library(cluster)

# Cargar la matriz de datos.

X<-as.data.frame(state.x77)
colnames(X)

#-------------------------------------
#     Transformación de datos

#1.- Transformación de las variables x1,x3 y x8
# con la función de logaritmo.

X[,1]<-log(X[,1])
colnames(X)[1]<-"Log-Population"

X[,3]<-log(X[,3])
colnames(X)[3]<-"Log-Illiteracy"

X[,8]<-log(X[,8])
colnames(X)[8]<-"Log-Area"

#---------------------------------
#    Método PAM

#1.- Separación de filas y columnas.

dim(X)
n<-dim(X)[1]
p<-dim(X)[2]

# 2.- Estandarización univariante.
X.s<-scale(X)

# 3.- Aplicación del algoritmo
pam.3<-pam(X.s,3)

# 4.- Clusters
cl.pam<-pam.3$clustering
cl.pam

#5.- Scatter plot de la matriz con los grupos
col.cluster<-c("blue","pink","cyan")[cl.pam]
pairs(X.s, col=col.cluster, main="PAM", pch=19)

#---------------------------------
#  Visualización con Componentes Principales

clusplot(X.s,cl.pam)
text(princomp(X.s)$scores[,1:2],
     labels=rownames(X.s),pos=1, col="purple")

#-------------------------------------
#   Silhouette

# Representación grafica de la eficacia de
# clasificación de una observación dentro de un
# grupo.

# 1.- Generación de los cálculos
dist.Euc<-dist(X.s, method = "euclidean")
Sil.pam<-silhouette(cl.pam, dist.Euc)

#2.- Generación del gráfico
plot(Sil.pam, main="Silhouette for PAM", 
     col="violet")
