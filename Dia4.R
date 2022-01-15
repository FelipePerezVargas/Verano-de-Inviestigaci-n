library(readr)  # Cargar paquetes
library(corrplot)
library(Hmisc)
library(PerformanceAnalytics)
library(psych)
library(ggplot2)
library(plotly)
library(heatmaply)
library(ggridges)
library(gridExtra)
library(corrgram)
graphics.off() #Borramos los gráficos anteriores
rm(list = ls()) #Borramos todas las variables anteriores
bd2 <- read_csv("C:/Users/Admin/Desktop/Semestre_Enero_Julio_2021/Verano/BD2.csv")
ds1<-data.frame(bd2$Name, bd2$Age,bd2$Gender, bd2$Weight, bd2$Height,
                bd2$BMI, bd2$Fat_Percent, bd2$`Mass Muscle`, bd2$Waist, bd2$Hip,
                bd2$Neck, bd2$SBP, bd2$DBP, bd2$TA, bd2$`Handgrip streght`)
#Modificamos los nombres
colnames(ds1)<-c('Nombre','Edad', 'Genero', 'Peso','Altura','BMI','Grasa',
                 'Masa Muscular','Cintura','Hip','Cuello','SBP', 'DBP', 'TA',
                 'Agarre')

ds2<-textshape::column_to_rownames(ds1, loc = 1)


describe(ds2)
summary(ds2)
library(stats)
ds2<-ds2[,-2]

respca<-prcomp(ds2, scale = TRUE) #Resultados del PCA

respca$center
mean(ds2$Edad)
respca$x
dim(respca$rotation) #Número de distintos componentes
head(respca$x)[,1:5] #los vectores de los scores.
respca$sdev #las desviaciones estándares de cada CP.
respca$sdev^2  ## Varianza explicada por cada componente
summary(respca)
#comprobemos la importancia del componente 1
xx<-respca$x
xx<-as.data.frame(xx)
ds2$PC1<-xx$PC1
ds2$PC2<-xx$PC2
cor(ds2)
#otra función
names(ds2)
princomp(ds2[,-c(14,15)],cor = TRUE) 
#Ahora podemos hacer ACP con solo unas cuantas variables
respca1 <- princomp(~ Edad + Peso+Hip,
                    data = ds2, na.action = na.exclude, cor = TRUE)

names(respca1)
respca1$sdev
respca1$loadings
respca1$scores
respca1$n.obs
respca1$scale
respca1$center
respca1$call
summary(respca1)


library(FactoMineR)

#PCA() #PCA con resultados más detallados. Los valores ausentes se reemplazan por la media de cada columna.
#Pueden incluirse variables categóricas suplementarias. Estandariza automáticamente los datos.
# Otro método sofisticado

respca2 <- PCA(X = ds2, scale.unit = TRUE, ncp = 3, graph = TRUE)
print(respca2)
head(respca2$eig) #como ejemplo
cor(ds2)
respca2$var

library(factoextra)
get_pca(respca2) #Extrae la información sobre las variables.
get_pca_var(respca2) #Extrae la información sobre las variables.
get_pca_ind(respca2) #Extrae la información sobre las observaciones.
respca$scale
respca2$var
respca2$ind
respca2$svd
respca$x
#visualización
fviz_eig(respca2) #visualizar eigenvalores (scree plot)
fviz_screeplot(respca2) #visualizar eigenvalores (scree plot)
fviz_pca_ind(respca2) #Representación de observaciones sobre componentes principales.
fviz_pca_ind(respca2,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE)     # Avoid text overlapping


fviz_pca_var(respca2) #Representación de variables sobre componentes principales.

fviz_pca_var(respca2,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)



fviz_pca_var(respca2, col.var = "cos2", 
             geom.var = "arrow", 
             labelsize = 2, 
             repel = FALSE)
#Ejemplos de como visualizar aquí:
#http://www.sthda.com/english/wiki/fviz-pca-quick-principal-component-analysis-data-visualization-r-software-and-data-mining
#Grupos por color individual
fviz_pca_ind(respca2, label="none", habillage=bd2$Alcoholismo)
p <- fviz_pca_ind(respca2, label="none", habillage=bd2$`clas BMI`,
                  addEllipses=TRUE, ellipse.level=0.95)
print(p)
fviz_contrib(respca2,choice = "var") #Representa la contribución de filas/columnas de los resultados de un pca.
fviz_contrib(respca2,choice = "ind")


biplot(x = respca1, scale = 0, cex = 0.6, col = c("blue4", "brown3"))

fviz_pca_biplot(respca1, repel = FALSE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

fviz_pca_biplot(respca1, repel = TRUE,
                #geom= "point", #o text o geom=c("point", "text")
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969", # Individuals color
                select.ind = list(contrib = 10)
)

grupo<-as.factor(ds1$Genero)
grupo1<-as.factor(bd2$Alcoholismo)

#función contraida####
ggbiplot <- function(pcobj, choices = 1:2, scale = 1, pc.biplot = TRUE, 
                     obs.scale = 1 - scale, var.scale = scale, 
                     groups = NULL, ellipse = FALSE, ellipse.prob = 0.68, 
                     labels = NULL, labels.size = 3, alpha = 1, 
                     var.axes = TRUE, 
                     circle = FALSE, circle.prob = 0.69, 
                     varname.size = 3, varname.adjust = 1.5, 
                     varname.abbrev = FALSE, ...)
{
  library(ggplot2)
  library(plyr)
  library(scales)
  library(grid)
  
  stopifnot(length(choices) == 2)
  
  # Recover the SVD
  if(inherits(pcobj, 'prcomp')){
    nobs.factor <- sqrt(nrow(pcobj$x) - 1)
    d <- pcobj$sdev
    u <- sweep(pcobj$x, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- pcobj$rotation
  } else if(inherits(pcobj, 'princomp')) {
    nobs.factor <- sqrt(pcobj$n.obs)
    d <- pcobj$sdev
    u <- sweep(pcobj$scores, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- pcobj$loadings
  } else if(inherits(pcobj, 'PCA')) {
    nobs.factor <- sqrt(nrow(pcobj$call$X))
    d <- unlist(sqrt(pcobj$eig)[1])
    u <- sweep(pcobj$ind$coord, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- sweep(pcobj$var$coord,2,sqrt(pcobj$eig[1:ncol(pcobj$var$coord),1]),FUN="/")
  } else if(inherits(pcobj, "lda")) {
    nobs.factor <- sqrt(pcobj$N)
    d <- pcobj$svd
    u <- predict(pcobj)$x/nobs.factor
    v <- pcobj$scaling
    d.total <- sum(d^2)
  } else {
    stop('Expected a object of class prcomp, princomp, PCA, or lda')
  }
  
  # Scores
  choices <- pmin(choices, ncol(u))
  df.u <- as.data.frame(sweep(u[,choices], 2, d[choices]^obs.scale, FUN='*'))
  
  # Directions
  v <- sweep(v, 2, d^var.scale, FUN='*')
  df.v <- as.data.frame(v[, choices])
  
  names(df.u) <- c('xvar', 'yvar')
  names(df.v) <- names(df.u)
  
  if(pc.biplot) {
    df.u <- df.u * nobs.factor
  }
  
  # Scale the radius of the correlation circle so that it corresponds to 
  # a data ellipse for the standardized PC scores
  r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4)
  
  # Scale directions
  v.scale <- rowSums(v^2)
  df.v <- r * df.v / sqrt(max(v.scale))
  
  # Change the labels for the axes
  if(obs.scale == 0) {
    u.axis.labs <- paste('standardized PC', choices, sep='')
  } else {
    u.axis.labs <- paste('PC', choices, sep='')
  }
  
  # Append the proportion of explained variance to the axis labels
  u.axis.labs <- paste(u.axis.labs, 
                       sprintf('(%0.1f%% explained var.)', 
                               100 * pcobj$sdev[choices]^2/sum(pcobj$sdev^2)))
  
  # Score Labels
  if(!is.null(labels)) {
    df.u$labels <- labels
  }
  
  # Grouping variable
  if(!is.null(groups)) {
    df.u$groups <- groups
  }
  
  # Variable Names
  if(varname.abbrev) {
    df.v$varname <- abbreviate(rownames(v))
  } else {
    df.v$varname <- rownames(v)
  }
  
  # Variables for text label placement
  df.v$angle <- with(df.v, (180/pi) * atan(yvar / xvar))
  df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar)) / 2)
  
  # Base plot
  g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + 
    xlab(u.axis.labs[1]) + ylab(u.axis.labs[2]) + coord_equal()
  
  if(var.axes) {
    # Draw circle
    if(circle) 
    {
      theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
      circle <- data.frame(xvar = r * cos(theta), yvar = r * sin(theta))
      g <- g + geom_path(data = circle, color = muted('white'), 
                         size = 1/2, alpha = 1/3)
    }
    
    # Draw directions
    g <- g +
      geom_segment(data = df.v,
                   aes(x = 0, y = 0, xend = xvar, yend = yvar),
                   arrow = arrow(length = unit(1/2, 'picas')), 
                   color = muted('red'))
  }
  
  # Draw either labels or points
  if(!is.null(df.u$labels)) {
    if(!is.null(df.u$groups)) {
      g <- g + geom_text(aes(label = labels, color = groups), 
                         size = labels.size)
    } else {
      g <- g + geom_text(aes(label = labels), size = labels.size)      
    }
  } else {
    if(!is.null(df.u$groups)) {
      g <- g + geom_point(aes(color = groups), alpha = alpha)
    } else {
      g <- g + geom_point(alpha = alpha)      
    }
  }
  
  # Overlay a concentration ellipse if there are groups
  if(!is.null(df.u$groups) && ellipse) {
    theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
    circle <- cbind(cos(theta), sin(theta))
    
    ell <- ddply(df.u, 'groups', function(x) {
      if(nrow(x) <= 2) {
        return(NULL)
      }
      sigma <- var(cbind(x$xvar, x$yvar))
      mu <- c(mean(x$xvar), mean(x$yvar))
      ed <- sqrt(qchisq(ellipse.prob, df = 2))
      data.frame(sweep(circle %*% chol(sigma) * ed, 2, mu, FUN = '+'), 
                 groups = x$groups[1])
    })
    names(ell)[1:2] <- c('xvar', 'yvar')
    g <- g + geom_path(data = ell, aes(color = groups, group = groups))
  }
  
  # Label the variable axes
  if(var.axes) {
    g <- g + 
      geom_text(data = df.v, 
                aes(label = varname, x = xvar, y = yvar, 
                    angle = angle, hjust = hjust), 
                color = 'darkred', size = varname.size)
  }
  # Change the name of the legend for groups
  # if(!is.null(groups)) {
  #   g <- g + scale_color_brewer(name = deparse(substitute(groups)), 
  #                               palette = 'Dark2')
  # }
  
  # TODO: Add a second set of axes
  
  return(g)
}


######
grupo<-as.factor(ds1$Genero)
grupo1<-as.factor(bd2$Alcoholismo)
grupo2<-as.factor(bd2$`clas BMI`)
grupo3<-as.factor(bd2$Exercise)
ggbiplot(respca2)
ggbiplot(respca2,ellipse=TRUE,  labels=rownames(ds2), groups=grupo3)

ggbiplot(respca2, ellipse=TRUE, choices=c(3,4), labels=rownames(ds2), groups=grupo)


fviz_pca_ind(respca2,
             col.ind = grupo, # color por grupos
             palette = c("#00AFBB", "#FC4E07","#696969"),
             addEllipses = TRUE, # elipses de Concentración 
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = FALSE
)

fviz_pca_biplot(respca1,
                col.ind = grupo, # color por grupos
                palette = c("#00AFBB", "#FC4E07","#696969"),
                addEllipses = TRUE, # elipses de Concentración 
                ellipse.type = "confidence",
                legend.title = "Groups",
                repel = TRUE
)


#Clustering

#Cargar y utilizar función IPAK
#ver vídeo https://www.youtube.com/watch?v=UjQz9SxG9rk

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse","cluster", "factoextra","NbClust","tidyr")
ipak(packages)



#normalizar las puntuaciones
df <- scale(ds2)
df
head(df)

#calcular la matriz de distacias
m.distancia <- get_dist(df, method = "euclidean") #el método aceptado también puede ser: "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman" o "kendall"
fviz_dist(m.distancia, gradient = list(low = "blue", mid = "white", high = "red"))

#estimar el número de clústers
#Elbow, silhouette o gap_stat  method
fviz_nbclust(df, kmeans, method = "wss")
fviz_nbclust(df, kmeans, method = "silhouette")
fviz_nbclust(df, kmeans, method = "gap_stat")

#con esta función se pueden calcular:
#the index to be calculated. This should be one of : "kl", "ch", "hartigan", "ccc", "scott",
#"marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda",
#"pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", "gamma",
#"gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw", "all" (all indices except GAP,
#Gamma, Gplus and Tau), "alllong" (all indices with Gap, Gamma, Gplus and Tau included).
resnumclust<-NbClust(df, distance = "euclidean", min.nc=2, max.nc=10, method = "kmeans", index = "alllong")
fviz_nbclust(resnumclust)
NbC
#calculamos los dos clústers
k2 <- kmeans(df, centers = 2, nstart = 25)
k2
str(k2)

#plotear los cluster
fviz_cluster(k2, data = df)
fviz_cluster(k2, data = df, ellipse.type = "euclid",repel = TRUE,star.plot = TRUE) #ellipse.type= "t", "norm", "euclid"
fviz_cluster(k2, data = df, ellipse.type = "norm")
fviz_cluster(k2, data = df, ellipse.type = "norm",palette = "Set2", ggtheme = theme_minimal())



res2 <- hcut(df, k = 2, stand = TRUE)
fviz_dend(res2, rect = TRUE, cex = 0.5,
          k_colors = c("red","#2E9FDF"))

res4 <- hcut(df, k = 4, stand = TRUE)
fviz_dend(res4, rect = TRUE, cex = 0.5,
          k_colors = c("red","#2E9FDF","green","black"))


#pasar los cluster a mi df inicial para trabajar con ellos

ds2 %>%
  mutate(Cluster = k2$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

df <- ds2
df
df$clus<-as.factor(k2$cluster)
df

df <- ds2
df <- scale(df)
df<- as.data.frame(df)
df$clus<-as.factor(k2$cluster)
df

df$clus<-factor(df$clus)
names(ds2)
data_long <- gather(df, caracteristica, valor,Edad:Agarre, factor_key=TRUE)
data_long

ggplot(data_long, aes(as.factor(x = caracteristica), y = valor,group=clus, colour = clus)) + 
  stat_summary(fun = mean, geom="pointrange", size = 1)+
  stat_summary(geom="line")
+geom_point(aes(shape=clus))

