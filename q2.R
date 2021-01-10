library(tidyverse)
library(grid)
library(gridExtra)
library(car)
library(HSAUR2)
library(reshape2)

# Leitura dos dados
data("heptathlon")


################################################################################
############################# Analise Descritiva ###############################
################################################################################

# Variáveis sumárias
medados <-rbind(apply(heptathlon,2,mean),
                apply(heptathlon,2,var),
                apply(heptathlon,2,sd),
                100*apply(heptathlon,2,sd)/apply(heptathlon,2,mean),
                apply(heptathlon,2,min),
                apply(heptathlon,2,quantile,0.5),
                apply(heptathlon,2,max))

rownames(medados)<-c("Média","Var.","DP","CV(%)","Mínimo","Mediana","Máximo")

round(medados,2)


# Gráfico de dispersão
par(mfrow=c(1,1))
pairs(heptathlon,
      col = "#80009A",
      labels = c("100m com\n barreiras", "Salto em\n altura",
                 "Arremesso\n de peso", "200m\n rasos", 
                 "Salto em\n distância", "Lançamento\n de dardos",
                 "800m\n rasos", "Score"), cex.labels = 1.9,
      pch = 19)


# Matriz de correlações
cormat <- round(cor(heptathlon),2)
colnames(cormat) <- c("100m com\n barreiras", "Salto em\n altura",
                      "Arremesso\n de peso", "200m\n rasos", 
                      "Salto em\n distância", "Lançamento\n de dardos",
                      "800m\n rasos", "Score")
rownames(cormat) <- c("100m com\n barreiras", "Salto em\n altura",
                      "Arremesso\n de peso", "200m\n rasos", 
                      "Salto em\n distância", "Lançamento\n de dardos",
                      "800m\n rasos", "Score")

melted_cormat <- melt(cormat)

# Correlograma
ggplot(data = melted_cormat, aes(x=Var2, y=Var1, fill=value)) + 
  geom_tile()+ 
  scale_fill_distiller()+
  labs(x="", y="", fill="Correlação")+
  theme_bw()


# Histograma
par(mfrow=c(2,4),mar=c(4,4,2,2))
for(i in 1:8){
  hist(as.numeric(unlist(heptathlon[i])),probability=TRUE, col="#EE6060",
       main=colnames(cormat)[i],xlab="",ylab="")
}


# Boxplot
par(mfrow=c(2,4),mar=c(4,4,2,2))
for(i in 1:8){
  boxplot(as.numeric(unlist(heptathlon[i])),probability=TRUE,
       main=colnames(heptathlon)[i],xlab="",ylab="")
}

# Nomes
nomes <- c("100m com barreiras", "Salto em altura",
           "Arremesso de peso", "200m rasos", 
           "Salto em distância", "Lançamento de dardos",
           "800m rasos", "Score")

# Gráfico Q-Q
par(mfrow=c(2,4))
for(i in 1:8){
  qqPlot(scale(heptathlon[i]),dist="norm", id=F,
         mean=0,sd=1,col.lines="#80009A",pch = 19, col="#108A0C",
         xlab="",ylab=nomes[i])
}

################################################################################
################################# Modelagem ####################################
################################################################################