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
##Print medados
medados


# Gráfico de dispersão
par(mfrow=c(1,1))
pairs(heptathlon,
      col = "#80009A",
      pch = 19)


# Matriz de correlações
cormat <- round(cor(heptathlon),2)
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
  hist(as.numeric(unlist(heptathlon[i])),probability=TRUE, col="#80009A",
       main=colnames(heptathlon)[i],xlab="",ylab="")
}


# Boxplot
par(mfrow=c(2,4),mar=c(4,4,2,2))
for(i in 1:8){
  boxplot(as.numeric(unlist(heptathlon[i])),probability=TRUE,
       main=colnames(heptathlon)[i],xlab="",ylab="")
}


# Gráfico Q-Q
par(mfrow=c(2,4))
for(i in 1:8){
  qqPlot(scale(heptathlon[i]),dist="norm", id=F,
         mean=0,sd=1,col.lines="#80009A",pch = 19, col="#108A0C",
         xlab="Quantil da N(0,1)",ylab=colnames(heptathlon)[i])
}

################################################################################
################################# Modelagem ####################################
################################################################################