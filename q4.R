library(tidyverse)
library(grid)
library(gridExtra)
library(car)
library(mlbench)
library(reshape2)

# Carregar os dados
data(PimaIndiansDiabetes)

# Substituindo os valores iguais a 0 por NA
PimaIndiansDiabetes[PimaIndiansDiabetes == 0] = NA

# Removendo os NA
pima <- PimaIndiansDiabetes %>% drop_na()


################################################################################
############################# Analise Descritiva ###############################
################################################################################

# Variáveis sumárias
medados <- rbind(apply(pima[-9],2,mean),
                 apply(pima[-9],2,var),
                 apply(pima[-9],2,sd),
                 100*apply(pima[-9],2,sd)/apply(pima[-9],2,mean),
                 apply(pima[-9],2,min),
                 apply(pima[-9],2,quantile,0.5),
                 apply(pima[-9],2,max))

rownames(medados)<-c("Média","Var.","DP","CV(%)","Mínimo","Mediana","Máximo")

medados

# Sumarias do grupo negativo
rbind(apply(pima[pima[9]=="neg",-9],2,mean),
      apply(pima[pima[9]=="neg",-9],2,var),
      apply(pima[pima[9]=="neg",-9],2,sd),
      100*apply(pima[pima[9]=="neg",-9],2,sd)/
        apply(pima[pima[9]=="neg",-9],2,mean),
      apply(pima[pima[9]=="neg",-9],2,min),
      apply(pima[pima[9]=="neg",-9],2,quantile,0.5),
      apply(pima[pima[9]=="neg",-9],2,max))

#Sumarias do grupo positivo
rbind(apply(pima[pima[9]=="pos",-9],2,mean),
      apply(pima[pima[9]=="pos",-9],2,var),
      apply(pima[pima[9]=="pos",-9],2,sd),
      100*apply(pima[pima[9]=="pos",-9],2,sd)/
        apply(pima[pima[9]=="pos",-9],2,mean),
      apply(pima[pima[9]=="pos",-9],2,min),
      apply(pima[pima[9]=="pos",-9],2,quantile,0.5),
      apply(pima[pima[9]=="pos",-9],2,max))


# Gráfico de dispersão
par(mfrow=c(1,1))
pairs(pima[-9],
      col = c("#4DB620", "#80009A")[pima$diabetes],   # Mudar cor por grupo
      pch = c("\u25CF", "\u25B2")[pima$diabetes],     # Mudar pontos por grupo
      #labels = c(),
      oma=c(3,3,3,20))
par(xpd = TRUE)
legend("right", col = c("#4DB620", "#80009A"), title = "Diabetes",
       pch = c("\u25CF", "\u25B2"), legend = c("Negativo","Positivo"))


# Boxplot geral
par(mfrow=c(1,1))
boxplot(pima[-9],xlab="variável")

# Boxplot por grupo
pima %>% gather("a", "b", -diabetes) %>%
  ggplot(aes(x=a, y=b, color=diabetes))+
  geom_boxplot()+
  labs(
    x = "Variável",
    y = "",
    color = "Diabetes"
  ) +
  theme_bw(base_size = 13)


# Correlograma
melt(cor(pima[-9])) %>% ggplot(aes(x=Var2, y=Var1, fill=value)) + 
  geom_tile()+ 
  scale_fill_distiller()+
  labs(x="", y="", fill="Correlação")+
  theme_bw()


# Histograma
par(mfrow=c(2,4))
for(i in 1:8){
  hist(as.numeric(unlist(pima[i])), probability=TRUE,
       main=stringr::str_to_title(colnames(pima)[i]), xlab="", ylab="")
}


# Gráfico Q-Q
par(mfrow=c(2,4))
for(i in 1:8){
  qqPlot(scale(pima[i]),dist="norm", id=F,
         mean=0,sd=1,col.lines="#80009A",pch = 19, col="#108A0C",
         xlab="Quantil da N(0,1)",ylab=stringr::str_to_title(colnames(pima)[1]))
}

################################################################################
################################# Modelagem ####################################
################################################################################
