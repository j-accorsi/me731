library(tidyverse)
library(grid)
library(gridExtra)
library(car)
library(mlbench)
library(reshape2)

# Carregar os dados
data(PimaIndiansDiabetes)

# Removendo os NA
pima <- PimaIndiansDiabetes %>% filter(glucose != 0, pressure != 0, 
                                       triceps != 0, insulin != 0, mass !=0) %>%
  select(-pedigree)

colnames(pima) <- c("Gravidez", "Glicose", "Pressão", "Tríceps", "Insulina",
                     "IMC", "Idade", "Diabetes")

################################################################################
############################# Analise Descritiva ###############################
################################################################################

# Variáveis sumárias
medados <- rbind(apply(pima[-8],2,mean),
                 apply(pima[-8],2,var),
                 apply(pima[-8],2,sd),
                 100*apply(pima[-8],2,sd)/apply(pima[-8],2,mean),
                 apply(pima[-8],2,min),
                 apply(pima[-8],2,quantile,0.5),
                 apply(pima[-8],2,max))

rownames(medados)<-c("Média","Var.","DP","CV(%)","Mínimo","Mediana","Máximo")

round(medados,2)

# Sumarias do grupo negativo
round(rbind(apply(pima[pima[8]=="neg",-8],2,mean),
            apply(pima[pima[8]=="neg",-8],2,var),
            apply(pima[pima[8]=="neg",-8],2,sd),
            100*apply(pima[pima[8]=="neg",-8],2,sd)/
              apply(pima[pima[8]=="neg",-8],2,mean),
            apply(pima[pima[8]=="neg",-8],2,min),
            apply(pima[pima[8]=="neg",-8],2,quantile,0.5),
            apply(pima[pima[8]=="neg",-8],2,max)),2)

#Sumarias do grupo positivo
round(rbind(apply(pima[pima[8]=="pos",-8],2,mean),
            apply(pima[pima[8]=="pos",-8],2,var),
            apply(pima[pima[8]=="pos",-8],2,sd),
            100*apply(pima[pima[8]=="pos",-8],2,sd)/
              apply(pima[pima[8]=="pos",-8],2,mean),
            apply(pima[pima[8]=="pos",-8],2,min),
            apply(pima[pima[8]=="pos",-8],2,quantile,0.5),
            apply(pima[pima[8]=="pos",-8],2,max)),2)


# Gráfico de dispersão
par(mfrow=c(1,1))
pairs(pima[-8],
      col = c("#4DB620", "#80009A")[pima$diabetes],   # Mudar cor por grupo
      pch = c("\u25CF", "\u25B2")[pima$diabetes],     # Mudar pontos por grupo
      labels = c("Gravidez", "Glicose", "Pressão", "Tríceps", "Insulina",
                 "IMC", "Idade"),
      oma=c(3,3,3,20))
par(xpd = TRUE)
legend("right", col = c("#4DB620", "#80009A"), title = "Diabetes",
       pch = c("\u25CF", "\u25B2"), legend = c("Negativo","Positivo"))


# Boxplot geral
par(mfrow=c(1,1))
boxplot(pima[-c(1,2,5,8)],xlab="", names=c("Pressão", "Tríceps",
                                                   "IMC", "Idade"))

par(mfrow=c(1,3))
boxplot(pima[1], xlab="Gravidez", cex.axis=1.5, cex.lab=2)
boxplot(pima[2], xlab="Glicose", cex.axis=1.5, cex.lab=2)
boxplot(pima[5], xlab="Insulina", cex.axis=1.5, cex.lab=2)

# Boxplot por grupo
pima = pima %>% mutate(Diabetes = ifelse(Diabetes == "neg", 
                                         "Negativo", "Positivo"))


pima %>% select(Pressão, Tríceps, IMC, Idade, Diabetes) %>%
  gather("a", "b", -Diabetes) %>%
  ggplot(aes(x=a, y=b, color=Diabetes))+
  geom_boxplot()+
  labs(
    x = "",
    y = "",
    color = "Diabetes"
  ) +
  theme_bw(base_size = 13)


# Correlograma
melt(cor(pima[-8])) %>% ggplot(aes(x=Var2, y=Var1, fill=value)) + 
  geom_tile()+ 
  scale_fill_distiller()+
  labs(x="", y="", fill="Correlação")+
  theme_bw()


# Histograma
par(mfrow=c(2,4))
for(i in 1:7){
  hist(as.numeric(unlist(pima[i])), probability=TRUE, col="#EE6060",
       main=stringr::str_to_title(colnames(pima)[i]), xlab="", ylab="")
}


# Gráfico Q-Q
par(mfrow=c(2,4))
for(i in 1:8){
  qqPlot(scale(pima[i]),dist="norm", id=F,
         mean=0,sd=1,col.lines="#80009A",pch = 19, col="#108A0C",
         xlab="",ylab=stringr::str_to_title(colnames(pima)[i]))
}


################################################################################
################################# Modelagem ####################################
################################################################################
