library(plyr)
library(tidyverse)
library(grid)
library(gridExtra)
library(car)
library(mlbench)
library(reshape2)
library(CCA)
library(MASS)
library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter","dplyr")
conflict_prefer("mutate","dplyr")

# Carregar os dados
data(PimaIndiansDiabetes)

# Removendo os NA
pima <- PimaIndiansDiabetes %>% filter(glucose != 0, pressure != 0, 
                                       triceps != 0, insulin != 0, mass !=0) %>%
  select(-pedigree)

nomes <- c("Gravidez", "Glicose", "Pressão", "Tríceps", "Insulina",
           "IMC", "Idade", "Diabetes")

colnames(pima) <- nomes

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
      col = c("#4DB620", "#80009A")[pima$Diabetes],   # Mudar cor por grupo
      pch = c("\u25CF", "\u25B2")[pima$Diabetes],     # Mudar pontos por grupo
      labels = c("Gravidez", "Glicose", "Pressão", "Tríceps", "Insulina",
                 "IMC", "Idade"),
      cex.labels = 2.5,
      cex.axis = 1.75,
      oma=c(3,3,6,20))
par(xpd = TRUE)
legend("right", col = c("#4DB620", "#80009A"), title = "Diabetes", cex = 1.1,
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
pima %>% mutate(Diabetes = ifelse(Diabetes == "neg", 
                                  "Negativo", "Positivo")) %>% 
  select(Pressão, Tríceps, IMC, Idade, Diabetes) %>%
  gather("a", "b", -Diabetes) %>%
  ggplot(aes(x=a, y=b, color=Diabetes))+
  geom_boxplot()+
  labs(
    x = "",
    y = "",
    color = "Diabetes"
  ) +
  theme_bw(base_size = 25)

pima %>% mutate(Diabetes = ifelse(Diabetes == "neg", 
                                  "Negativo", "Positivo")) %>% 
  select(Gravidez, Diabetes) %>%
  gather("a", "b", -Diabetes) %>%
  ggplot(aes(x=a, y=b, color=Diabetes))+
  geom_boxplot()+
  labs(
    x = "",
    y = "",
    color = "Diabetes"
  ) +
  theme_bw(base_size = 25)


# Correlograma #1
melt(cor(pima[-8])) %>% ggplot(aes(x=Var2, y=Var1, fill=value)) + 
  geom_tile()+ 
  scale_fill_distiller()+
  labs(x="", y="", fill="Correlação")+
  theme_bw(base_size = 20)+
  geom_text(label = round(melt(cor(pima[-8]))$value,2))

# Correlograma #2
corrplot(cor(pima[-8]), method="color", tl.cex = 1.5, type="full",
         addCoef.col = "black", tl.col = "black")


# Histograma
par(mfrow=c(2,4))
for(i in 1:7){
  hist(as.numeric(unlist((pima %>% filter(Diabetes == "neg"))[i])),
       probability=TRUE, col="#4DB620", cex.axis = 1.5, cex.main = 2,
       main=stringr::str_to_title(colnames(pima)[i]), xlab="", 
       ylab="Frequência", cex.lab=1.3)
}

par(mfrow=c(2,4))
for(i in 1:7){
  hist(as.numeric(unlist((pima %>% filter(Diabetes == "pos"))[i])),
       probability=TRUE, col="#80009A", cex.axis = 1.5, cex.main = 2,
       main=stringr::str_to_title(colnames(pima)[i]), xlab="", 
       ylab="Frequência", cex.lab=1.3)
}


# Gráfico Q-Q
par(mfrow=c(2,4))
for(i in 1:7){
  qqPlot(scale((pima %>% filter(Diabetes == "neg"))[i]),
         dist="norm", id=F, ylab = "", col="#4DB620",
         mean = 0, sd = 1,col.lines = "#F26514", pch = 19,
         xlab = "", main=stringr::str_to_title(colnames(pima)[i]))
}

par(mfrow=c(2,4))
for(i in 1:7){
  qqPlot(scale((pima %>% filter(Diabetes == "pos"))[i]),
         dist="norm", id=F, ylab = "", col="#80009A",
         mean = 0, sd = 1,col.lines = "#F26514", pch = 19,
         xlab = "", main=stringr::str_to_title(colnames(pima)[i]))
}
################################################################################
############################## Analise Inferencial #############################
################################################################################

pima <- PimaIndiansDiabetes %>% filter(glucose != 0, pressure != 0, 
                                       triceps != 0, insulin != 0, mass !=0) %>%
  select(-pedigree)

# Densidades suavizadas
par(mfrow=c(2,4))
for (i in 1:7) {
  plot(density(unlist((pima %>% filter(diabetes == "pos"))[i])),
       lwd=2, xlim=c(min(pima[,i])-1,max(pima[,i])+1),
       ylim = c(0,0.28),
       xlab=nomes[i],ylab="Densidade",cex=1.2,
       cex.lab=1.5,cex.main=1.2,main="")
  lines(density(unlist((pima %>% filter(diabetes == "neg"))[i])),
        col=2,lwd=2)
  if(i == 1){
    legend(7,0.20,lwd=c(2,2,2),col=c(1,2,3),
           legend=c("Positivo","Negativo"),
           bty="n",cex=1.25)
  }
  else{
    legend(min(pima[,i]) + 10,0.20,lwd=c(2,2,2),col=c(1,2,3),
           legend=c("Positivo","Negativo"),
           bty="n",cex=1.25)
  }
}

# Gráficos das distâncias de Mahalanobis
vqn <- nrow(pima)*mahalanobis(pima %>% filter(diabetes == "neg") %>%
                                select(-diabetes),
                              center = apply((pima %>% 
                                                filter(diabetes == "neg") %>%
                                                select(-diabetes)),2,mean),
                              cov = cov(pima %>% filter(diabetes == "neg") %>%
                                          select(-diabetes)))

vqp <- nrow(pima)*mahalanobis(pima %>% filter(diabetes == "pos") %>%
                                select(-diabetes),
                              center = apply((pima %>% 
                                                filter(diabetes == "pos") %>%
                                                select(-diabetes)),2,mean),
                              cov = cov(pima %>% filter(diabetes == "pos") %>%
                                          select(-diabetes)))

par(mfrow=c(1,1))
plot(density(vqn),lwd=2,xlab="Valor",main="",
     ylab="Densidade",cex=1.2,cex.lab=1.5,ylim=c(0,0.001))
lines(density(vqp),lwd=2,cex=1.2,cex.lab=1.2,cex.main=1.2,col=2)
legend(300,0.001,lwd=c(2,2,2),col=c(1,2,3),legend=c("Negativo","Positivo"),
       bty="n",cex=1.5)


# Trabalhando com dois grupos (positivo e negativo)

# Selecionando as amostras
set.seed(223515)
pima_ordenado <- rbind(pima %>% filter(diabetes == "neg"),
                       pima %>% filter(diabetes == "pos"))
treino <- c(sort(sample(1:262, 184, replace=FALSE)),
            sort(sample(263:392,91,replace=FALSE)))

# Amostra treino
# Utilizando a função lda
m_pima <- data.frame(pima_ordenado[-8],
                     diabetes = c(rep("neg", 262), rep("pos", 130)))

result_ad <- lda(c(rep("neg", 262), rep("pos", 130)) ~ .,
                 m_pima[-8], prior = c(262/392, 130/392),
                 subset = treino)
result_ad

# predizendo os grupos na amostra teste
pred <- predict(result_ad, m_pima[-treino, ])$class
# função discriminante
y <-predict(result_ad, m_pima[-treino, ])$x 
# Pegando a amostra teste
data_teste <- m_pima[-treino,8]
# Tabela de classificação
tc <- table(data_teste,pred)

# TEA
100*(tc[1,2]+tc[2,1])/sum(tc)

# TOE
m_dados_pima <- m_pima[treino,]

v_mean1 <- cbind(c(result_ad$means[1,]))
v_mean2 <- cbind(c(result_ad$means[2,]))
S21 <- cov(m_dados_pima %>% filter(diabetes == "pos") %>%
             select(-diabetes))
S22 <- cov(m_dados_pima %>% filter(diabetes == "neg") %>%
             select(-diabetes))
Sp <- ((nrow(m_dados_pima %>% filter(diabetes == "pos") %>%
               select(-diabetes))-1)*S21 +  
         ((nrow(m_dados_pima %>% filter(diabetes == "neg") %>%
                  select(-diabetes))-1)*S22))/
  (nrow(m_dados_pima %>% filter(diabetes == "pos") %>%
          select(-diabetes)) + 
     nrow(m_dados_pima %>% filter(diabetes == "neg") %>%
            select(-diabetes)) - 2)
q_classifi <- 0.5*t(v_mean1-v_mean2)%*%solve(Sp)%*%(v_mean1+v_mean2)
delta2 <-  t(v_mean1-v_mean2)%*%solve(Sp)%*%(v_mean1 - v_mean2)
#TOE
100*pnorm(-sqrt(delta2)/2)

# Análise da função discriminante para a amostra teste
grupo <- as.factor(c(rep("neg", 262 - 184), rep("pos", 130 - 91)))
# Medidas resumo
ad_dados<-data.frame(y,grupo)
colnames(ad_dados)<-c("y","dados")
medados<-ddply(ad_dados,.(grupo),plyr::summarise,media=mean(y),
               dp=sqrt(var(y)),vari=var(y),minimo=min(y),
               mediana=quantile(y,0.5),maximo=max(y),n=length(y))
colnames(medados)<-c("Grupo","Média","DP","Var.",
                     "Mínimo","Mediana","Máximo","n")
medados

# boxplot

par(mfrow=c(1,1))
boxplot(y~grupo,cex=1.2,cex.lab=1.2,
        xlab = "Grupo")

# densidades
par(mfrow=c(1,1))
plot(density(y[grupo=="pos",1]),lwd=2,
     xlim=c(min(y[,1])-1,max(y[,1])+3),xlab="Função discriminante",
     ylab="Densidade",cex=1.2,cex.lab=1.2,cex.main=1.2,main="",ylim=c(0,0.60),
     cex.lab=1.5)
lines(density(y[grupo=="neg",1]),col=2,lwd=2)
legend(3,0.45,lwd=c(2,2),col=c(1,2),legend=c("Positivo","Negativo"),bty="n",
       cex=1.5)
