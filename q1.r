library(tidyverse)
library(grid)
library(gridExtra)
library(car)

# Leitura dos dados
tartarugas <- read_csv("dados/tartarugas.csv")

# Transformando em factor
tartarugas$sex <- as.factor(tartarugas$sex)

# Mudando nomes das colunas
colnames(tartarugas) <- c("Comprimento", "Largura", "Altura", "Sexo")

################################################################################
############################# Analise Descritiva ###############################
################################################################################

# Variáveis sumárias
medados <-rbind(apply(tartarugas[1:3],2,mean),
                apply(tartarugas[1:3],2,var),
                apply(tartarugas[1:3],2,sd),
                100*apply(tartarugas[1:3],2,sd)/apply(tartarugas[1:3],2,mean),
                apply(tartarugas[1:3],2,min),
                apply(tartarugas[1:3],2,quantile,0.5),
                apply(tartarugas[1:3],2,max))
                
rownames(medados)<-c("Média","Var.","DP","CV(%)","Mínimo","Mediana","Máximo")

medados

# Gráfico de dispersão
par(mfrow=c(1,1))
pairs(tartarugas[1:3],
      col = c("#4DB620", "#80009A")[tartarugas$Sexo],   # Change color by group
      pch = 19,                            # Change points by group
      labels = c("Comprimento", "Largura", "Altura"),
      oma=c(3,3,3,15))
par(xpd = TRUE)
legend("right", col = c("#4DB620", "#80009A"),   # Change color by group
       pch = c(19,19), legend = c("Fêmea","Macho"))

# Boxplot geral
par(mfrow=c(1,1))
boxplot(tartarugas[1:3],xlab="variável")

# Boxplot por sexo
tartarugas %>% gather("a", "b", -Sexo) %>%
  ggplot(aes(x=a, y=b, color=Sexo))+
  geom_boxplot()+
  labs(
    x = "Variável",
    y = "",
    color = "Sexo"
  ) +
  theme_bw(base_size = 13)

# Histogramas
grid.arrange(tartarugas %>% ggplot()+
               geom_histogram(aes(Largura, fill=Sexo), bins=12, col="black")+
               theme_bw(), 
             tartarugas %>% ggplot()+
               geom_histogram(aes(Altura, fill=Sexo), bins=12, col="black")+
               theme_bw(),
             tartarugas %>% ggplot()+
               geom_histogram(aes(Comprimento, fill=Sexo), bins=12, col="black")+
               theme_bw(),
             nrow=2, ncol=2)

# Gráfico Q-Q
par(mfrow=c(2,2))
for(i in 1:3){
  qqPlot(scale(tartarugas[i]),dist="norm", id=F,
         mean=0,sd=1,col.lines="#80009A",pch = 19, col="#108A0C",
         xlab="Quantil da N(0,1)",ylab=colnames(tartarugas)[i])
}

################################################################################
################################# Modelagem ####################################
################################################################################
