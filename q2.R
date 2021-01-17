# Carregando bibliotecas utilizadas
library(tidyverse)
library(grid)
library(gridExtra)
library(car)
library(HSAUR2)
library(reshape2)
library(factoextra)
library(corrplot)
library(FactoMineR)

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

# Correlograma #1
ggplot(data = melted_cormat, aes(x=Var2, y=Var1, fill=value))+ 
  theme_bw(base_size = 16)+
  geom_tile()+ 
  scale_fill_distiller()+
  labs(x="", y="", fill="Correlação")+
  geom_text(label = melted_cormat$value)

# Correlograma #2
corrplot(cormat, method="color", tl.cex = 1.5, type="full",
         addCoef.col = "black", tl.col = "black")

# Nomes
nomes <- c("100m com barreiras", "Salto em altura",
           "Arremesso de peso", "200m rasos", 
           "Salto em distância", "Lançamento de dardos",
           "800m rasos", "Score")

# Unidades de Medida
UM = c('Tempo (s)', 'Altura (m)', 'Distância (m)', 'Tempo (s)', 
       'Distância (m)', 'Distância (m)', 'Tempo (s)', 'Pontuação')


# Histograma
par(mfrow=c(2,4),mar=c(4,4,2,2))
for(i in 1:8){
  hist(as.numeric(unlist(heptathlon[i])),probability=TRUE, col="#80009A",
       main=colnames(cormat)[i],xlab="",ylab=UM[i],
       cex.axis=1.5, cex.main=1.3, cex.lab = 1.3)
}


# Boxplot
par(mfrow=c(2,4),mar=c(4,4,2,2))
for(i in 1:8){
  boxplot(as.numeric(unlist(heptathlon[i])),probability=TRUE,
       main=nomes[i],xlab="",ylab=UM[i],
       cex.axis=1.5, cex.main=1.75, cex.lab = 1.3)
}

# Gráfico Q-Q
par(mfrow=c(2,4))
for(i in 1:8){
  qqPlot(scale(heptathlon[i]),dist="norm", id=F,
         mean=0,sd=1,col.lines="#80009A",pch = 19, col="#108A0C",
         xlab="", main=nomes[i], ylab="", cex=0.8)
}

################################################################################
############################## Analise Inferencial #############################
################################################################################

# Auto valores
m_cor <- cor(heptathlon)
aut_val <- eigen(m_cor)$values
round(aut_val,2)

# Auto vetores
aut_vec <- (eigen(m_cor)$vectors)
round(aut_vec,2)

p <- 8 # numero de variaveis

colnames(heptathlon) <- nomes

m_aut_val <- t(matrix(((aut_val)),p,p))
result_cp_cor <- princomp(heptathlon,cor=TRUE)
corr_cp_var <- aut_vec*sqrt(m_aut_val)
summary(result_cp_cor)

# Grafico da variancia dos componentes
par(mfrow=c(1,1))
screeplot(result_cp_cor, type = c("lines"), main = "", 
          cex = 1.2, cex.lab = 1.2, cex.main = 1.2, pch = 19)

# Grafico do PV
fviz_eig(result_cp_cor, addlabels = TRUE, xlab = "Componentes", main = "",
         ylab = "Percentual de variancia explicada",
         ggtheme = theme_bw(base_size = 16))

# primeiros autovetores
mcps <- t(round(aut_vec[,1:2],2))
colnames(mcps) <- nomes
mcps

# correlações dos dois primeiros autovetores com as variáveis originais
mcorcps <- t(round(corr_cp_var[,1:2],2))
colnames(mcorcps) <- nomes
mcorcps

cp1 <- cbind((result_cp_cor$scores)[,1])
cp2 <- cbind((result_cp_cor$scores)[,2])
boxplot(cbind(cp1,cp2), cex.lab = 1.2, xlab = "CP")

par(mfrow=c(1,2))
hist(cp1, probability = TRUE, xlab = "CP1", ylab = "Densidade", main = "")
hist(cp2, probability = TRUE, xlab = "CP2", ylab = "Densidade", main = "")

# Dispersão entre as componentes
par(mfrow=c(1,1))
plot(cp1, cp2, cex = 1.2, pch = 19)

# Biplot padrão
par(mfrow=c(1,1))
biplot(result_cp_cor,cex=1)

# Biplot factoextra
#result_cp_cor <- PCA(heptathlon, graph = FALSE)
fviz_pca_biplot(result_cp_cor,
                pointshape = 19,
                pointsize = 1.5,
                col.ind = "black",
                title = "",
                repel = TRUE)

# Biplot sem nome
fviz_pca_biplot(result_cp_cor, 
                geom.ind = "point",
                pointshape = 19,
                pointsize = 1.5,
                col.ind = "black",
                repel = TRUE) 

# Rank
as_tibble(matrix(c(rownames(cp1), round(cp1,2)),
                 ncol = 2)) %>% 
  mutate(V2 = as.numeric(V2)) %>%
  arrange(desc(V2)) %>%
  view()