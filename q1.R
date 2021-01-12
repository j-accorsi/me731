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
    
round(medados,2)

# Sumarias do grupo macho
round(rbind(apply(tartarugas[tartarugas[4]=="Macho",-4],2,mean),
            apply(tartarugas[tartarugas[4]=="Macho",-4],2,var),
            apply(tartarugas[tartarugas[4]=="Macho",-4],2,sd),
            100*apply(tartarugas[tartarugas[4]=="Macho",-4],2,sd)/
              apply(tartarugas[tartarugas[4]=="Macho",-4],2,mean),
            apply(tartarugas[tartarugas[4]=="Macho",-4],2,min),
            apply(tartarugas[tartarugas[4]=="Macho",-4],2,quantile,0.5),
            apply(tartarugas[tartarugas[4]=="Macho",-4],2,max)),2)

#Sumarias do grupo femêa
round(rbind(apply(tartarugas[tartarugas[4]=="Fêmea",-4],2,mean),
            apply(tartarugas[tartarugas[4]=="Fêmea",-4],2,var),
            apply(tartarugas[tartarugas[4]=="Fêmea",-4],2,sd),
            100*apply(tartarugas[tartarugas[4]=="Fêmea",-4],2,sd)/
              apply(tartarugas[tartarugas[4]=="Fêmea",-4],2,mean),
            apply(tartarugas[tartarugas[4]=="Fêmea",-4],2,min),
            apply(tartarugas[tartarugas[4]=="Fêmea",-4],2,quantile,0.5),
            apply(tartarugas[tartarugas[4]=="Fêmea",-4],2,max)),2)

# Gráfico de dispersão
par(mfrow=c(1,1))
pairs(tartarugas[1:3],
      col = c("#4DB620", "#80009A")[tartarugas$Sexo],  # Mudar cor por grupo
      pch = 19,                                        # Mudar pontos
      labels = c("Comprimento", "Largura", "Altura"), 
      cex.labels = 2.5,
      cex.axis = 1.75,
      oma=c(3,3,3,15))
par(xpd = TRUE)
legend("right", col = c("#4DB620", "#80009A"),
       pch = c(19,19), legend = c("Fêmea","Macho"),
       cex = 1.35)

# Boxplot geral
par(mfrow=c(1,1))
boxplot(tartarugas[1:3],xlab="variável")

# Boxplot por sexo
tartarugas %>% gather("a", "b", -Sexo) %>%
  ggplot(aes(x=a, y=b, color=Sexo))+
  geom_boxplot()+
  labs(
    x = "",
    y = "",
    color = "Sexo"
  ) +
  theme_bw(base_size = 25)

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
p1 <- tartarugas %>% ggplot()+
  geom_histogram(aes(Largura, fill=Sexo), bins=12, col="black")+
  labs(y = "")+
  theme_bw()
mylegend<-g_legend(p1)

# Histogramas
grid.arrange(
  tartarugas %>% filter(Sexo == "Fêmea") %>%
    ggplot()+
    geom_histogram(aes(Largura), fill="#4DB620", bins=12, col="black")+
    labs(y = "")+
    theme_bw(), 
  tartarugas %>% filter(Sexo == "Macho") %>%
    ggplot()+
    geom_histogram(aes(Largura), fill="#80009A", bins=12, col="black")+
    labs(y = "")+
    theme_bw(),
  tartarugas %>% filter(Sexo == "Fêmea") %>%
    ggplot()+
    geom_histogram(aes(Altura), fill="#4DB620", bins=12, col="black")+
    labs(y = "")+
    theme_bw(),
  tartarugas %>% filter(Sexo == "Macho") %>%
    ggplot()+
    geom_histogram(aes(Altura), fill="#80009A", bins=12, col="black")+
    labs(y = "")+
    theme_bw(),
  tartarugas %>% filter(Sexo == "Fêmea")%>% 
    ggplot()+
    geom_histogram(aes(Comprimento), fill="#4DB620", bins=12, col="black")+
    labs(y = "")+
    theme_bw(),
  tartarugas %>% filter(Sexo == "Macho")%>% 
    ggplot()+
    geom_histogram(aes(Comprimento), fill="#80009A", bins=12, col="black")+
    labs(y = "")+
    theme_bw(),
  nrow=3, ncol=2
)

# Gráfico Q-Q
par(mfrow=c(2,2))
for(i in 1:3){
  qqPlot(scale((tartarugas %>% filter(Sexo == "Fêmea"))[i]),dist="norm", id=F,
         mean=0,sd=1,col.lines="#F26514",pch = 19, col="#108A0C", xlab = "",
         main=colnames((tartarugas %>% filter(Sexo == "Fêmea")))[i], ylab = "")
}

par(mfrow=c(2,2))
for(i in 1:3){
  qqPlot(scale((tartarugas %>% filter(Sexo == "Macho"))[i]),dist="norm", id=F,
         mean=0,sd=1,col.lines="#F26514",pch = 19, col="#80009A", xlab = "",
         main=colnames((tartarugas %>% filter(Sexo == "Fêmea")))[i], ylab = "")
}

################################################################################
############################## Analise Inferencial #############################
################################################################################

# Dois ou mais vetores de médias
teste_mu1mu2_Homocedast <- function(m_X_completa, v_grupos, Delta, alpha)
{
  m_X_1 <-  m_X_completa[v.grupos==1,]
  m_X_2 <-  m_X_completa[v.grupos==2,]
  p<- ncol(m_X_1)
  v_n <- rbind(nrow(m_X_1),nrow(m_X_2))
  v_mu1 <-  cbind(apply(m_X_1,2,mean))
  v_mu2 <-  cbind(apply(m_X_2,2,mean))
  m_Sigma1 <- cov(m_X_1)
  m_Sigma2 <- cov(m_X_2)
  m_SigmaP <- ((v_n[1]-1)*m_Sigma1 + (v_n[2]-1)*m_Sigma2)/(v_n[1] + v_n[2] - 2)
  e_F <- (1/(1/v_n[1] + 1/v_n[2]))*(t(((v_mu1 - v_mu2) - Delta))) %*% 
    solve(m_SigmaP) %*% (((v_mu1 - v_mu2) - Delta))
  df1 <- p
  df2 <- v_n[1] + v_n[2] - p - 1
  e_F <- e_F *df2/(df1*(v_n[1] + v_n[2] - 2))
  p_valor <- 1 - pf(e_F,df1,df2)
  fc = qf(1 - alpha,df1,df2)
  e.pnc = e_F
  e.pt = 1 - pf(fc,df1,df2,ncp=e.pnc) # poder aproximado
  cat("Estatística do Teste: ",e_F,"\n")
  cat("nível descritivo: ",p_valor,"\n")
  cat("Estimativa do poder do teste: ",e.pt,"\n")
  result <- list(e_F=e_F,p.valor=p_valor,e.pt=e.pt)
} 