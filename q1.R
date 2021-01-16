library(tidyverse)
library(grid)
library(gridExtra)
library(car)
library(plotrix)

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
    y = "Milímetros",
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
    labs(y = "Frequência")+
    theme_bw(), 
  tartarugas %>% filter(Sexo == "Macho") %>%
    ggplot()+
    geom_histogram(aes(Largura), fill="#80009A", bins=12, col="black")+
    labs(y = "")+
    theme_bw(),
  tartarugas %>% filter(Sexo == "Fêmea") %>%
    ggplot()+
    geom_histogram(aes(Altura), fill="#4DB620", bins=12, col="black")+
    labs(y = "Frequência")+
    theme_bw(),
  tartarugas %>% filter(Sexo == "Macho") %>%
    ggplot()+
    geom_histogram(aes(Altura), fill="#80009A", bins=12, col="black")+
    labs(y = "")+
    theme_bw(),
  tartarugas %>% filter(Sexo == "Fêmea")%>% 
    ggplot()+
    geom_histogram(aes(Comprimento), fill="#4DB620", bins=12, col="black")+
    labs(y = "Frequência")+
    theme_bw(),
  tartarugas %>% filter(Sexo == "Macho")%>% 
    ggplot()+
    geom_histogram(aes(Comprimento), fill="#80009A", bins=12, col="black")+
    labs(y = "")+
    theme_bw(),
  nrow=3, ncol=2
)

# Gráfico Q-Q
par(mfrow=c(1,3))
for(i in 1:3){
  qqPlot(scale((tartarugas %>% filter(Sexo == "Fêmea"))[i]),dist="norm", id=F,
         mean=0,sd=1,col.lines="#F26514",pch = 19, col="#108A0C", xlab = "",
         main=colnames((tartarugas %>% filter(Sexo == "Fêmea")))[i], ylab = "")
}

par(mfrow=c(1,3))
for(i in 1:3){
  qqPlot(scale((tartarugas %>% filter(Sexo == "Macho"))[i]),dist="norm", id=F,
         mean=0,sd=1,col.lines="#F26514",pch = 19, col="#80009A", xlab = "",
         main=colnames((tartarugas %>% filter(Sexo == "Fêmea")))[i], ylab = "")
}

################################################################################
############################## Analise Inferencial #############################
################################################################################

# definindo funções do prof. Caio

# m_X_completa: matriz de dados de todos os grupos
# v_grupos: vetor indicar de grupos
# v_n: vetor com tamanhos amostrais em cada grupo
# G: número de grupos

Box_teste_Igual_MCov<-function(m_X_completa,v_grupos,v_n,G)
{
  # v_grupos (1,2,3...)
  # m_X_completa : matriz de dados com todos os grupos
  grupo <- 1
  p<- ncol(m_X_completa)
  m_X_k <- m_X_completa[v_grupos==grupo,]
  Sigma_k <- cov(m_X_k)
  m_Sigma_completa <- cbind(grupo,Sigma_k)
  Sigma_P <- (v_n[grupo]-1)*Sigma_k # estimativa ponderada
  aux_k_1 <- (v_n[grupo] - 1)*log(det(Sigma_k))
  grupo <- grupo + 1
  for (i in 2:G)
  {
    m_X_k <- m_X_completa[v_grupos==grupo,] # pegar os dados referentes ao grupo i
    Sigma_k <- cov(m_X_k)
    m_Sigma_completa <- rbind(m_Sigma_completa,cbind(grupo,Sigma_k))
    Sigma_P <- Sigma_P + (v_n[grupo]-1)*Sigma_k # estimativa ponderada
    aux_k_1 <- aux_k_1 + (v_n[grupo] - 1)*log(det(Sigma_k))
    grupo <- grupo + 1
  }
  Sigma_P <- Sigma_P/(sum(v_n)-G)
  
  # Estatística de ajuste
  aux_u <- (sum(1/(v_n - 1)) - 
              (1/(sum(v_n - 1))))*(2*p^2 + 3*p - 1)/(6*(p+1)*(G-1))
  Q_B <-  (1 - aux_u)*(sum(v_n-1)*log(det(Sigma_P)) - aux_k_1)
  aux.v <- 0.5*p*(p+1)*(G-1)
  e_nd_QB <- 1 - pchisq(Q_B,aux.v)
  cat("Estatística do Teste: ", Q_B, "\n")
  cat("nível descritivo: ",e_nd_QB,"\n")
  cat("Matrizes de Covariâncias por grupo: \n")
  print(m_Sigma_completa)
  Sigma_P <-as.matrix(data.frame(Sigma_P))
  list(Sigma_P=Sigma_P)
} # fim fa função


# mY: matriz de dados
# fit_model: ajuste da função lm

mSigmareg <- function(mY,fit_model)
{
  mX <- as.matrix(model.matrix(fit_model))
  q <- ncol(mX)
  p<-ncol(mY)
  n <- nrow(mY)
  mB <- matrix(coef(fit_model),q,p)
  mSigma <- t(as.matrix(mY-mX%*%mB))%*%(as.matrix(mY-mX%*%mB))/(n-q)
  return(mSigma)
}

# mY: matriz de dados
# fit_model: ajuste da função lm
# gama: nível de confiança

estim_par_MRNLM <- function(mY,fit_model,gama)
  
{
  
  vbeta <- c(t(coef(fit_model)))
  mSigma<-mSigmareg(mY,fit_model)
  mX <-as.matrix(model.matrix(fit_model))
  n<-nrow(mX)
  q<-ncol(mX)
  mSigmabeta <- kronecker(solve(t(mX)%*%mX),mSigma)
  epbeta<-sqrt(diag(mSigmabeta))
  et <- qt(0.5*(1+gama),df=n-q)
  LIIC <- vbeta-et*epbeta
  LSIC <- vbeta+et*epbeta
  estt <- vbeta/epbeta
  pvalor <- 2*(1-pt(abs(estt),df=n-q))
  #
  mresult <- cbind(vbeta,epbeta,LIIC,LSIC,estt,pvalor)
  return(mresult)
  
  
}

# fit_model: ajuste da função manova
# m_Sigma_P: matriz de variâncias e covariâncias estimada via modelo
# p: número de variáveis
# q: número de parâmetros por variável
# m_C,m_U,m_M: matrizes de interesse

Teste_CBU_M<-function(fit_model,m_Sigma_P,p,q,m_C,m_U,m_M)
{
  m_B <- matrix(coef(fit_model),q,p)
  v_beta <- matrix(t(m_B))
  m_X <- model.matrix(fit_model)
  m_Ca <- kronecker(m_C,t(m_U))
  m_Ma <- matrix(t(m_M))
  v_theta <- m_Ca%*%v_beta - m_Ma
  m_Sigmabeta <- kronecker(solve(t(m_X)%*%m_X),m_Sigma_P)
  estat <- t(v_theta)%*%solve(m_Ca%*%(m_Sigmabeta)%*%t(m_Ca))%*%v_theta
  p_valor <- 1-pchisq(estat,df=nrow(m_C)*ncol(m_U))
  cat("Estatistica Qui-quadrado = ",round(estat,2),"\n")
  cat("pvalor = ",round(p_valor,4),"\n")
  cat("Matriz C :","\n")
  print(m_C)
  cat("Matriz U :","\n")
  print(m_U)
  cat("Matriz M :","\n")
  print(m_M)
}

###################################
# mY: matriz de dados
# mresult: resultado da função manova
# var: escolha da variável (1,2,..)
# typeresid:
# univariate (RS-veja slides)
# multivariate (RSM-veja slides)
# wplot:
# diagnostics: quatro gráficos
# envelope: gráfico de envelope

gen_graf_resid<-function(mY,mresult,var,typeresid,wplot)
{
  mresiduo <- mresult$residuals
  mbeta <- coef(mresult) 
  mX <- as.matrix(model.matrix(mresult))
  n <- nrow(mX)
  p <- ncol(mbeta)
  q <- nrow(mbeta)
  mSigma<-t(mY-mX%*%mbeta)%*%(mY-mX%*%mbeta)/(n-q)
  if (typeresid == "univariate")
  {
    auxres <- diag((diag(n) - mX%*%solve(t(mX)%*%mX)%*%t(mX)))
    mresiduo <- mresiduo/(sqrt((matrix(auxres,n,p))%*%diag(diag(mSigma))))
  }
  else if (typeresid == "multivariate")
  {
    mresiduo <- t(solve(t(chol(mSigma)))%*%t(mresiduo))
  }
  mfit <- fitted.values(mresult)
  #
  if (wplot == "diagnostics")
  {
    par(mfrow =c(2,2))
    plot(mresiduo[,var],
         ylim=c(min(-3,min(mresiduo[,var])),max(3,max(mresiduo[,var]))),
         xlab="índice",ylab="resíduo studentizado")
    abline(-2,0,lty=2)
    abline(2,0,lty=2)
    abline(0,0,lty=2)
    #
    plot(mfit[,var],mresiduo[,var],
         ylim=c(min(-3,min(mresiduo[,var])),max(3,max(mresiduo[,var]))),
         xlab="valor ajustado",ylab="resíduo studentizado")
    abline(-2,0,lty=2)
    abline(2,0,lty=2)
    abline(0,0,lty=2)
    #
    hist(mresiduo[,var],probability=TRUE,xlab="resíduo studentizado",
         main="",ylab="densidade")
    #
    qqPlot((mresiduo[,var]),dist="norm",mean=0,sd=1,col.lines=1,
           grid="FALSE",xlab="quantil da N(0,1)",
           ylab=paste("quantil do resíduo studentizado"),cex=1.2)
  }
  
  else if (wplot == "envelope")
  {
    par(mfrow =c(1,1))
    qqPlot((mresiduo[,var]),dist="norm",mean=0,sd=1,col.lines=1,
           grid="FALSE",xlab="quantil da N(0,1)",
           ylab=paste("quantil do resíduo studentizado"),cex=1.2)
  }
}

# mY: matriz de dados
# mresult: resultado da função manova

gen_graf_resid_quad_form<-function(mY,mresult)
{
  mresiduo <- mresult$residuals
  mbeta <- coef(mresult) 
  mX <- as.matrix(model.matrix(mresult))
  n <- nrow(mX)
  p <- ncol(mbeta)
  q <- nrow(mbeta)
  mSigma<-t(mY-mX%*%mbeta)%*%(mY-mX%*%mbeta)/(n-q)
  vmu<- apply(mresiduo,2,mean)
  #vresid <- n*apply(((mresiduo-vmu)*(mresiduo-vmu)%*%solve(mSigma)),1,sum)
  vresid <- n*mahalanobis(mresiduo,center=vmu,cov=mSigma)
  #vresid<- #(n-nvar)*vresid/((n-1)*n)
  nvar <- length(vmu)
  n <- length(vresid)
  #  qqPlot(vresid,dist="chisq",df=nvar,col.lines=1,grid="FALSE",xlab="quantil da distribuição qui-quadrado",ylab="quantil da forma quadrática",cex=1.2,id.cex=1.2)
  mX <- model.matrix(mresult)
  vresidA <- matrix(0,n,1)
  #mident <- diag(1,p)
  for (i in 1:n)
  {
    mXi <- rbind(mX[i,])
    mYi <- rbind(mY[i,])
    Ai <- 1 - mXi%*%solve(t(mX)%*%mX)%*%t(mXi)
    vresidA[i] <- (Ai^(-2))*mYi%*%solve(mSigma)%*%t(mYi)
  }
  par(mfrow =c(1,1))
  qqPlot(vresidA,dist="chisq",df=nvar,col.lines=1,grid="FALSE",
         xlab="quantil da distribuição qui-quadrado",
         ylab="quantil da forma quadrática",cex=1.2)
  
}  


###################################
# Análise de variância Multivariada

#p=3
#G<-2

# Teste de iguldade das matrizes de covariância
m_Sigma_P<-Box_teste_Igual_MCov(tartarugas[-4],
                                cbind(as.numeric(unlist(tartarugas$Sexo))),
                                rbind(24,24), 2)$Sigma_P
#

# Comparação dos vetores de médias
fit_model <- manova(as.matrix(tartarugas[-4]) ~ tartarugas$Sexo)
summary.manova(fit_model,test="Wilks")
summary.manova(fit_model,test="Pillai")
summary.manova(fit_model,test="Hotelling-Lawley")
summary.manova(fit_model,test="Roy")

summary.aov(fit_model)
fit_modelLM <- lm(as.matrix(tartarugas[-4]) ~ tartarugas$Sexo)
summary(fit_modelLM)

# Matriz de variâncias e covariâncias via modelo
mSigmareg(tartarugas[-4],fit_modelLM)

# Estimativa dos parâmetros
estim_par_MRNLM(tartarugas[-4],fit_modelLM,0.95)


########################
# Comparações múltiplas

# Variável Comprimento
Teste_CBU_M(fit_model,m_Sigma_P,p = 3,q = 2, 
            m_C = cbind(0,1), m_U = rbind(1,0,0) ,m_M = 0)

# Variável Largura
Teste_CBU_M(fit_model,m_Sigma_P,p = 3,q = 2,
            m_C = cbind(0,1),m_U = rbind(0,1,0), m_M = 0)
#
# Variável Altura
Teste_CBU_M(fit_model,m_Sigma_P,p = 3,q = 2,
            m_C = cbind(0,1),m_U = rbind(0,0,1), m_M = 0)

# Médias preditas pelo modelo
m_B <- matrix(coef(fit_model),2,3)
v_beta <- matrix(t(m_B))
m_X <- model.matrix(fit_model)
m_Sigmabeta <- kronecker(solve(t(m_X)%*%m_X),m_Sigma_P)
# Comprimento
m_mu_comp <- rbind(rbind(c(1, rep(0,5))),
                   rbind(c(1, 0, 0, 1, 0, 0)))
m_pre_comp <- m_mu_comp%*%v_beta
m_ep_comp <-  sqrt(diag(m_mu_comp%*%m_Sigmabeta%*%t(m_mu_comp)))
m_IC_comp <- cbind(m_pre_comp-1.96*m_ep_comp,m_pre_comp+1.96*m_ep_comp)
# Largura
m_mu_lag <- rbind(rbind(c(0, 1, rep(0,4))),
                  rbind(c(0, 1, 0, 0, 1, 0)))
m_pre_lag <- m_mu_lag%*%v_beta
m_ep_lag <-  sqrt(diag(m_mu_lag%*%m_Sigmabeta%*%t(m_mu_lag)))
m_IC_lag <- cbind(m_pre_lag-1.96*m_ep_lag,m_pre_lag+1.96*m_ep_lag)
# Altura
m_mu_alt <- rbind(rbind(c(0, 0, 1, 0, 0, 0)),
                  rbind(c(0, 0, 1, 0, 0, 1)))
m_pre_alt <- m_mu_alt%*%v_beta
m_ep_alt <-  sqrt(diag(m_mu_alt%*%m_Sigmabeta%*%t(m_mu_alt)))
m_IC_alt <- cbind(m_pre_alt-1.96*m_ep_alt,m_pre_alt+1.96*m_ep_alt)

#
par(mfrow=c(2,2))
plotCI(m_pre_comp,ui=m_IC_comp[,2],li=m_IC_comp[,1],axes=FALSE,xlab="Sexo",
       ylab="Média",pch=19,cex=1.2,cex.lab=1.2,cex.axis=1.2,main="Comprimento")
lines(mean(tartarugas$Comprimento),pch=17,col="gray",cex=1.2,type="p")
axis(2,seq(4,7,0.2),cex.axis=1.2)
axis(1,1:2,c("Fêmea","Macho"),cex.axis=1.2)
#
plotCI(m_pre_lag,ui=m_IC_lag[,2],li=m_IC_lag[,1],axes=FALSE,xlab="Sexo",
       ylab="Média",pch=19,cex=1.2,cex.lab=1.2,cex.axis=1.2,main="Largura")
lines(mean(tartarugas$Largura),pch=17,col="gray",cex=1.2,type="p")
axis(2,seq(2,4,0.2),cex.axis=1.2)
axis(1,1:2,c("Fêmea","Macho"),cex.axis=1.2)
#
plotCI(m_pre_alt,ui=m_IC_alt[,2],li=m_IC_alt[,1],axes=FALSE,xlab="Sexo",
       ylab="Média",pch=19,cex=1.2,cex.lab=1.2,cex.axis=1.2,main="Altura")
lines(mean(tartarugas$Altura),pch=17,col="gray",cex=1.2,type="p")
axis(2,seq(1,6,0.2),cex.axis=1.2)
axis(1,1:2,c("Fêmea","Macho"),cex.axis=1.2)


# Resíduos univariados


# Comprimento
gen_graf_resid(as.matrix(tartarugas[-4]),fit_model, 1,
               "univariate","diagnostics")

# Largura
gen_graf_resid(as.matrix(tartarugas[-4]),fit_model, 2,
               "univariate","diagnostics")

# Altura
gen_graf_resid(as.matrix(tartarugas[-4]),fit_model, 3,
               "univariate","diagnostics")


# Zoom nos envelopes

# Comprimento
gen_graf_resid(as.matrix(tartarugas[-4]),fit_model, 1,
               "univariate","envelope")

# Largura
gen_graf_resid(as.matrix(tartarugas[-4]),fit_model, 2,
               "univariate","envelope")

# Altura
gen_graf_resid(as.matrix(tartarugas[-4]),fit_model, 3,
               "univariate","envelope")


# Resíduos multivariados


# Comprimento
gen_graf_resid(as.matrix(tartarugas[-4]),fit_model, 1,
               "multivariate","diagnostics")

# Largura
gen_graf_resid(as.matrix(tartarugas[-4]),fit_model, 2,
               "multivariate","diagnostics")

# Altura
gen_graf_resid(as.matrix(tartarugas[-4]),fit_model, 3,
               "multivariate","diagnostics")



# Comprimento
gen_graf_resid(as.matrix(tartarugas[-4]),fit_model, 1,
               "multivariate","envelope")

# Largura
gen_graf_resid(as.matrix(tartarugas[-4]),fit_model, 2,
               "multivariate","envelope")

# Altura
gen_graf_resid(as.matrix(tartarugas[-4]),fit_model, 3,
               "multivariate","envelope")



# Resíduo baseado na distância Mahalanobis
gen_graf_resid_quad_form(as.matrix(tartarugas[-4]),fit_model)