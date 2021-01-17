#Importando pacotes:
library(tidyverse)
library(readr)
library(MASS)
library(ca)
library(factoextra)
library(xtable)

saudemental<-read_csv("dados/saudeMental.csv")
saudemental<-saudemental %>% 
  spread(name,value)
m.saude<-as.matrix(saudemental[,-1])
dimnames(m.saude)<-list(c(saudemental[,1])$saude,names(saudemental)[-1])
m.saude<-m.saude[c(1,3,4,2),]

################################################################################
############################# Analise Descritiva ###############################
################################################################################

grid.arrange(
  saudemental %>% group_by(saude) %>%
    summarise(n = sum(value), 
              saude = ifelse(saude == "Presença fraca de sintomas",
                             "Presença fraca\nde sintomas",
                             ifelse(saude == "Presença moderada de sintomas",
                                    "Presença moderada\nde sintomas",
                                    saude)))%>%
    ggplot(aes(saude,n)) +
    geom_bar(stat = "identity", fill = "#4DB620")+
    labs(y = "Frequência",
         x = "Estado da saúde mental") + 
    theme_bw(base_size = 18),
  saudemental %>% group_by(name) %>%
    summarise(n = sum(value)) %>%
    ggplot(aes(name,n)) +
    geom_bar(stat = "identity", fill = "#80009A")+
    labs(y = "",
         x = "Classe social") + 
    theme_bw(base_size = 18),
  nrow = 1, ncol = 2
)

################################################################################
############################## Analise Inferencial #############################
################################################################################

# Teste qui-quadrado:

chisq.test(saudemental[,-1])

# Perfil linha, coluna e matrix inercia:

ACaux <-function(m.X)     
{
  n.I <- nrow(m.X)
  n.J <- ncol(m.X)
  m.X.completa <- cbind(m.X,apply(m.X,1,sum))
  m.X.completa <- rbind(m.X.completa,apply(m.X.completa,2,sum))
  
  # Matriz de proporções
  m.P <-  m.X.completa/sum(m.X)
  
  # Vetor Pr e Pc
  P.r <- cbind(m.P[,n.J+1])
  P.c <- cbind(m.P[n.I+1,])
  
  # Matrizes Dr e Dc
  D.r <- diag(c(P.r),n.I+1,n.I+1)
  D.c <- diag(c(P.c),n.J+1,n.J+1)
  
  # Perfis das linhas e colunas
  m.R <- solve(D.r)%*%m.P
  m.C <- t(solve(D.c)%*%t(m.P))
  round(m.R*100,2)
  
  #t(round(m.C*100,2))
  
  m.P.aux <- m.P[1:n.I,1:n.J]
  P.c.aux <- cbind(P.c[1:n.J,])
  P.r.aux <- cbind(P.r[1:n.I,])
  D.r.aux <- diag(sqrt(c(P.r.aux)),n.I,n.I)
  D.c.aux <- diag(sqrt(c(P.c.aux)),n.J,n.J)
  m.P.rc <- (solve(D.r.aux))%*%(m.P.aux - P.r.aux%*%t(P.c.aux))%*%(solve(D.c.aux))
  result.svd <- svd(m.P.rc)
  v.gamma <- cbind(result.svd$d)
  inercia <- (v.gamma^2)
  #round(cbind(v.gamma,inercia),4)
  
  # Valor singular é raiz quadrada do autovalor (positivo)
  eigen1 <- eigen(m.P.rc%*%t(m.P.rc))
  eigen2 <- eigen(t(m.P.rc)%*%(m.P.rc))
  m.Gamma <- diag(result.svd$d,min(n.I,n.J),min(n.I,n.J))
  m.U <- (result.svd$u)
  m.V <- (result.svd$v)
  
  # componentes
  m.PL <- (solve(D.r.aux)%*%m.U%*%(m.Gamma))
  m.PC <- (solve(D.c.aux)%*%m.V%*%(m.Gamma))
  m.FullLC <- rbind(m.PL,m.PC)
  #result.AC.inercia <- list(v.gamma=v.gamma,inercia=inercia)
  return(list(inercia=inercia,m.R=m.R,m.C=m.C))
}


# Matriz linha e coluna:

aux<-ACaux(m.saude)


saudeca<-ca(m.saude)

# Matriz de inércia:

aux$inercia<-summary(saudeca)$scree


# Bi-plot:


fviz_ca_biplot(saudeca,title = "")


