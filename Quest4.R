library(tidyverse)
library(mlbench)
# limpar todas as variáveis 
rm(list = ls(all.names = TRUE))

library(MASS)
library(CCA)
library(car)
library(reshape2)


set.seed(4142)
data(PimaIndiansDiabetes2)
PimaIndiansDiabetes2
Pima <- drop_na(PimaIndiansDiabetes2)
# Iris de Fisher
iris
diabetes <- Pima[,9]

diabetesaux<-revalue(diabetes,c("neg"="N","pos"="P"))

inames <- c("comprimento da sépala","largura da sépala","comprimento da pétala","largura da pétala")
Pimad <- Pima[,1:9]
Pima <- Pimaind %>% dplyr::select(-pedigree)
colnames(Pimad)<-c("Paciente","Glicose","Pressão","Tríceps", "Insulina", "IMC", "Idade", "Diabetes")
Pimaaux <- Pima
colnames(Pimaaux)<-c("Paciente","Glicose","Pressão","Tríceps", "Insulina", "IMC", "Idade", "Diabetes")

# Densidades suavizadas
par(mfrow=c(2,2))
plot(density(Pimaaux[diabtes=="pos",1]),lwd=2,xlim=c(min(Pimaaux[,1])-1,max(Pimaaux[,1])+1),xlab="comprimento da sépala",ylab="densidade",cex=1.2,cex.lab=1.2,cex.main=1.2,main="CS")
lines(density(Pimaaux[diabetes=="neg",1]),col=2,lwd=2)
#lines(density(irisaux[especie=="virginica",1]),col=3,lwd=2)
legend(7.5,0.5,lwd=c(2,2,2),col=c(1,2,3),legend=c("pos","neg"),bty="n",cex=0.7)
#
plot(density(Pimaaux[diabetes=="pos",2]),lwd=2,ylim=c(0,1.4),xlim=c(min(Pimaaux[,2])-1,max(Pimaaux[,2])+1.5),xlab="largura da sépala",ylab="densidade",cex=1.2,cex.lab=1.2,cex.main=1.2,main="LS")
lines(density(Pimaaux[diabetes=="neg",2]),col=2,lwd=2)
#lines(density(Pimaaux[diabetes=="virginica",2]),col=3,lwd=2)
legend(4.5,0.6,lwd=c(2,2,2),col=c(1,2,3),legend=c("pos","neg"),bty="n",cex=0.7)
#
plot(density(Pimaaux[diabetes=="pos",3]),lwd=2,xlim=c(min(Pimaaux[,3])-1,max(Pimaaux[,3])+1),xlab="comprimento da pétala",ylab="densidade",cex=1.2,cex.lab=1.2,cex.main=1.2,main="CP")
lines(density(Pimaaux[diabetes=="neg",3]),col=2,lwd=2)
#lines(density(irisaux[especie=="virginica",3]),col=3,lwd=2)
legend(6,2,lwd=c(2,2,2),col=c(1,2,3),legend=c("pos","neg"),bty="n",cex=0.7)
#
plot(density(Pimaaux[diabetes=="pos",4]),lwd=2,xlim=c(min(Pimaaux[,4])-1,max(Pimaaux[,4])+1),xlab="largura da pétala",ylab="densidade",cex=1.2,cex.lab=1.2,cex.main=1.2,main="LP")
lines(density(Pimaaux[diabetes=="neg",4]),col=2,lwd=2)
#lines(density(irisaux[especie=="virginica",4]),col=3,lwd=2)
legend(2.5,6.5,lwd=c(2,2,2),col=c(1,2,3),legend=c("pos","neg"),bty="n",cex=0.7)
dev.off()
#
# Gráficos das distâncias de Mahalanobis

par(mfrow=c(1,1))
mxP <- Pimad[diabetes=="pos",]
nP <- nrow(mxP)
vmuP <- apply(mxP,2,mean)
s2P <- cov(mxP)
mmuP <- t(matrix(t(vmuP),4,nP))
#vQS <- nS*apply(as.matrix((mxS-vmuS)*(mxS-vmuS))%*%as.matrix(solve(s2S)),1,sum)
# vQS1<-nS*apply(as.matrix(mxS-mmuS)*(as.matrix(mxS-mmuS)%*%as.matrix(solve(s2S))),1,sum)
vQP<-nP*mahalanobis(mxP,center=vmuP,cov=s2P)

# nS*apply(as.matrix((mxS-mmuS)*(mxS-mmuS))%*%as.matrix(solve(s2S)),1,sum)
#
mxN <- Pimad[diabetes=="neg",]
nN <- nrow(mxN)
vmuN <- apply(mxN,2,mean)
s2N <- cov(mxN)
mmuN <- t(matrix(t(vmuN),4,nN))
#vQV <- nS*apply(as.matrix((mxV-mmuV)*(mxV-mmuV))%*%as.matrix(solve(s2V)),1,sum)
#vQV <- nV*apply(as.matrix(mxV-mmuV)*(as.matrix(mxV-mmuV)%*%as.matrix(solve(s2V))),1,sum)
vQN<-nN*mahalanobis(mxN,center=vmuN,cov=s2N)
#
#mxVi <- irisd[especie=="virginica",]
#nVi <- nrow(mxVi)
#vmuVi <- apply(mxVi,2,mean)
#s2Vi <- cov(mxVi)
#mmuVi <- t(matrix(t(vmuVi),4,nVi))
#vQVi <- nVi*apply(as.matrix((mxVi-mmuVi)*(mxVi-mmuVi))%*%as.matrix(solve(s2Vi)),1,sum)
#vQVi<-nVi*mahalanobis(mxVi,center=vmuVi,cov=s2Vi)
#
plot(density(vQP),lwd=2,xlab="valor",main="distância de Mahalanobis",ylab="densidade",cex=1.2,cex.lab=1.2,cex.main=1.2,ylim=c(0,0.004))
lines(density(vQN),lwd=2,cex=1.2,cex.lab=1.2,cex.main=1.2,col=2)
#lines(density(vQVi),lwd=2,cex=1.2,cex.lab=1.2,cex.main=1.2,col=3)
legend(300,0.003,lwd=c(2,2,2),col=c(1,2,3),legend=c("pos","neg"),bty="n",cex=1.2)
dev.off()
#

######################################################
# Trabalhando com dois grupos (versicolor e virginica)

# Selecionando as amostras
treinog1 <- sort(sample(1:50,25,replace=FALSE))
treinog2 <- sort(sample(51:100,25,replace=FALSE))
treino <- c(treinog1,treinog2)

# Amostra treino
#atrein <- iris[treino,]

# Utilizando a função lda
m.X <- rbind(Pimad[51:100,1:4],Pimad[101:150,1:4])
Sp = rep(c("pos","neg"), rep(50,2))
m.Pima <- data.frame(m.X,Sp)
table(m.Pima$Sp[treino])
#result.ad <- lda(Sp~., m.Iris, prior = c(1,1)/2,subset = treino)
result.ad <- lda(Sp~., m.Pima[,1:4], prior = c(1,1)/2,subset = treino)
result.ad
# coeficientes da função discriminante
# result.ad$scaling
# predizendo os grupos na amostra teste
pred<-predict(result.ad, m.Pima[-treino, ])$class
# função discriminante
y <-predict(result.ad, m.Pima[-treino, ])$x 
# Pegando a amostra teste
data.teste <- m.Pima[-treino,5]
# Tabela de classificação
tc <- table(data.teste,pred)
xtable(tc)

# TEA
TEA <- (tc[1,2]+tc[2,1])/sum(tc)
100*TEA

# TOE
m.dados.Pima <- m.Pima[treino,]
#  Dados.1 <- m.dados.iris[m.dados.iris[,5]=="VE",][,]
#  Dados.2 <- m.dados.iris[m.dados.iris[,5]=="VI",][,]
Dados.1 <- (m.dados.Pima[m.dados.Pima[,5]=="pos",][,1:4])
Dados.2 <- (m.dados.Pima[m.dados.Pima[,5]=="neg",][,1:4])
v.mean1 <- cbind(c(result.ad$means[1,]))
v.mean2 <- cbind(c(result.ad$means[2,]))
S21 <- cov(Dados.1[,1:4])
S22 <- cov(Dados.2[,1:4])
Sp <- ((nrow(Dados.1)-1)*S21 +  ((nrow(Dados.2)-1)*S22))/(nrow(Dados.1) + nrow(Dados.2) - 2)
q.classifi <- 0.5*t(v.mean1-v.mean2)%*%solve(Sp)%*%(v.mean1+v.mean2)
delta2 <-  t(v.mean1-v.mean2)%*%solve(Sp)%*%(v.mean1 - v.mean2)
#
TOE <- pnorm(-sqrt(delta2)/2)
100*TOE
xtable(cbind(v.mean1,v.mean2))
xtable(S21)
xtable(S22)


# m = 0.5*(t(v.mean1-v.mean2))%*%solve(Sp)%*%((v.mean1+v.mean2))

# Função linear discriminante

# Y <- c(t(v.mean1-v.mean2)%*%solve(Sp)%*%t(as.matrix(m.Iris[-treino,1:4])))
# b <- t(v.mean1-v.mean2)%*%solve(Sp)
# ba<-b/drop(sqrt(b%*%Sp%*%t(b)))
# m = 0.5*(t(v.mean1-v.mean2))%*%solve(Sp)%*%((v.mean1+v.mean2))

# Comparação entre os coeficientes Coeficientes
# cbind(t(t(v.mean1-v.mean2)%*%solve(Sp)),result.ad$scaling)
# (t(t(v.mean1-v.mean2)%*%solve(Sp)))/result.ad$scaling

# Análise da função discriminante para a amostra teste
grupo <- as.factor(rep(c("pos","neg"), rep(25,2)))
# Medidas resumo
datadados<-data.frame(y,grupo)
colnames(datadados)<-c("y","dados")
medados<-ddply(datadados,.(grupo),summarise,media=mean(y),dp=sqrt(var(y)),vari=var(y),minimo=min(y),mediana=quantile(y,0.5),maximo=max(y),n=length(y))
colnames(medados)<-c("Grupo","Média","DP","Var.","Mínimo","Mediana","Máximo","n")
xtable(medados)

#
# boxplot

par(mfrow=c(1,1))
boxplot(y~grupo,cex=1.2,cex.lab=1.2)
dev.off()
# densidades

par(mfrow=c(1,1))
plot(density(y[grupo=="pos",1]),lwd=2,xlim=c(min(y[,1])-1,max(y[,1])+3),xlab="função discriminante",ylab="densidade",cex=1.2,cex.lab=1.2,cex.main=1.2,main="",ylim=c(0,0.60))
lines(density(y[grupo=="neg",1]),col=2,lwd=2)
legend(3,0.45,lwd=c(2,2),col=c(1,2),legend=c("pos","neg"),bty="n",cex=1.2)
dev.off()

#hist(y[grupo=="Versicolor"],probability=TRUE,cex=1.2,cex.lab=1.2)
#hist(y[grupo=="Virginica"],probability=TRUE,cex=1.2,cex.lab=1.2)