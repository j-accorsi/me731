# Importando pacotes:

library(tidyverse)
library(readr)
library(MASS)

saudemental<-read_csv("dados/saudeMental.csv")
saudemental<-saudemental %>% 
  spread(name,value)

chisq.test(saudemental[,-1])

# Tem dependência entre as variáveis

perfil_linha<-saudemental %>% 
  gather(status,qtd,-1) %>% 
  group_by(saude) %>% 
  mutate(qtd=qtd/sum(qtd)) %>% 
  spread(status,qtd)
  
perfil_linha

perfil_coluna<-saudemental %>% 
  gather(status,qtd,-1) %>% 
  group_by(status) %>% 
  mutate(qtd=qtd/sum(qtd)) %>% 
  spread(status,qtd)

perfil_coluna
