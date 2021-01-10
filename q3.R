# Importando pacotes:

library(tidyverse)
library(readr)
library(MASS)

saudemental<-read_csv("dados/saudeMental.csv")
saudemental<-saudemental %>% 
  spread(name,value)

chisq.test(saudemental[,-1])

# Tem dependência entre as variáveis