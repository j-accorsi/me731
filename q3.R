# Importando pacotes:
library(tidyverse)
library(readr)
library(MASS)

saudemental<-read_csv("dados/saudeMental.csv")

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
