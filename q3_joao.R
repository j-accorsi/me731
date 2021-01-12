library(tidyverse)
library(readr)
library(MASS)
library(gridExtra)

saudemental<-read_csv("dados/saudeMental.csv")

################################################################################
############################# Analise Descritiva ###############################
################################################################################


# Variáveis sumárias
# medados <-rbind(apply(saudemental %>% group_by(saude) %>%   
#                         summarise(saude = sum(value)) %>%
#                         select(saude),2,mean),
#                 apply(saudemental %>% group_by(saude) %>%   
#                         summarise(saude = sum(value)) %>%
#                         select(saude),2,var),
#                 apply(saudemental %>% group_by(saude) %>%   
#                         summarise(saude = sum(value)) %>%
#                         select(saude),2,sd),
#                 100*apply(saudemental %>% group_by(saude) %>%   
#                             summarise(saude = sum(value)) %>%
#                             select(saude),2,sd)/
#                   apply(saudemental %>% group_by(saude) %>%   
#                           summarise(saude = sum(value)) %>%
#                           select(saude),2,mean),
#                 apply(saudemental %>% group_by(saude) %>%   
#                         summarise(saude = sum(value)) %>%
#                         select(saude),2,min),
#                 apply(saudemental %>% group_by(saude) %>%   
#                         summarise(saude = sum(value)) %>%
#                         select(saude),2,quantile,0.5),
#                 apply(saudemental %>% group_by(saude) %>%   
#                         summarise(saude = sum(value)) %>%
#                         select(saude),2,max))
# rownames(medados)<-c("Média","Var.","DP","CV(%)","Mínimo","Mediana","Máximo")
# 
# round(medados,2)
# 
# rbind(apply(saudemental %>% group_by(name) %>%   
#               summarise(classe =sum(value)) %>%
#               select(classe),2,mean),
#       apply(saudemental %>% group_by(name) %>%   
#               summarise(classe =sum(value)) %>%
#               select(classe),2,var),
#       apply(saudemental %>% group_by(name) %>%   
#               summarise(classe =sum(value)) %>%
#               select(classe),2,sd),
#       100*apply(saudemental %>% group_by(name) %>%   
#                   summarise(classe =sum(value)) %>%
#                   select(classe),2,sd)/
#         apply(saudemental %>% group_by(name) %>%   
#                 summarise(classe =sum(value)) %>%
#                 select(classe),2,mean),
#       apply(saudemental %>% group_by(name) %>%   
#               summarise(classe =sum(value)) %>%
#               select(classe),2,min),
#       apply(saudemental %>% group_by(name) %>%   
#               summarise(classe =sum(value)) %>%
#               select(classe),2,quantile,0.5),
#       apply(saudemental %>% group_by(name) %>%   
#               summarise(classe =sum(value)) %>%
#               select(classe),2,max))

saudemental %>% group_by(saude) %>%
  summarise(n = sum(value)) %>%
  ggplot(aes(saude,n)) +
  geom_bar(stat = "identity", fill = "#4DB620")+
  labs(y = "n",
       x = "Estado da saúde mental") + 
  theme_bw(base_size = 13)

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
    labs(y = "n",
         x = "Estado da saúde mental") + 
    theme_bw(base_size = 18),
  saudemental %>% group_by(name) %>%
    summarise(n = sum(value)) %>%
    ggplot(aes(name,n)) +
    geom_bar(stat = "identity", fill = "#80009A")+
    labs(y = "n",
         x = "Classe social") + 
    theme_bw(base_size = 18),
  nrow = 1, ncol = 2
)
