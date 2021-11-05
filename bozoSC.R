library(tidyverse)
library(readxl)
library(coefplot)
library(huxtable)
library(olsrr)
options("scipen"=100, "digits"=4)
library(knitr)
library(kableExtra)






#cRIANDO AS VARIÁVEIS HOBUS_2014 E HOBUS_2018 #### - IR COPIANDO E COLANDO NA BASE ####


baseTSE2014 <- read_csv("D:/Nova pasta/eleicoes_2018_sc/votacao_candidato_munzona_2014_SC.csv")
baseTSE2018 <- read_csv("D:/Nova pasta/eleicoes_2018_sc/votacao_candidato_munzona_2018_SC.csv")


estadual2014 <- baseTSE2014%>%
  filter(CD_MUNICIPIO == 80039)%>%
  filter(CD_CARGO == "7")
estadual2018 <- baseTSE2018%>%
  filter(CD_MUNICIPIO == 80039)%>%
  filter(CD_CARGO == "7")
table(estadual2014$NM_MUNICIPIO) # para testar
estadual2014 <- estadual2014 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2014%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))

estadual2018 <- estadual2018 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2018%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))
#

estadual2014 <- baseTSE2014%>%
  filter(CD_MUNICIPIO == 80055)%>%
  filter(CD_CARGO == "7")
estadual2018 <- baseTSE2018%>%
  filter(CD_MUNICIPIO == 80055)%>%
  filter(CD_CARGO == "7")
table(estadual2014$NM_MUNICIPIO) # para testar
estadual2014 <- estadual2014 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2014%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))

estadual2018 <- estadual2018 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2018%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))
#

estadual2014 <- baseTSE2014%>%
  filter(CD_MUNICIPIO == 80357)%>%
  filter(CD_CARGO == "7")
estadual2018 <- baseTSE2018%>%
  filter(CD_MUNICIPIO == 80357)%>%
  filter(CD_CARGO == "7")
table(estadual2014$NM_MUNICIPIO) # para testar
estadual2014 <- estadual2014 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2014%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))
estadual2018 <- estadual2018 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2018%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))

#

estadual2014 <- baseTSE2014%>%
  filter(CD_MUNICIPIO == 80373)%>%
  filter(CD_CARGO == "7")
estadual2018 <- baseTSE2018%>%
  filter(CD_MUNICIPIO == 80373)%>%
  filter(CD_CARGO == "7")
table(estadual2014$NM_MUNICIPIO) # para testar
estadual2014 <- estadual2014 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2014%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))
estadual2018 <- estadual2018 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2018%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))

#

estadual2014 <- baseTSE2014%>%
  filter(CD_MUNICIPIO == 	80420)%>%
  filter(CD_CARGO == "7")
estadual2018 <- baseTSE2018%>%
  filter(CD_MUNICIPIO == 	80420)%>%
  filter(CD_CARGO == "7")
table(estadual2014$NM_MUNICIPIO) # para testar
estadual2014 <- estadual2014 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2014%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))
estadual2018 <- estadual2018 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2018%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))


#

estadual2014 <- baseTSE2014%>%
  filter(CD_MUNICIPIO == 	81701)%>%
  filter(CD_CARGO == "7")
estadual2018 <- baseTSE2018%>%
  filter(CD_MUNICIPIO == 	81701)%>%
  filter(CD_CARGO == "7")
table(estadual2014$NM_MUNICIPIO) # para testar
estadual2014 <- estadual2014 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2014%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))
estadual2018 <- estadual2018 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2018%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))

#

estadual2014 <- baseTSE2014%>%
  filter(CD_MUNICIPIO == 	80993)%>%
  filter(CD_CARGO == "7")
estadual2018 <- baseTSE2018%>%
  filter(CD_MUNICIPIO == 	80993)%>%
  filter(CD_CARGO == "7")
table(estadual2014$NM_MUNICIPIO) # para testar
estadual2014 <- estadual2014 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2014%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))
estadual2018 <- estadual2018 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2018%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))

#

estadual2014 <- baseTSE2014%>%
  filter(CD_MUNICIPIO == 	81353)%>%
  filter(CD_CARGO == "7")
estadual2018 <- baseTSE2018%>%
  filter(CD_MUNICIPIO == 	81353)%>%
  filter(CD_CARGO == "7")
table(estadual2014$NM_MUNICIPIO) # para testar
estadual2014 <- estadual2014 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2014%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))
estadual2018 <- estadual2018 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2018%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))

#

estadual2014 <- baseTSE2014%>%
  filter(CD_MUNICIPIO == 	81450)%>%
  filter(CD_CARGO == "7")
estadual2018 <- baseTSE2018%>%
  filter(CD_MUNICIPIO == 	81450)%>%
  filter(CD_CARGO == "7")
table(estadual2014$NM_MUNICIPIO) # para testar
estadual2014 <- estadual2014 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2014%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))
estadual2018 <- estadual2018 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2018%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))

#

estadual2014 <- baseTSE2014%>%
  filter(CD_MUNICIPIO == 	81671)%>%
  filter(CD_CARGO == "7")
estadual2018 <- baseTSE2018%>%
  filter(CD_MUNICIPIO == 	81671)%>%
  filter(CD_CARGO == "7")
table(estadual2014$NM_MUNICIPIO) # para testar
estadual2014 <- estadual2014 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2014%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))
estadual2018 <- estadual2018 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2018%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))

#

estadual2014 <- baseTSE2014%>%
  filter(CD_MUNICIPIO == 	80101)%>%
  filter(CD_CARGO == "7")
estadual2018 <- baseTSE2018%>%
  filter(CD_MUNICIPIO == 	80101)%>%
  filter(CD_CARGO == "7")
table(estadual2014$NM_MUNICIPIO) # para testar
estadual2014 <- estadual2014 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2014%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))
estadual2018 <- estadual2018 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2018%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))

#

estadual2014 <- baseTSE2014%>%
  filter(CD_MUNICIPIO == 81876)%>%
  filter(CD_CARGO == "7")
estadual2018 <- baseTSE2018%>%
  filter(CD_MUNICIPIO == 	81876)%>%
  filter(CD_CARGO == "7")
table(estadual2014$NM_MUNICIPIO) # para testar
estadual2014 <- estadual2014 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2014%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))
estadual2018 <- estadual2018 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2018%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))

#

estadual2014 <- baseTSE2014%>%
  filter(CD_MUNICIPIO == 81957)%>%
  filter(CD_CARGO == "7")
estadual2018 <- baseTSE2018%>%
  filter(CD_MUNICIPIO == 81957)%>%
  filter(CD_CARGO == "7")
table(estadual2014$NM_MUNICIPIO) # para testar
estadual2014 <- estadual2014 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2014%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))
estadual2018 <- estadual2018 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2018%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))

# mirim DOCE 

estadual2014 <- baseTSE2014%>%
  filter(CD_MUNICIPIO == 	80365)%>%
  filter(CD_CARGO == "7")
estadual2018 <- baseTSE2018%>%
  filter(CD_MUNICIPIO == 	80365)%>%
  filter(CD_CARGO == "7")
table(estadual2014$NM_MUNICIPIO) # para testar
estadual2014 <- estadual2014 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2014%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))
estadual2018 <- estadual2018 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2018%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))

# 82490

estadual2014 <- baseTSE2014%>%
  filter(CD_MUNICIPIO == 	82490)%>%
  filter(CD_CARGO == "7")
estadual2018 <- baseTSE2018%>%
  filter(CD_MUNICIPIO == 	82490)%>%
  filter(CD_CARGO == "7")
table(estadual2014$NM_MUNICIPIO) # para testar
estadual2014 <- estadual2014 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2014%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))
estadual2018 <- estadual2018 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2018%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))

# 82694

estadual2014 <- baseTSE2014%>%
  filter(CD_MUNICIPIO == 	82694)%>%
  filter(CD_CARGO == "7")
estadual2018 <- baseTSE2018%>%
  filter(CD_MUNICIPIO == 	82694)%>%
  filter(CD_CARGO == "7")
table(estadual2014$NM_MUNICIPIO) # para testar
estadual2014 <- estadual2014 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2014%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))
estadual2018 <- estadual2018 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2018%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))

# 82759

estadual2014 <- baseTSE2014%>%
  filter(CD_MUNICIPIO == 	82759)%>%
  filter(CD_CARGO == "7")
estadual2018 <- baseTSE2018%>%
  filter(CD_MUNICIPIO == 	82759)%>%
  filter(CD_CARGO == "7")
table(estadual2014$NM_MUNICIPIO) # para testar
estadual2014 <- estadual2014 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2014%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))
estadual2018 <- estadual2018 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2018%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))

# 82775

estadual2014 <- baseTSE2014%>%
  filter(CD_MUNICIPIO == 	82775)%>%
  filter(CD_CARGO == "7")
estadual2018 <- baseTSE2018%>%
  filter(CD_MUNICIPIO == 	82775)%>%
  filter(CD_CARGO == "7")
table(estadual2014$NM_MUNICIPIO) # para testar
estadual2014 <- estadual2014 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2014%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))
estadual2018 <- estadual2018 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2018%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))

#	82856

estadual2014 <- baseTSE2014%>%
  filter(CD_MUNICIPIO == 	82856)%>%
  filter(CD_CARGO == "7")
estadual2018 <- baseTSE2018%>%
  filter(CD_MUNICIPIO == 	82856)%>%
  filter(CD_CARGO == "7")
table(estadual2014$NM_MUNICIPIO) # para testar
estadual2014 <- estadual2014 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2014%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))
estadual2018 <- estadual2018 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2018%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))

#82872

estadual2014 <- baseTSE2014%>%
  filter(CD_MUNICIPIO == 	82872)%>%
  filter(CD_CARGO == "7")
estadual2018 <- baseTSE2018%>%
  filter(CD_MUNICIPIO == 	82872)%>%
  filter(CD_CARGO == "7")
table(estadual2014$NM_MUNICIPIO) # para testar
estadual2014 <- estadual2014 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2014%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))
estadual2018 <- estadual2018 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2018%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))


# 82910

estadual2014 <- baseTSE2014%>%
  filter(CD_MUNICIPIO == 	82910)%>%
  filter(CD_CARGO == "7")
estadual2018 <- baseTSE2018%>%
  filter(CD_MUNICIPIO == 	82910)%>%
  filter(CD_CARGO == "7")
table(estadual2014$NM_MUNICIPIO) # para testar
estadual2014 <- estadual2014 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2014%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))
estadual2018 <- estadual2018 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2018%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))


# 	83011

estadual2014 <- baseTSE2014%>%
  filter(CD_MUNICIPIO == 	83011)%>%
  filter(CD_CARGO == "7")
estadual2018 <- baseTSE2018%>%
  filter(CD_MUNICIPIO == 	83011)%>%
  filter(CD_CARGO == "7")
table(estadual2014$NM_MUNICIPIO) # para testar
estadual2014 <- estadual2014 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2014%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))
estadual2018 <- estadual2018 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2018%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))

# 	80349

estadual2014 <- baseTSE2014%>%
  filter(CD_MUNICIPIO == 	80349)%>%
  filter(CD_CARGO == "7")
estadual2018 <- baseTSE2018%>%
  filter(CD_MUNICIPIO == 	80349)%>%
  filter(CD_CARGO == "7")
table(estadual2014$NM_MUNICIPIO) # para testar
estadual2014 <- estadual2014 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2014%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))
estadual2018 <- estadual2018 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2018%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))
# 	83518

estadual2014 <- baseTSE2014%>%
  filter(CD_MUNICIPIO == 	83518)%>%
  filter(CD_CARGO == "7")
estadual2018 <- baseTSE2018%>%
  filter(CD_MUNICIPIO == 	83518)%>%
  filter(CD_CARGO == "7")
table(estadual2014$NM_MUNICIPIO) # para testar
estadual2014 <- estadual2014 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2014%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))
estadual2018 <- estadual2018 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2018%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))

#  83658

estadual2014 <- baseTSE2014%>%
  filter(CD_MUNICIPIO == 	83658)%>%
  filter(CD_CARGO == "7")
estadual2018 <- baseTSE2018%>%
  filter(CD_MUNICIPIO == 	83658)%>%
  filter(CD_CARGO == "7")
table(estadual2014$NM_MUNICIPIO) # para testar
estadual2014 <- estadual2014 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2014%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))
estadual2018 <- estadual2018 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2018%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))

# 	83771

estadual2014 <- baseTSE2014%>%
  filter(CD_MUNICIPIO == 	83771)%>%
  filter(CD_CARGO == "7")
estadual2018 <- baseTSE2018%>%
  filter(CD_MUNICIPIO == 	83771)%>%
  filter(CD_CARGO == "7")
table(estadual2014$NM_MUNICIPIO) # para testar
estadual2014 <- estadual2014 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2014%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))
estadual2018 <- estadual2018 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2018%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))

# 80128

estadual2014 <- baseTSE2014%>%
  filter(CD_MUNICIPIO == 	80128)%>%
  filter(CD_CARGO == "7")
estadual2018 <- baseTSE2018%>%
  filter(CD_MUNICIPIO == 	80128)%>%
  filter(CD_CARGO == "7")
table(estadual2014$NM_MUNICIPIO) # para testar
estadual2014 <- estadual2014 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2014%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))
estadual2018 <- estadual2018 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2018%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))

# 83810

estadual2014 <- baseTSE2014%>%
  filter(CD_MUNICIPIO == 	83810)%>%
  filter(CD_CARGO == "7")
estadual2018 <- baseTSE2018%>%
  filter(CD_MUNICIPIO == 	83810)%>%
  filter(CD_CARGO == "7")
table(estadual2014$NM_MUNICIPIO) # para testar
estadual2014 <- estadual2014 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2014%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))
estadual2018 <- estadual2018 %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>% 
  mutate(perc= round(perc*100, 2))
tibble(estadual2018%>%
         arrange(desc(perc))%>%select(NM_URNA_CANDIDATO, SG_PARTIDO, perc
                                      , QT_VOTOS_NOMINAIS))







# ANALISE ####


base <- read_excel("dados.xlsx")
base$Bolsonaro2018 <- 100*base$Bolsonaro2018
base$Neves14 <- 100*base$Neves14


# bozo sc 2018 segundo turno = 75,92%
summary(base$Bolsonaro2018)


base <- base%>%
  mutate(maior_q_sc = case_when(Bolsonaro2018 >=75.92 ~ 1,
                                TRUE ~ 0))

base <- base%>%
  mutate(maior_q_sc_ = case_when(Bolsonaro2018 >=75.92 ~ "sim",
                                 TRUE ~ "não")) 
table(base$Bolsonaro2018) # para testar
table(base$maior_q_sc) # para testar
table(base$maior_q_sc_) # para testar nominal

b5 <- base %>% 
  dplyr::select(CIDADE, maior_q_sc_, Bolsonaro2018) %>% 
  arrange(desc(Bolsonaro2018))
b5 %>%
  kbl(caption = "Municipios do Alto Vale comparados a SC em 2018") %>%
  kable_classic(full_width = F, html_font = "Garamond")


t.test(masculino ~ maior_q_sc, data = base)
t.test(superior_comp ~ maior_q_sc, data = base)
t.test(Ciro2018 ~ maior_q_sc, data = base)
t.test(Lula2006 ~ maior_q_sc, data = base)
t.test(Neves14 ~ maior_q_sc, data = base)
t.test(pop ~ maior_q_sc, data = base)
prop.test(table(base$MDB_pref, base$maior_q_sc)) #nop
chisq.test(base$MDB_pref, base$maior_q_scO) # nop


modelo1 <- glm(maior_q_sc ~  Neves14, data = base, family=binomial(link=logit))
summary(modelo1)



modelo2 <- glm(maior_q_sc ~  Neves14 + Ciro2018 + pt_ganhou_2014 + MDB_pref, data = base, family=binomial(link=logit))
summary(modelo2)


huxreg(modelo1, modelo2, stars = c(`*` = 0.1, `**` = 0.05,
                                   `***` = 0.01))




#### modelo 3
cor(base$Hobus14, base$Hobus18)

modelo3 <- glm(maior_q_sc ~ Neves14 + Ciro2018 + MDB_pref + pt_ganhou_2014 + Hobus18, data = base, family=binomial(link=logit))
summary(modelo3)


huxreg(modelo1, modelo2, modelo3, stars = c(`*` = 0.1, `**` = 0.05,
                                            `***` = 0.01))




