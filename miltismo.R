library(tidyverse)
library(readxl)
library(coefplot)
library(huxtable)
library(olsrr)
options("scipen"=100, "digits"=4)
library(knitr)
library(kableExtra)
library(polycor) #pacote exigido
base <- read_excel("dados.xlsx")
base$Bolsonaro2018 <- 100*base$Bolsonaro2018
base$Neves14 <- 100*base$Neves14

base$aumento_bolso <- base$Bolsonaro2018 - base$Neves14


# verificar explicações para o voto em Hpobus 2018 ( e a queda)


cor.test(base$Neves14, base$Hobus18)
cor.test(base$Bolsonaro2018, base$Hobus18)
cor.test(base$Hobus14, base$Hobus18)
polyserial(base$MDB_pref, base$Hobus18)
polyserial(base$pt_ganhou_2014, base$Hobus18)
cor.test(base$aumento_bolso, base$Hobus18)
cor.test(base$pop, base$Hobus18)
cor.test(base$Lula2006, base$Hobus18)


modelo1 <- lm(Hobus18 ~ Hobus14, data =base) 
summary(modelo1)

modelo2 <- lm(Hobus18 ~ Hobus14 + Bolsonaro2018 + Neves14 + MDB_pref + pt_ganhou_2014 + pop, data = base)
summary(modelo2)

huxreg(modelo1, modelo2)

# analisando a diferença

base$diferenca <- base$Hobus18 - base$Hobus14
boxplot(base$diferenca)
summary(base$diferenca)


cor.test(base$diferenca, base$Bolsonaro2018)
cor.test(base$diferenca, base$pop)
cor(base$MDB_pref, base$miltismo)
polyserial(base$MDB_pref, base$diferenca)
cor(base$pt_ganhou_2014, base$mdiferenca)
polyserial(base$pt_ganhou_2014, base$diferenca)
cor(base$Lula2006, base$diferenca)

cor.test(base$aumento_bolso, base$diferenca)



#modelos com diferença sem mexer
modelo3 <- lm(diferenca ~  Bolsonaro2018, data = base)
summary(modelo3)
modelo4 <- lm(diferenca ~ Bolsonaro2018 + pop + MDB_pref + pt_ganhou_2014, data = base)
summary(modelo4)

huxreg(modelo1, modelo2, modelo3, modelo4, stars = c(`*` = 0.1, `**` = 0.05,
                                   `***` = 0.01), statistics = c("N. obs." = "nobs", "R2" = "r.squared",
                                                                 "AIC" = "AIC"))
coefplot(modelo2, intercept = F)
coefplot(modelo4, intercept = F)



# em 14 era mais imbricado ao antipetismo
cor.test(base$Hobus14, base$Neves14)
cor.test(base$Hobus18, base$Bolsonaro2018)
cor.test(base$Hobus14, base$Lula2006)
cor.test(base$Hobus18, base$Lula2006)
cor.test(base$Hobus14, base$pop)
cor.test(base$Hobus18, base$pop)

modelo_14 <- lm(Hobus14 ~ Neves14 + Lula2006 + pop, data = base)
summary(modelo_14)
modelo_18 <- lm(Hobus18 ~ Bolsonaro2018 + Lula2006 + pop, data = base)
summary(modelo_18)


huxreg(modelo_14, modelo_18, stars = c(`*` = 0.1, `**` = 0.05,
                                                     `***` = 0.01), statistics = c("N. obs." = "nobs", "R2" = "r.squared",
                                                                                   "AIC" = "AIC"))
coefplot(modelo_14, intercept = F)
coefplot(modelo_18, intercept = F)

b5 <- base %>% 
  dplyr::select(CIDADE, diferenca) %>% 
  arrange(desc(diferenca))
b5 %>%
  kbl(caption = "Municipios do Alto Vale comparados a SC em 2018") %>%
  kable_classic(full_width = F, html_font = "Garamond")


# a parte mais importante - uma variável de cada poder político
modelo66 <- lm(diferenca ~  Bolsonaro2018, data = base)

modelo64 <- lm(diferenca ~ Bolsonaro2018 + pop + MDB_pref + pt_ganhou_2014 + Ciro2018, data = base)


huxreg(modelo66, modelo64, stars = c(`*` = 0.1, `**` = 0.05,
                                                     `***` = 0.01), statistics = c("N. obs." = "nobs", "R2" = "r.squared",
                                                                                   "AIC" = "AIC"))


