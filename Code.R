
# Data for: "Alexa for the win: Electronic media usage and ideology in Brazil".


# Carregar ou instalar pacotes necessários:
if(require(plyr)==F)install.packages('plyr');require(plyr)
if(require(dplyr)==F)install.packages('dplyr');require(dplyr)
if(require(readxl)==F)install.packages('readxl');require(readxl)
if(require(ggplot2)==F)install.packages('ggplot2');require(ggplot2)
if(require(tidyverse)==F)install.packages('tidyverse');require(tidyverse)
if(require(gridExtra)==F)install.packages('gridExtra');require(gridExtra)

# Carregar base de 2018 e filtrar por Brasil:
setwd("") # !!!Defina diretório de trabalho!!!
base <- readRDS("Latinobarometro_2018_Esp_R_v20190303.rds")
basebr <- filter(base, IDENPA == "76")

# Filtrar a base para respondentes válidos nas variáveis do modelo:
# Primeiro modelo:
d <- c(0,1)
e <- c(0,1,2,3,4,5,6,7,8,9,10)
i <- c(1,2,3,4,5)
j <- c(1,2,3,4,5,6,7)
k <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)

base2018 <- filter(basebr, P19ST.G %in% d)
base2018 <- filter(base2018, P22ST %in% e)
base2018 <- filter(base2018, S1 %in% i)
base2018 <- filter(base2018, S6 %in% j)
base2018 <- filter(base2018, S10 %in% k)
base2018 <- filter(base2018, SEXO != -2)

saveRDS(base2018, file = "base2018.rds")

# Total respondentes = 956
mod1 <- glm(P19ST.G ~ P22ST + S1 + S6 + S10 + EDAD + SEXO,
            data = base2018, family = binomial)
summary(mod1)


---

  
## Segundo modelo, adicionar redes que usa:
# S12M.D = Redes sociales que utiliza: Twitter
# S12M.E =  Redes sociales que utiliza: Whatsapp   
base2018.2 <- filter(base2018, S12M.D %in% d)
base2018.2 <- filter(base2018.2, S12M.E %in% d)

# Total respondentes = 956
mod2 <- glm(P19ST.G ~ P22ST + SEXO + S1 + S6 + S10 + EDAD + S12M.D + S12M.E, 
            data = base2018.2, family = binomial)
summary(mod2)


---


# Terceiro modelo, adicionar satisfação com economia (P13STGBS.B), 
# confiança no governo (P15STGBSC.E), na mídia (P16NC.D) e
# apoio à democracia (P12STGBS):
  
filt <- c(1,2,3)
filt2 <- c(1,2,3,4)
base2018.3 <- filter(base2018.2, P16NC.D %in% filt2)
base2018.3 <- filter(base2018.3, P12STGBS %in% filt)
base2018.3 <- filter(base2018.3,P13STGBS.B %in% filt2)
base2018.3 <- filter(base2018.3,P15STGBSC.E %in% filt2)

# Total respondentes = 869 
mod3 <- glm(P19ST.G ~ P22ST + P13STGBS.B + P15STGBSC.E + 
                 P16NC.D + P12STGBS + SEXO + S1 + S6 +
                 S10 + EDAD + S12M.D + S12M.E, data = base2018.3, family = binomial)
summary(mod3)


---
  
  
# VIs:
# P22ST = Self-positioning in Left - Right scale
# S1 = Subjective social class
# S6 = Ethnicity or race with which you identify better
# S10 = Respondent Years/Type of education
# EDAD =  Age
# P13STGBS.B = Satisfaction with economy
# P15STGBSC.E = Confidence in the government
# P16NC.D = Trust in media
# P12STGBS = Support for democracy
# S12M.D = Redes sociales que utiliza: Twitter
# SM12.E =  Redes sociales que utiliza: Whatsapp 

# VD: 
# P19ST.G = Cómo se informa Ud. de los asuntos políticos: Medios electronicos/Internet

# Obs: Há um problema no livro de códigos do Latinobarómetro de 2018. Na
# variável para o uso de internet, não aparece o código P19ST.G. Foi possível 
# perceber que este era de fato o código para internet ao contar os 1 da variável 
# na base de dados e ver que batia com a contagem do relatório por país que o 
# latinobarómetro apresenta.

# Estes são os códigos para as variáveis que mensuram como os respondentes 
# se informam sobre política. Foram também testadas como VD. 

# P19ST.A = Não signif = Familia
# P19ST.B = Não signif = Amigos
# P19ST.C = Não signif = Colegas de trabalho
# P19ST.D = Não signif = Colegas de estudo
# P19ST.E = Não signif = Radio
# P19ST.F = Signif em 1 modelo = Jornais/revistas
# P19ST.H = Não signif = TV 
# P19NC.I = Não signif = Facebook 
# P19NC.J = Não signif = Twitter
# P19F.K = Não signif = Youtube 
# P19ST.L = Não signif = Outro 
# P19ST.M = Não signif = Nenhum

---

# Espectros ideológicos 2018:
    
# Idade média centro:
brasilcentro <- filter(base, P22ST == "5")
brasilcentro <- filter(brasilcentro, EDAD != -2)
summary(brasilcentro$EDAD)

# Idade média esquerda:
esq <- c(0,1,2,3,4)
brasilesq <- filter(base, P22ST %in% esq)
brasilesq <- filter(brasilesq, EDAD != -2)
summary(brasilesq$EDAD)

# Idade média direita:
dir <- c(6,7,8,9,10)
brasildir <- filter(base, P22ST %in% dir)
brasilesq <- filter(brasildir, EDAD != -2)
summary(brasildir$EDAD)

# Sexo centro:
brasilcentro <- filter(base, P22ST == "5")
brasilcentro <- filter(brasilcentro, SEXO != -2)
count(brasilcentro, SEXO == 1) # 53,3%

# Sexo esquerda:
esq <- c(0,1,2,3,4)
brasilesq <- filter(base, P22ST %in% esq)
brasilesq <- filter(brasilesq, SEXO != -2)
count(brasilesq, SEXO == 1) # 42,3%

# Sexo direita: 
dir <- c(6,7,8,9,10)
brasildir <- filter(base, P22ST %in% dir)
brasilesq <- filter(brasildir, SEXO != -2)
count(brasildir, SEXO == 1) # 54,6%

# Educação centro:
brasilcentro <- filter(base, P22ST == "5")
count(brasilcentro, REEDUC.1 == 3) # 26,3%

# Educação direita:
brasildir <- filter(base, P22ST %in% dir)
count(brasildir, REEDUC.1 == 3) # 19,2%

# Educação esquerda:
brasilesq <- filter(base, P22ST %in% esq)
count(brasilesq, REEDUC.1 == 3) # 14,41%

# Etnia/Raça centro:
mm <- c(4,5)
brasilcentro <- filter(base, P22ST == "5")
count(brasilcentro, S6 == 6) # Brancos =  36.3%
count(brasilcentro, S6 == 2) # Negros = 22.1%
count(brasilcentro, S6 %in% mm) # Mestiços e mulatos = 25.9%

# Etnia/Raça direita:
brasildir <- filter(base, P22ST %in% dir)
count(brasildir, S6 == 6) # Brancos =  40.4%
count(brasildir, S6 == 2) # Negros = 21%
count(brasildir, S6 %in% mm ) # Mestiços e mulatos = 19.5%

# Etnia/Raça esquerda:
brasilesq <- filter(base, P22ST %in% esq)
count(brasilesq, S6 == 6) # Brancos =  28.6%
count(brasilesq, S6 == 2) # Negros = 23.9%
count(brasilesq, S6 %in% mm ) # Mestiços e mulatos = 26.6%

# Classe social centro:
brasilcentro <- filter(base, P22ST == "5")
count(brasilcentro, S1 == 1) # Alta =  1.73%
count(brasilcentro, S1 == 2) # Média alta = 2.77%
count(brasilcentro, S1 == 3) # Média = 25.6% 
count(brasilcentro, S1 == 4) # Média baixa = 43.9%
count(brasilcentro, S1 == 5) # Baixa = 24.6%
summary(brasilcentro$S1) 

# Classe social direita:
brasildir <- filter(base, P22ST %in% dir)
count(brasildir, S1 == 1) # Alta =  1.65%
count(brasildir, S1 == 2) # Média alta = 6.3% 
count(brasildir, S1 == 3) # Média = 29.5% 
count(brasildir, S1 == 4) # Média baixa = 35.4% 
count(brasildir, S1 == 5) # Baixa = 25.4%
summary(brasildir$S1) 

# Classe social esquerda:
brasilesq <- filter(base, P22ST %in% esq)
count(brasilesq, S1 == 1) # Alta =  1.35%
count(brasilesq, S1 == 2) # Média alta = 2.03%
count(brasilesq, S1 == 3) # Média = 26.8% 
count(brasilesq, S1 == 4) # Média baixa = 37.8% 
count(brasilesq, S1 == 5) # Baixa = 30.4%
summary(brasilesq$S1) 

# Religião centro:
ev <- c(3,4,5)
brasilcentro <- filter(base, P22ST == "5")
count(brasilcentro, S5 == 1) # Católicos =  53.3%
count(brasilcentro, S5 %in% ev) # Evangélicos = 9.7%
summary(brasilcentro$S5) 

# Religião direita:
brasildir <- filter(base, P22ST %in% dir)
count(brasildir, S5 == 1) # Católicos =  58.9%
count(brasildir, S5 %in% ev) # Evangélicos = 5.6%
summary(brasildir$S5) 

# Religião esquerda:
brasilesq <- filter(base, P22ST %in% esq)
count(brasilesq, S5 == 1) # Católicos =  60.1%
count(brasilesq, S5 %in% ev) # Evangélicos = 6.8%
summary(brasilesq$S5) 

# Praticantes de religião centro:
prats <- c(1,2)
pratn <- c(3,4)
brasilcentro <- filter(base, P22ST == "5")
count(brasilcentro, S5A %in% prats) # Praticantes = 46%
count(brasilcentro, S5A %in% pratn) # Não muito/não = 41.5%

# Praticantes direita:
count(brasildir, S5A %in% prats) # Praticantes = 50%
count(brasildir, S5A %in% pratn) # Não muito/não = 39.7%

# Praticantes esquerda:
count(brasilesq, S5A %in% prats) # Praticantes = 44.1%
count(brasilesq, S5A %in% pratn) # Não muito/não = 41.4%


---
  
  
# Analisar esta relação em 2017:
base2017 <- load(file = "Latinobarometro2017Eng_v20180117.rdata")
base2017 <- filter(Latinobarometro2017Eng_v20180117, idenpa == "76")

# Filtrar respondentes válidos para as variáveis utilizadas: 
m <- c(0,1)
n <- c(0,1,2,3,4,5,6,7,8,9,10)
o <- c(1,2,3,4,5)
p <- c(1,2,3,4,5,6,7)
q <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)

base2017 <- filter(base2017, P15ST.H %in% m)
base2017 <- filter(base2017, P19STC %in% n)
base2017 <- filter(base2017, S1 %in% o)
base2017 <- filter(base2017, edad != -2)
base2017 <- filter(base2017, S10 %in% p)
base2017 <- filter(base2017, S14 %in% q)
base2017 <- filter(base2017, sexo != -2)
saveRDS(base2017, file = "base2017.rds")

# Total respondentes = 854
mod4 <- glm(P15ST.H ~ P19STC + S1 + S10 + edad + S14 + sexo,
               data = base2017, family = binomial)
summary(mod4)

# VD:
# P15ST.H =  ¿Cómo se informa Ud. de los asuntos políticos? Medios electrónicos/internet.

# VIs:
# P19STC = Self-positioning in Left- Right scale
# edad = age
# S10 = Ethnicity or race with which you identify better
# S1 = Subjective social class
# S14 = Respondent Years/Type of education


---

  
base2016 <- load("Latinobarometro2016Esp_v20170205.rdata")
base2016 <- filter(Latinobarometro2016Esp_v20170205, idenpa == "76")

r <- c(0,1,2,3,4,5,6,7,8,9,10)
s <- c(1,2,3,4,5,6,7)
t <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
w <- c(1,2,3,4)
v <- c(0,1)

base2016 <- filter(base2016, P17ST %in% r)
base2016 <- filter(base2016, S9 %in% s)
base2016 <- filter(base2016, S13 %in% t)
base2016 <- filter(base2016, S4 %in% w)
base2016 <- filter(base2016, edad != -2)
base2016 <- filter(base2016, P26STH %in% v)
base2016 <- filter(base2016, sexo != -2)
saveRDS(base2016, file = "base2016.rds")

# Total respondentes = 974
mod5 <- glm(P26STH ~ P17ST + S9 + S13 + S4 + edad + sexo, data = base2016, 
            family = binomial)
summary(mod5)

# VD:
# P26STH = ¿Como se informa Ud. de los asuntos polticos? Medios electronicos/internet. 

# VIs:
# P17ST = Self-positioning in Left - Right scale (0 a 10)
# S9 = Ethnicity or race with which you identify better (1 a 7)  
# S13 = Respondent Years/Type of education (1 a 17)  
# S4 = Subjective Income (1 a 4)  
# edad = Age  


---  
  

# Quantos usaram meios eletrônicos para informação sobre política 2016,2017,2018 (nas bases filtradas):
count(base2016, P26STH == 1) # 258 - 26,5%
count(base2017, P15ST.H == 1) # 229 - 26,8%
count(base2018, P19ST.G == 1) # 415 - 43,4%


---


# Neste modelo, os eleitores que se declaram de centro (5) foram excluídos da
# amostra, para analisar a hipótese de que estes poderiam estar influenciando 
# o resultado encontrado. Ainda assim, o coeficiente para ideologia não perdeu
# significância.
  # Replicar modelo 1:  
basesemcentro <- filter(base2018, P22ST != 5)
mod6 <- glm(P19ST.G ~ P22ST + S1 + S6 + S10 + EDAD + SEXO,
            data = basesemcentro, family = binomial)
summary(mod6)
  # Replicar modelo 3:
basesemcentro2 <- filter(base2018.3, P22ST != 5)
mod6b <- glm(P19ST.F ~ P22ST + P13STGBS.B + P15STGBSC.E + 
                  P16NC.D + P12STGBS + SEXO + S1 + S6 +
                  S10 + EDAD + S12M.D + S12M.E, data = basesemcentro2,
                  family = binomial)
summary(mod6b)


# Nestes dois modelos foram separados 2 grupos, com escores de 0 a 4 (esquerda, n = 404) 
# e 6 a 10 (direita, n = 275). Os modelos apontam que a ideologia influencia a probabilidade
# de buscar informações na internet para o grupo da direita, mas não é significativa no grupo
# da esquerda.

# Filtrar base para direita:
dir <- c(6,7,8,9,10)
basedir <- filter(base2018, P22ST %in% dir)

# Total respondentes = 275
mod7 <- glm(P19ST.G ~ P22ST + S1 + S6 + S10 + EDAD + SEXO + S12M.E + S12M.D,
              data = basedir, family = binomial)
summary(mod7)

# Filtrar base para a esquerda:
esq <- c(0,1,2,3,4)
baseesq <- filter(base2018, P22ST %in% esq)

# Total respondentes = 404
mod8 <- glm(P19ST.G ~ P22ST + S1 + S6 + S10 + EDAD + SEXO + S12M.E + S12M.D,
                      data = baseesq, family = binomial)
summary(mod8)


---
  
  
# Comparar Brasil com outros países que tiveram eleições presidenciais em 2018:
  
# Brasil: 
brasil2 <- filter(base, IDENPA == "76")
d <- c(0,1)
e <- c(0,1,2,3,4,5,6,7,8,9,10)
i <- c(1,2,3,4,5)
j <- c(1,2,3,4,5,6,7)
k <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
ac <- c(1,2)

basebr2 <- filter(brasil2, P19ST.G %in% d)
basebr2 <- filter(basebr2, P22ST %in% e)
basebr2 <- filter(basebr2, S1 %in% i)
basebr2 <- filter(basebr2, S6 %in% j)
basebr2 <- filter(basebr2, S10 %in% k)
basebr2 <- filter(basebr2, SEXO != -2)
basebr2 <- filter(basebr2, EDAD != -2)
basebr2 <- filter(basebr2, S21.O %in% ac)

# Total respondentes = 956
modbr2 <- glm(P19ST.G ~ P22ST + S1 + S6 + S10 + EDAD + SEXO + S21.O,
             data = basebr2, family = binomial)
summary(modbr2)


---

  
# Costa Rica:
costarica <- filter(base, IDENPA == "188")
d <- c(0,1)
e <- c(0,1,2,3,4,5,6,7,8,9,10)
i <- c(1,2,3,4,5)
j <- c(1,2,3,4,5,6,7)
k <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
ac <- c(1,2)
require(dplyr)
basecr <- filter(costarica, P19ST.G %in% d)
basecr <- filter(basecr, P22ST %in% e)
basecr <- filter(basecr, EDAD != -2)
basecr <- filter(basecr, S1 %in% i)
basecr <- filter(basecr, S6 %in% j)
basecr <- filter(basecr, S10 %in% k)
basecr <- filter(basecr, SEXO != -2)
basecr <- filter(basecr, S21.O %in% ac)

# Total respondentes = 740
modcr <- glm(P19ST.G ~ P22ST + S1 + S6 + S10 + EDAD + SEXO + S21.O,
             data = basecr, family = binomial)
summary(modcr)


---

  
# Mexico:
mexico <- filter(base, IDENPA == "484")
d <- c(0,1)
e <- c(0,1,2,3,4,5,6,7,8,9,10)
i <- c(1,2,3,4,5)
j <- c(1,2,3,4,5,6,7)
k <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
ac <- c(1,2)
require(dplyr)
basemx <- filter(mexico, P19ST.G %in% d)
basemx <- filter(basemx, P22ST %in% e)
basemx <- filter(basemx, EDAD != -2)
basemx <- filter(basemx, S1 %in% i)
basemx <- filter(basemx, S6 %in% j)
basemx <- filter(basemx, S10 %in% k)
basemx <- filter(basemx, SEXO != -2)
basemx <- filter(basemx, S21.O %in% ac)

# Total respondentes = 673
modmx <- glm(P19ST.G ~ P22ST + S1 + S6 + S10 + EDAD + SEXO + S21.O,
             data = basemx, family = binomial)
summary(modmx)


---
  

# Colombia:  
colombia <- filter(base, IDENPA == "170")
d <- c(0,1)
e <- c(0,1,2,3,4,5,6,7,8,9,10)
i <- c(1,2,3,4,5)
j <- c(1,2,3,4,5,6,7)
k <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
ac <- c(1,2)
require(dplyr)
basecolom <- filter(colombia, P19ST.G %in% d)
basecolom <- filter(basecolom, P22ST %in% e)
basecolom <- filter(basecolom, EDAD != -2)
basecolom <- filter(basecolom, S1 %in% i)
basecolom <- filter(basecolom, S6 %in% j)
basecolom <- filter(basecolom, S10 %in% k)
basecolom <- filter(basecolom, SEXO != -2)
basecolom <- filter(basecolom, S21.O %in% ac)

# Total respondentes = 947
modcolom <- glm(P19ST.G ~ P22ST + S1 + S6 + S10 + EDAD + SEXO + S21.O,
                data = basecolom, family = binomial)
summary(modcolom)


---
  
  
# Paraguai:
paraguay <- filter(base, IDENPA == "600")
d <- c(0,1)
e <- c(0,1,2,3,4,5,6,7,8,9,10)
i <- c(1,2,3,4,5)
j <- c(1,2,3,4,5,6,7)
k <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
ac <- c(1,2)
require(dplyr)
basepar <- filter(paraguay, P19ST.G %in% d)
basepar <- filter(basepar, P22ST %in% e)
basepar <- filter(basepar, EDAD != -2)
basepar <- filter(basepar, S1 %in% i)
basepar <- filter(basepar, S6 %in% j)
basepar <- filter(basepar, S10 %in% k)
basepar <- filter(basepar, SEXO != -2)
basepar <- filter(basepar, S21.O %in% ac)

# Total respondentes = 735
modpar <- glm(P19ST.G ~ P22ST + S1 + S6 + S10 + EDAD + SEXO + S21.O,
              data = basepar, family = binomial)
summary(modpar)


---
  

# Gráfico ideologia:  

# Carregar bases:
load("AvgIdeologyBrazil.RData")
AvgDiff <- read_xlsx("AvgDiff.xlsx")

# Plotar médias esquerda/direita:
windowsFonts(Times=windowsFont("Times New Roman"))
p2 <- ggplot(data = AvgIdeology, aes(x=Year, y=AverageIdeology,linetype = Side)) + 
geom_line()+
theme_grey()+
theme(text=element_text(family="Times", size=12))+
theme(legend.position="bottom")+
ylim(0,10)+
scale_x_continuous(limit=c(2002, 2018),breaks=c(2002,2010,2018))+
labs(y = "Average Ideology Score", x= "Year", linetype = "")+
scale_color_manual(values = c("#737373","#252525"))

# Gráfico distância média:
windowsFonts(Times=windowsFont("Times New Roman"))
p1 <- ggplot(data = AvgDiff, aes(x=Year, y=Diff)) + 
  geom_line()+
  geom_point()+
  theme_grey()+
  theme(text=element_text(family="Times", size=12))+
  ylim(0,10)+
  scale_x_continuous(limit=c(2002, 2018),breaks=c(2002,2010,2018))+
  labs(y = "Average Distance", x= "Year")+
  scale_color_manual(values = c("#737373"))

grid.arrange(p2, p1, nrow=2)


---
  
  
# Último teste para 2018:
  
# Adicionar variável "Info", que é a média do percentual dos entrevistados que 
# responderam 1 para cada uma das 12 categorias sobre busca de informação em
# cada um dos escores da escala de ideologia:
 
base2018.4 <- mutate(base2018.3, Info = ifelse(P22ST == 0, "17.83",
 ifelse(P22ST == 1, "21.14",ifelse(P22ST == 2, "22.78",ifelse(P22ST == 3, "19.62",
 ifelse(P22ST == 4, "22.99",ifelse(P22ST == 5, "23.96",ifelse(P22ST == 6, "21.92",
 ifelse(P22ST == 7, "23.21",ifelse(P22ST == 8, "22.22",ifelse(P22ST == 9, "19.75",
 ifelse(P22ST == 10, "19.29", "NA"))))))))))))

# Adicionar variável "Size", que corresponde à quantidade de respondentes que
# se declararam como pertencentes a um número X na escala de ideologia:

base2018.4 <- mutate(base2018.4, Size = ifelse(P22ST == 0, "143",
ifelse(P22ST == 1, "54",ifelse(P22ST == 2, "60",ifelse(P22ST == 3, "79",
ifelse(P22ST == 4, "108",ifelse(P22ST == 5, "289",ifelse(P22ST == 6, "73",
ifelse(P22ST == 7, "56",ifelse(P22ST == 8, "57",ifelse(P22ST == 9, "27",
ifelse(P22ST == 10, "89", "NA"))))))))))))

base2018.4$Info <- as.numeric(as.character(base2018.4$Info))
base2018.4$Size <- as.numeric(as.character(base2018.4$Size))

# Adicionar variáveis ao modelo 3:
# Total respondentes = 869 
mod9 <- glm(P19ST.G ~ P22ST + P13STGBS.B + P15STGBSC.E + P16NC.D + P12STGBS +
              SEXO + S1 + S6 + Info + Size + S10 + EDAD + S12M.D + S12M.E, 
            data = base2018.4, family = binomial)
summary(mod9)

# Replicar modelo sem o centro:
# Total respondentes = 613
base2018.5 <- filter(base2018.4, P22ST != 5)
mod10 <- glm(P19ST.G ~ P22ST + P13STGBS.B + P15STGBSC.E + P16NC.D + P12STGBS +
              SEXO + S1 + S6 + Info + Size + S10 + EDAD + S12M.D + S12M.E, 
            data = base2018.5, family = binomial)
summary(mod10)
