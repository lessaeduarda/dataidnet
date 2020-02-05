
# Data for: "Alexa for the win: Electronic media usage and ideology in Brazil".


# Carregar ou instalar pacotes necessários:
if(require(plyr)==F)install.packages('plyr');require(plyr)
if(require(dplyr)==F)install.packages('dplyr');require(dplyr)
if(require(tidyr)==F)install.packages('tidyr');require(tidyr)
if(require(haven)==F)install.packages('haven');require(haven)
if(require(readxl)==F)install.packages('readxl');require(readxl)
if(require(ggpubr)==F)install.packages('ggpubr');require(ggpubr)
if(require(ggplot2)==F)install.packages('ggplot2');require(ggplot2)
if(require(magrittr)==F)install.packages('magrittr');require(magrittr)
if(require(tidyverse)==F)install.packages('tidyverse');require(tidyverse)
if(require(gridExtra)==F)install.packages('gridExtra');require(gridExtra)


# Carregar base de 2018 e filtrar por Brasil:
setwd("") # !!!Defina diretório de trabalho!!!
# Carregar base de 2018 e filtrar por Brasil:
base <- readRDS("Latinobarometro_2018_Esp_R_v20190303.rds")
basebr <- filter(base, IDENPA == "76")

# Filtrar a base para respondentes válidos nas variáveis do modelo:
# Primeiro modelo:
ac <- c(1,2)
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
base2018 <- filter(base2018, S21.O %in% ac)


saveRDS(base2018, file = "base2018.rds")

# Total respondentes = 956
mod1 <- glm(P19ST.G ~ P22ST + S1 + S6 + S10 + EDAD + SEXO + S21.O,
            data = base2018, family = binomial)
summary(mod1)


---

  
## Segundo modelo, adicionar redes que usa:
# S12M.D = Redes sociales que utiliza: Twitter
# S12M.E =  Redes sociales que utiliza: Whatsapp   
base2018.2 <- filter(base2018, S12M.D %in% d)
base2018.2 <- filter(base2018.2, S12M.E %in% d)

# Total respondentes = 956
mod2 <- glm(P19ST.G ~ P22ST + SEXO + S1 + S6 + S10 + EDAD + S12M.D + S12M.E + S21.O, 
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
                 P16NC.D + P12STGBS + SEXO + S1 + S6 + S21.O +
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
# S21.O = Possui internet em casa

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
# P19ST.F = Significante em 1 modelo = Jornais/revistas
# P19ST.H = Não signif = TV 
# P19NC.I = Não signif = Facebook 
# P19NC.J = Não signif = Twitter
# P19F.K = Não signif = Youtube 
# P19ST.L = Não signif = Outro 
# P19ST.M = Não signif = Nenhum

---

# Espectros ideológicos 2018:
    
# Idade média centro:
brasilcentro <- filter(basebr, P22ST == "5")
brasilcentro <- filter(brasilcentro, EDAD != -2)
summary(brasilcentro$EDAD)

# Idade média esquerda:
esq <- c(0,1,2,3,4)
brasilesq <- filter(basebr, P22ST %in% esq)
brasilesq <- filter(brasilesq, EDAD != -2)
summary(brasilesq$EDAD)

# Idade média direita:
dir <- c(6,7,8,9,10)
brasildir <- filter(basebr, P22ST %in% dir)
brasilesq <- filter(brasildir, EDAD != -2)
summary(brasildir$EDAD)

# Sexo centro:
brasilcentro <- filter(basebr, P22ST == "5")
brasilcentro <- filter(brasilcentro, SEXO != -2)
count(brasilcentro, SEXO == 1) # 53.3%

# Sexo esquerda:
esq <- c(0,1,2,3,4)
brasilesq <- filter(basebr, P22ST %in% esq)
brasilesq <- filter(brasilesq, SEXO != -2)
count(brasilesq, SEXO == 1) # 42.3%

# Sexo direita: 
dir <- c(6,7,8,9,10)
brasildir <- filter(basebr, P22ST %in% dir)
brasilesq <- filter(brasildir, SEXO != -2)
count(brasildir, SEXO == 1) # 54.6%

# Educação centro:
brasilcentro <- filter(basebr, P22ST == "5")
count(brasilcentro, REEDUC.1 == 3) # 26.3%

# Educação direita:
brasildir <- filter(basebr, P22ST %in% dir)
count(brasildir, REEDUC.1 == 3) # 19.2%

# Educação esquerda:
brasilesq <- filter(basebr, P22ST %in% esq)
count(brasilesq, REEDUC.1 == 3) # 14.41%

# Etnia/Raça centro:
mm <- c(4,5)
brasilcentro <- filter(basebr, P22ST == "5")
count(brasilcentro, S6 == 6) # Brancos =  36.3%
count(brasilcentro, S6 == 2) # Negros = 22.1%
count(brasilcentro, S6 %in% mm) # Mestiços e mulatos = 25.9%

# Etnia/Raça direita:
brasildir <- filter(basebr, P22ST %in% dir)
count(brasildir, S6 == 6) # Brancos =  40.4%
count(brasildir, S6 == 2) # Negros = 21%
count(brasildir, S6 %in% mm ) # Mestiços e mulatos = 19.5%

# Etnia/Raça esquerda:
brasilesq <- filter(basebr, P22ST %in% esq)
count(brasilesq, S6 == 6) # Brancos =  28.6%
count(brasilesq, S6 == 2) # Negros = 23.9%
count(brasilesq, S6 %in% mm ) # Mestiços e mulatos = 26.6%

# Classe social centro:
brasilcentro <- filter(basebr, P22ST == "5")
count(brasilcentro, S1 == 1) # Alta =  1.73%
count(brasilcentro, S1 == 2) # Média alta = 2.77%
count(brasilcentro, S1 == 3) # Média = 25.6% 
count(brasilcentro, S1 == 4) # Média baixa = 43.9%
count(brasilcentro, S1 == 5) # Baixa = 24.6%
summary(brasilcentro$S1) 

# Classe social direita:
brasildir <- filter(basebr, P22ST %in% dir)
count(brasildir, S1 == 1) # Alta =  1.65%
count(brasildir, S1 == 2) # Média alta = 6.3% 
count(brasildir, S1 == 3) # Média = 29.5% 
count(brasildir, S1 == 4) # Média baixa = 35.4% 
count(brasildir, S1 == 5) # Baixa = 25.4%
summary(brasildir$S1) 

# Classe social esquerda:
brasilesq <- filter(basebr, P22ST %in% esq)
count(brasilesq, S1 == 1) # Alta =  1.35%
count(brasilesq, S1 == 2) # Média alta = 2.03%
count(brasilesq, S1 == 3) # Média = 26.8% 
count(brasilesq, S1 == 4) # Média baixa = 37.8% 
count(brasilesq, S1 == 5) # Baixa = 30.4%
summary(brasilesq$S1) 


# Religião centro:
ev <- c(3,4,5)
brasilcentro <- filter(basebr, P22ST == "5")
count(brasilcentro, S5 == 1) # Católicos =  53.3%
count(brasilcentro, S5 %in% ev) # Evangélicos = 9.7%
summary(brasilcentro$S5) 

# Religião direita:
brasildir <- filter(basebr, P22ST %in% dir)
count(brasildir, S5 == 1) # Católicos =  58.9%
count(brasildir, S5 %in% ev) # Evangélicos = 5.6%
summary(brasildir$S5) 

# Religião esquerda:
brasilesq <- filter(basebr, P22ST %in% esq)
count(brasilesq, S5 == 1) # Católicos =  60.1%
count(brasilesq, S5 %in% ev) # Evangélicos = 6.8%
summary(brasilesq$S5) 

# Praticantes de religião centro:
prats <- c(1,2)
pratn <- c(3,4)
brasilcentro <- filter(basebr, P22ST == "5")
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

# Juntar variáveis P15ST.H (internet) e P15ST.G (redes sociais):
base2017 <- mutate(base2017, netusage = P15ST.H + P15ST.G)
base2017$netusage <- recode(base2017$netusage, "2" = "1", "1" = "1", "0" = "0")
base2017$netusage <- as.numeric(as.character(base2017$netusage))

# Filtrar respondentes válidos para as variáveis utilizadas: 
m <- c(0,1)
n <- c(0,1,2,3,4,5,6,7,8,9,10)
o <- c(1,2,3,4,5)
p <- c(1,2,3,4,5,6,7)
q <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)

base2017 <- filter(base2017, netusage %in% m)
base2017 <- filter(base2017, P19STC %in% n)
base2017 <- filter(base2017, S1 %in% o)
base2017 <- filter(base2017, edad != -2)
base2017 <- filter(base2017, S10 %in% p)
base2017 <- filter(base2017, S14 %in% q)
base2017 <- filter(base2017, sexo != -2)
saveRDS(base2017, file = "base2017.rds")

# Total respondentes = 854
mod4 <- glm(netusage ~ P19STC + S1 + S10 + edad + S14 + sexo,
               data = base2017, family = binomial)
summary(mod4)

# VD:
# P15ST.H = Uses the internet to search for political information
# P15ST.G = Uses social networks to search for political information

# VIs:
# P19STC = Self-positioning in Left- Right scale
# edad = age
# S10 = Ethnicity or race with which you identify better
# S1 = Subjective social class
# S14 = Respondent Years/Type of education


---

  
base2016 <- load("Latinobarometro2016Esp_v20170205.rdata")
base2016 <- filter(Latinobarometro2016Esp_v20170205, idenpa == "76")

base2016 <- mutate(base2016, netusage = P26STH + P26STG)
base2016$netusage <- recode(base2016$netusage, "2" = "1", "1" = "1", "0" = "0")
base2016$netusage <- as.numeric(as.character(base2016$netusage))

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
base2016 <- filter(base2016, netusage %in% v)
base2016 <- filter(base2016, sexo != -2)
saveRDS(base2016, file = "base2016.rds")

# Total respondentes = 974
mod5 <- glm(netusage ~ P17ST + S9 + S13 + S4 + edad + sexo, data = base2016, 
            family = binomial)
summary(mod5)

# VD:
# P26STG = ¿Como se informa Ud. de los asuntos polticos?. Redes Sociales 
# P26STH = ¿Como se informa Ud. de los asuntos polticos?. Medios electronicos/internet 

# VIs:
# P17ST = Self-positioning in Left - Right scale (0 a 10)
# S9 = Ethnicity or race with which you identify better (1 a 7)  
# S13 = Respondent Years/Type of education (1 a 17)  
# S4 = Subjective Income (1 a 4)  
# edad = Age  


---  

# Quantos usaram meios eletrônicos para informação sobre política 2016,2017,2018:
count(base2016, netusage == 1)/974 # 342 - 35.1%
count(base2017, netusage == 1)/854 # 362 - 42.4%
count(base2018, P19ST.G == 1)/ # 415 - 43.4%

  
---
  
  
# Comparar Brasil com outros países que tiveram eleições presidenciais em 2018:
  
# Brasil: 
brasil2 <- filter(base, IDENPA == "76")
d <- c(0,1)
e <- c(0,1,2,3,4,5,6,7,8,9,10)
i <- c(1,2,3,4,5)
j <- c(1,2,3,4,5,6,7)
k <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)

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
p1 <- ggplot(data = AvgIdeology, aes(x=Year, y=AverageIdeology,linetype = Side)) + 
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
p2 <- ggplot(data = AvgDiff, aes(x=Year, y=Diff)) + 
  geom_line()+
  geom_point()+
  theme_grey()+
  theme(text=element_text(family="Times", size=12))+
  ylim(0,10)+
  scale_x_continuous(limit=c(2002, 2018),breaks=c(2002,2010,2018))+
  labs(y = "Average Distance", x= "Year")+
  scale_color_manual(values = c("#737373"))

grid.arrange(p1, p2, nrow=2)


---
  
  
# Outros testes para 2018:
  
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
              SEXO + S1 + S6 + Info + Size + S10 + EDAD + S12M.D + S12M.E + S21.O, 
            data = base2018.4, family = binomial)
summary(mod9)

# Replicar modelo sem o centro:
# Total respondentes = 613
base2018.5 <- filter(base2018.4, P22ST != 5)
mod10 <- glm(P19ST.G ~ P22ST + P13STGBS.B + P15STGBSC.E + P16NC.D + P12STGBS +
              SEXO + S1 + S6 + Info + Size + S10 + EDAD + S12M.D + S12M.E + S21.O, 
            data = base2018.5, family = binomial)
summary(mod10)


--
  
  
# APÊNDICE A:

# Criar base % de meios usados por nível da escala de ideologia:
basemeans <- data.frame(
  radio = c(18.2, 20.4, 23.3, 11.4, 25.9, 22.5, 17.8, 17.9,24.6, 11.1, 18),
  internet = c(30.8, 35.2, 35, 39.2, 44.4, 51.9, 47.9, 44.6, 38.6, 37, 39.3), 
  televisao = c(54.5, 40.7, 61.6, 53.2, 58.3, 56.7, 54.8, 53.6, 64.9, 40.7, 43.8),
  jornal = c(25.2, 38.9, 41.7, 35.4, 38, 43.2, 42.5, 51.8, 35.1, 33.3, 33.7),
  familia = c(26.6, 31.5, 28.3, 32.9, 37, 33.6, 34.2, 26.8, 21, 37, 34.8),
  amigos = c(27.3, 33.3, 35, 29.1, 25.9, 30.8, 28.8, 32.1, 36.8, 48.1, 22.5),
  ideologia = c(0:10)
)

# Gráfico Internet:
p3 <- ggplot(data=basemeans, aes(x=ideologia, y=internet))+
  geom_bar(stat = "identity", fill = "#525252")+                        
  xlab("Ideology Scale") + ylab("Percentage") +
  ggtitle("Internet")+
  theme(text=element_text(family="Times", size=12))+
  scale_x_continuous(breaks=c(0:10))+
  scale_y_continuous(limits=c(0,100))

# Gráfico Televisao:
p4 <- ggplot(data=basemeans, aes(x=ideologia, y=televisao))+
  geom_bar(stat = "identity", fill = "#525252")+                        
  xlab("Ideology Scale") + ylab("Percentage") + 
  ggtitle("Television")+
  theme(text=element_text(family="Times", size=12))+
  scale_x_continuous(breaks=c(0:10))+
  scale_y_continuous(limits=c(0,100))

# Gráfico Jornal:
p5 <- ggplot(data=basemeans, aes(x=ideologia, y=jornal))+
  geom_bar(stat = "identity", fill = "#525252")+                        
  xlab("Ideology Scale") + ylab("Percentage") +
  ggtitle("Newspaper/Magazine")+
  theme(text=element_text(family="Times", size=12))+
  scale_x_continuous(breaks=c(0:10))+
  scale_y_continuous(limits=c(0,100))

# Gráfico Radio:
p6 <- ggplot(data=basemeans, aes(x=ideologia, y=radio))+
  geom_bar(stat = "identity", fill = "#525252")+                        
  xlab("Ideology Scale") + ylab("Percentage") +
  ggtitle("Radio")+
  theme(text=element_text(family="Times", size=12))+
  scale_x_continuous(breaks=c(0:10))+
  scale_y_continuous(limits=c(0,100))

# Grafico Familia:
p7 <- ggplot(data=basemeans, aes(x=ideologia, y=familia))+
  geom_bar(stat = "identity", fill = "#525252")+                        
  xlab("Ideology Scale") + ylab("Percentage") +
  ggtitle("Family")+
  theme(text=element_text(family="Times", size=12))+
  scale_x_continuous(breaks=c(0:10))+
  scale_y_continuous(limits=c(0,100))

# Grafico Amigos:
p8 <- ggplot(data=basemeans, aes(x=ideologia, y=amigos))+
  geom_bar(stat = "identity", fill = "#525252")+                        
  xlab("Ideology Scale") + ylab("Percentage") +
  ggtitle("Friends")+
  theme(text=element_text(family="Times", size=12))+
  scale_x_continuous(breaks=c(0:10))+
  scale_y_continuous(limits=c(0,100))

grid.arrange(p3,p4,p5,p6,p7,p8,ncol=2,nrow=3)

# Checar média do número de meios usados para buscar informações sobre política
# por nível da escala de ideologia:
inf <- c(1:11)
basebr2 <- mutate(basebr, polinf = P19ST.A+P19ST.B+P19ST.C+P19ST.D+P19ST.E+P19ST.F+
                    P19ST.G+P19ST.H+P19NC.I+P19NC.J+P19F.K+P19ST.L)
basebr2 <- filter(basebr2, P22ST %in% e)
basebr2 <- filter(basebr2, polinf %in% inf)
with(basebr2, tapply(polinf, P22ST, mean))


---
  
  
# APÊNDICE B:
  
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


# Nestes dois modelos, para checar se os achados são robustos e não dependem
# do número de entrevistados de esquerda ou direita na amostra, foram separados
# 2 grupos, com escores de 0 a 4 (esquerda = 404) e 6 a 10 (direita = 275). Os 
# modelos apontam que a ideologia influencia a probabilidade de buscar informações
# na internet para o grupo da direita, mas não é significativa no grupo da esquerda.

# Filtrar base para direita:
dir <- c(6,7,8,9,10)
basedir <- filter(base2018, P22ST %in% dir)

# Total respondentes = 275
mod7 <- glm(P19ST.G ~ P22ST + S1 + S6 + S10 + EDAD + SEXO + S12M.E + S12M.D,
            data = basedir, family = binomial)
summary(mod7)

mod7a <- glm(P19ST.G ~ P22ST + S1 + S6 + S10 + EDAD + SEXO + S12M.E + 
               S12M.D + S21.O, data = basedir, family = binomial)
summary(mod7a)

# Filtrar base para a esquerda:
esq <- c(0,1,2,3,4)
baseesq <- filter(base2018, P22ST %in% esq)

# Total respondentes = 404
mod8 <- glm(P19ST.G ~ P22ST + S1 + S6 + S10 + EDAD + SEXO + S12M.E + S12M.D,
            data = baseesq, family = binomial)
summary(mod8)

mod8a <- glm(P19ST.G ~ P22ST + S1 + S6 + S10 + EDAD + SEXO + S12M.E + 
               S12M.D + S21.O, data = baseesq, family = binomial)
summary(mod8a)

# Modelo com polarização ao invés de ideologia:

# Adicionar variável "polarization" que é a diferença entre o self-placement na 
# escala de ideologia e o número 5 (centro da escala):
basepol <- mutate(base2018, polarization = P22ST - 5)
basepol$polarization <- abs(basepol$polarization)


# Modelo de regressão:
mod11 <- glm(P19ST.G ~ polarization + S1 + S6 + S10 + EDAD + SEXO + S21.O,
            data = basepol, family = binomial)
summary(mod11)
  
# Separar polarização esquerda e direita:
basepoldir <- filter(basepol, P22ST %in% dir)
mod12 <- glm(P19ST.G ~ polarization + S1 + S6 + S10 + EDAD + SEXO + S21.O,
             data = basepoldir, family = binomial)
summary(mod12)

basepolesq <- filter(basepol, P22ST %in% esq)
mod13 <- glm(P19ST.G ~ polarization + S1 + S6 + S10 + EDAD + SEXO + S21.O,
             data = basepolesq, family = binomial)
summary(mod13)

# Adicionar categorias esquerda e direita à base polarização:
basepol2 <- filter(basepol, P22ST != 5)
basepol2 <- mutate(basepol2, side = ifelse(P22ST %in% esq, "esq",
                                           ifelse(P22ST %in% dir, "dir", "NA")))
count(basepol2, side)


# Criar gráfico:
p9 <- ggplot(data = basepol2,
            mapping = aes(x = polarization, fill = side))

windowsFonts(Times=windowsFont("Times New Roman"))
p9 + geom_bar(position = "dodge", width = 0.5,
             mapping = aes(y = ..prop.., group = side))+
  theme(text=element_text(family="Times", size=12))+
  labs(y = "Percentage", x= "Distance")+
  coord_flip()+
  scale_fill_manual(breaks = c("esq", "dir"), name = "Side", labels = c("Left", "Right"), 
                    values = c("#737373","#373737"))


---
  

# APÊNDICE C:
  
# Teste-t para ideologia e confiança na mídia Latinobarómetro:
basetrustme <- filter(basebr, P16NC.D %in% filt2)
basetrustme <- filter(basetrustme, P22ST %in% e)
basetrustme2 <- filter(basetrustme, P22ST != 5)
basetrustme2 <- mutate(basetrustme2, side = ifelse(P22ST %in% esq, "esq",
                                                   ifelse(P22ST %in% dir, "dir", "NA")))

basetrustmeesq <- filter(basetrustme2, side == "esq")
basetrustmedir <- filter(basetrustme2, side == "dir")

t.test(basetrustmeesq$P16NC.D, basetrustmedir$P16NC.D)

# Teste-t para ideologia e confiança na mídia LAPOP:
# Carregar base LAPOP:
baselapop <- read_dta("Brazil LAPOP AmericasBarometer 2019 v1.0_W.dta")

# Filtrar base:
lp <- c(1:10)
media <- c(1:7)
lesq <- c(1:5)
ldir <- c(6:10)
baselapop2 <- filter(baselapop, l1 %in% lp)
baselapop2 <- filter(baselapop2, b37 %in% media)
baselapopesq <- filter(baselapop2, l1 %in% lesq)
baselapopdir <- filter(baselapop2, l1 %in% ldir)

# T-test:
t.test(baselapopesq$b37, baselapopdir$b37)

# Teste-t para candidato e confiança na mídia LAPOP:

# Filtrar base:
voto <- c(1501, 1502)
count(baselapop, vb3n)
baselapopvoto <- filter(baselapop, vb3n %in% voto)
baselapopvoto <- filter(baselapopvoto, b37 %in% media)
baselapopvotobol <- filter(baselapopvoto, vb3n == 1501)
baselapopvotohad <- filter(baselapopvoto, vb3n == 1502)

# T-test:
t.test(baselapopvotohad$b37, baselapopvotobol$b37)


---
 
   
# Juntar os três anos (2016, 2017 e 2018) em uma só base:
basenova2016 <- base2016[,c("P17ST", "S9", "S13", "S4", "edad", "netusage", "sexo")]
basenova2017 <- base2017[,c("P19STC", "S1", "edad", "S10", "S14", "netusage", "sexo")]
basenova2018 <- base2018[,c("P22ST", "P19ST.G", "S1", "S6", "S10", "SEXO", "EDAD")]

## 2016:
basenova2016 <- basenova2016 %>% 
  rename(
    ideologia = P17ST,
    etnia = S9,
    educacao = S13,
    renda = S4,
    idade = edad
  )

## 2017:
basenova2017 <- basenova2017 %>% 
  rename(
    ideologia = P19STC,
    etnia = S10,
    educacao = S14,
    renda = S1,
    idade = edad
  )

## 2018:
basenova2018 <- basenova2018 %>% 
  rename(
    ideologia = P22ST,
    etnia = S6,
    educacao = S10,
    renda = S1,
    idade = EDAD,
    netusage = P19ST.G,
    sexo = SEXO
  )

todos <- rbind(basenova2016, basenova2017, basenova2018)

# Regressão com todos os anos juntos: 
mod14 <- glm(netusage ~ ideologia + renda + idade + sexo + educacao + etnia,
           data = todos, family = binomial)

summary(mod14)
