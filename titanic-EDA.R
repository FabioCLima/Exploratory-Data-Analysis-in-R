#
# Este é um script do R correspondente processo de EDA no dataset Titanic
# 

library(tidyverse)
library(RColorBrewer)
library(viridis)
library(plotly)
library(patchwork)
options(digits = 3)

# 1. Importar o dado.
# 2. Limpeza do dado.
# 3. Processamento e organização.
# 4. Visualização dos dados.
#
# 1. Importar o dado
# training set   conjunto de dados que será usado para gerar o algoritmo de ML
# Informações sobre o dataset - TITANIC
# Dicionário dos Dados
# 
# Variável      Definition      Key
#
# Survived      Survival        0 = No, 1 = Yes
# PClass        Ticket class    1 = 1st, 2= 2nd, 3 = 3rd
# Sex           Sex(Gender)               
# Age           Age in years
# Sibsp         # of siblings / spouses aboard of the Titanic
# Parch         # of parents  / children aboard of the Titanic
# ticket        Ticket number
# fare          Passenger fare
# cabin         Cabin number
# embarked      Port of Embarkation  C = Cherbourg, Q = Queenstown, S = Southampton
#


Titanic <- read.csv("./data/train.csv", stringsAsFactors = FALSE, 
                    fileEncoding = "UTF-8-BOM", na.strings = c("NA",""))
View(Titanic)
str(Titanic)

# 2. Limpeza do dado, processamento e organização

Titanic <- Titanic %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked) %>%
  mutate(Survived = factor(Survived), Pclass = factor(Pclass), 
          Sex = factor(Sex), Embarked = factor(Embarked))
glimpse(Titanic)
save(Titanic, file = "rda/Titanic.rda")

# 4. Visualização dos dados.
#
# Vamos começar a nossa análise visual do dataset, 
# olhando a distribuição das idades entre os homens e mulheres e algumas
# outras variáveis que possam mostrar alguma covariação entre elas e a 
# variável "Survived" que é a variável de maior importância nesta análise.
# pra tentarmos buscar algum insight sobre que o que os dados nos revelam
# sobre a sobrevivência dos passageiros. Para responder as questões 
# relacionadas as taxas de sobrevivência. Especificamente, estas questões
# usam variáveis categóricas (factor).Dados categóricos(devem ser codificados -
# como factors), um tipo de dado muito comum neste tipo de estudo, e o ggplot2
# oferece features poderosas para visualização de dados categóricos (factor).

# Questão 1: Demografia dos passageiros do Titanic
# Faça density plots das idades, agrupados por sexo. Experimente com combina-
# ções de facetas, alpha blending, stacking e usando a contagem de variáveis 
# no eixo y, responda as seguintes perguntas.

# Mulheres e homens tem a mesma forma na distribuição das idades ?

g1 <- Titanic %>%
      ggplot(aes(x = Age, y = ..count..,color = Sex, fill = Sex)) +
      geom_density(alpha = 0.7) + 
      scale_fill_viridis(discrete = TRUE) +
      scale_color_viridis(discrete = TRUE) +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_continuous(breaks = seq(0,80, by = 10)) +
       facet_grid(Sex ~.) +
      labs(x = "idades", title = "Distribuição de idades por sexo",
           subtitle = "facetado") +
      theme_bw()

# Um gráfico facetado é útil para comparar as distribuições de homens e
# mulheres. Para cada faceta, temos uma distribuição com mesmo "shape",
# mostrando 2 modos ao redor de 4 - 5 anos, as proporções entre homens e
# mulheres diferem um pouco ao redor das idades e há mais homens do que,
# mulheres.

# Explique um pouco sobre a forma geral da distribuição da idade?

g2 <- Titanic %>% ggplot(aes(x = Age, y = ..count.., color = Sex, 
                             fill = Sex)) +
      geom_density(alpha = 0.2, position = "stack") +
      scale_color_brewer(palette = "Dark2") +
      scale_fill_brewer(palette = "Dark2") +
      scale_x_continuous(breaks = seq(0,80, by = 10)) +
      labs(x = "idades", title = "Distribuição de idades por sexo",
           subtitle = "stackeado") +
      theme_bw()

# Um density plot stacked com contador no eixo y, nos ajuda a quantificar as
# distribuições de homens e mulheres. O principal modo, está ao redor da idade
# 25 e modo menor, está ao redor de 4-5 anos. Há mais homens do que mulheres,
# como indicado pela área mais alta em praticamente todas as idades.

g3 <- Titanic %>% ggplot(aes(x = Age, y = ..count.., color = Sex, 
                            fill = Sex)) +
      geom_density(alpha = 0.2) +
      scale_color_brewer(palette = "Dark2") +
      scale_fill_brewer(palette = "Dark2") +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_continuous(breaks = seq(0,80, by = 10)) +
      labs(x = "idades", title = "Distribuição de idades por sexo",
           subtitle = "alpha blending") +
      theme_bw()

g1 / (g2 + g3) + plot_layout(ncol = 1, byrow = TRUE)

# Com esse gráfico preenchido por sexo, e com alpha blending, nos mostra que
# a proporção de mulheres, abaixo da idade 17 é mais do que a dos homens, 
# há mais homens do que mulheres, entre 18-35, temos uma mesma proporção entre
# os sexos para a faixa de 35-55 e uma proporção maior do que 55. Os mais velhos
# individuos são homens, faixa de 80 anos.

# Questão 3: QQ-plot da distribuição das idades
# Use geom_qq to make a QQ-plot da idade dos passageiros e adicione um linha
# com geom_abline. com uma idade of NA primeiramente. Use o objeto a seguir
# com o argumento o dparams

params <- Titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

Titanic %>%
  filter(!is.na(Age)) %>%   # filtro para tirar os valores NA ausentes
  ggplot(aes(sample = Age)) +
  geom_qq(dparams = params) +
  geom_abline() +
  theme_bw()

# Question 3: Sobrevivência por sexo
# Faça gráficos, barplots das variáveis survived e sexo, usando geom_bar.
# Tente plotar por uma variável e preenchendo por outra variável. Tente
# plot padrão, e então trate de adiciona position = position_dodge() para o 
# o geom_bar, para fazer barras separarem por grupo

grupos <- Titanic %>% 
           group_by(Sex) %>% 
           summarize(count = n(), media = mean(Age, na.rm = TRUE), 
           dp = sd(Age, na.rm = TRUE ))



g5 <- Titanic %>% 
      ggplot(aes(x = Age, y = ..count.., fill = Survived)) +
      geom_density(alpha = 0.2) +
      scale_color_brewer(palette = "Dark2") +
      scale_fill_brewer(palette = "Dark2") + 
      scale_x_continuous(breaks = seq(0,80, by = 10)) +
      ggtitle("Distribuição da idade dos sobreviventes", 
      subtitle = "preenchido por idade") +
      labs(x = "Sobreviventes") +
      theme_bw() 
           
g5            

p1 <- Titanic %>%
      ggplot(aes(Survived, fill = Sex)) +
      scale_color_brewer(palette = "Dark2") +
      scale_fill_brewer(palette = "Dark2") +
      geom_bar() +
      labs(title = "Contagem de sobreviventes",
      subtitle = "preenchidos por sexo",
      x = "sobreviventes", y = "contagem") + theme_bw() 
      

p2 <- Titanic %>%
      ggplot(aes(Survived, fill = Sex)) +
      scale_color_brewer(palette = "Dark2") +
      scale_fill_brewer(palette = "Dark2") +
      geom_bar(position = position_dodge()) +
      labs(title = "Contagem de sobreviventes
      separados por sexo",
      x = "Sobreviventes", y = "contagem") + theme_bw()

p3 <- Titanic %>%
      ggplot(aes(Sex, fill = Survived)) +
      geom_bar() +
      scale_fill_brewer(palette = "Dark2") +
      labs(title = "Contagem de homens e mulheres", 
      subtitle = "preenchidos por sobreviventes",
      x = "Sobreviventes", y = "contagem") + theme_bw()

(p1 + p2) / p3 + plot_layout(ncol = 1, byrow = TRUE)

# Questão 5: Sobrevivência por idade
# Faça um density plot preenchido por status de sobreviência.Mude o eixo y
# para ter a contagem das pessoas  e set o alpha = 0.2
# Qual é o group de idade com mais número de mortes?
# Qual é o group é o único de tem mais chance de sobreviver?
# Qual é a idade do grupo que quem a quantidade de mortes?
# Qual é a proporçao de mortes.

q5 <- Titanic %>% 
  ggplot(aes(x = Age, y = ..count.., fill = Survived)) +
  geom_density(alpha = 0.2) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") + 
  scale_x_continuous(breaks = seq(0,80, by = 10)) +
  ggtitle("Distribuição da idade dos sobreviventes", 
          subtitle = "preenchido por idade") +
  labs(x = "Sobreviventes") +
  theme_bw() 

q5  

# Pelo gráfico anterior, temos que o grupo de idade mais provável viver
# do que morrer é o grupo de 0-8 anos.
# O gráfico com maior número de mortes é o grupo de 18-30 anos.
# O gráfico com maior proporção de mortes é o grupo 70-80 anos.

# Questão 6: Sobrevivência por "Fare" - preço pago pela passagem
# Primeiro remova os individuos que pagaram 0 pela passagem. Tente um
# Uma transformação de log2 para os "Fare". Adicione os pontos, com 
# jitter e alpha blending.

q6 <- Titanic %>% filter(Fare > 0) %>%
      ggplot(aes(Survived, Fare)) +
      geom_boxplot(aes(fill = Survived)) +
      scale_fill_brewer(palette = "Dark2") +
      scale_y_continuous(trans = "log2") +
      geom_jitter(alpha = 0.2) +
      labs(title = "Sobrevivência distribuída por valor pago por Fare",
      x = "sobreviventes") + theme_bw()
      
q6
ggplotly(q6)

# Passageiros que sobreviveram geralmente pagaram passagens mais altas
# do que os que não sobreviveram.
# A maioria dos passageiros que pagaram pelo Fare ao redor de $8 não
# sobreviveram.
#

# A variável Pclass corresponde a classe ao qual o passageiro pertencia.
# Faça 3 barplots básicos dos passageiros por classe à qual ele pertencia,
# preenchidos pela variável Survival. Para os segundo, faça o mesmo barplot
# mas use o argumento position = position_fill, para mostrar as proporções
# relativas em cada grupo, ao invés das contagens. Para o terceiro gráfico
# faça um barplot da variável Survived preenchida por Pclass usando 
# position = position_fill.

# barplot of passenger class filled by survival
#
q7a <- Titanic %>%
      ggplot(aes(Pclass, fill = Survived)) +
      geom_bar() +
      scale_fill_brewer(palette = "Dark2") +
      labs(title = "Distribuição de passageiros por classes ", 
      subtitle = "estratificado por sobreviventes") +
      ylab("Proporção") +
      theme_bw()

# barplot of passenger class filled by survival with position_fill
#
q7b <- Titanic %>%
      ggplot(aes(Pclass, fill = Survived)) +
      geom_bar(position = position_fill()) +
      scale_fill_brewer(palette = "Dark2") +
      labs(title = "Distribuição de passageiros por classes ", 
      subtitle = "estratificado por sobreviventes") +
      ylab("Proporção") +
      theme_bw()

# Barplot of survival filled by passenger class with position_fill
#
q7c <- Titanic %>%
      ggplot(aes(Survived, fill = Pclass)) +
      geom_bar(position = position_fill()) +
      labs(title = "Distribuição dos sobreviventes ", 
      subtitle = "estratificado por classe") +
      scale_fill_brewer(palette = "Dark2") +
      ylab("Proporção") +
      theme_bw()


q7a / (q7b + q7c) + plot_layout(ncol = 1, byrow = TRUE)

# Há mais passageiros na 3 classe do que passageiros na primeira e segunda
# classes combinadas

# A taxa de sobrevivência foi alta para os passageiros da primeira classe
# seguidos pelos passageiros da segunda classe. A terceira classe teve o
# mais baixo taxa de sobrevivência.
# A maioria dos passageiros na primeira classe sobreviveram. Já a maioria
# das classes inferiores não sobreviveu.
# A majoridade dos que não sobreviveram eram da terceira classe.

# Questão 8: Sobrevivência por idade, sexo e classe do passageiro.
# Crie um grid de density plots por idade, preenchido por status de Survived,
# com um count no eixo y, facetado por sexo e classe de passageiros.

q8 <- Titanic %>% 
      ggplot(aes(x = Age, y = ..count.., fill = Survived)) + 
      geom_density(position = "stack") +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_discrete(labels = c("não sobreviventes", "sobreviventes")) +
      scale_fill_brewer(palette = "Dark2", labels = c("não sobreviventes", 
                      "sobreviventes" )) +
      labs(title = "Sobrevivência por idade", 
      subtitle = "estratificado por classes dos passageiros e gênero") +
      facet_grid(Sex ~ Pclass) + 
      theme_bw()
q8

# A porcentagem de sobreviventes também pode ser posta assim:

prop.table(table(Titanic$Survived))

# Podemos examinar as relações por classes - two-way cross-tabulations.\
# sobrevivência por Sexo
# sobrevivência por Pclasse
# só serve para variáveis categóricas (factors)
# É um teste para verificar a independência as duas variáveis em cada cel.
library(gmodels)
CrossTable(x = Titanic$Survived , y = Titanic$Sex, chisq = TRUE)
#
x = Titanic$Survived
z = Titanic$Pclass
3
CrossTable(x, z) 

# Os resultados das tabelas acima, mostram o que já tinhamos verificado
# através dos gráficos anteriores, mas é uma boa maneira de mostrar a rela
# ção entre as categóricas e taxa de sobreviência no que diz respeito ao 
# genero e a divisão de classes.

# Podemos utilizar os histogramas para vizualizar a sobrevivência
# Por idade e preenchendo os bins com a variável categórica  Survival

p1 <- Titanic %>%
      ggplot(aes(x = Age, fill = Survived)) +
      theme_bw() +
      geom_histogram(binwidth = 5, color = "black", alpha = 0.8) +
      labs(y = "Passenger Count",
           x = "Age (binwidth = 5)",
           title = "Taxa de Sobrevivência por idade") +
      scale_fill_brewer(palette = "Dark2") 
  


p2 <- Titanic %>% 
      ggplot(aes(x = Survived, y = Age, fill = Survived)) +
      theme_bw() +
      geom_boxplot() +
      scale_fill_brewer(palette = "Dark2") +
      labs(y = "Age",
      x = "Survived",
      title = "Taxa de Sobrevivência por idade")

p1 + p2 + plot_layout(ncol = 2, byrow = TRUE)
