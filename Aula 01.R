#====APRESENTAÇÃO E NOÇÕES FUNDAMENTAIS ====#

#==== PACOTES ====

install.packages("tidyverse")

#Chamando os pacotes instalados

library(tidyverse)
library(read.csv) #função nativa do R, basta chamar com library. Serve para ler arquivos csv.

#====SINTAXE DO R====

#====Operações matemáticas====

#Aritmética 

#soma e subtração

145+230

200-45

#multiplicação
5*250

(10-5)/(20+5)

#Potenciação e radiciação
(-4)^2

sqrt(-4) #note a mensagem de aviso. A raiz quadrada de -4 não é real!


#exponencial e logaritmo

exp(0) #exponencial é o número de euler, e. Estamos pedindo para o R elevar e a certo valor
log(1) #logaritmo na base e
log10(100) #logartimo de 100 na base 10

#==== ESTRUTURAS DE DADOS ====

#=== Vetores ====

#Vamos criar os seguintes vetores

times <- c("Flamengo", "Fluminense", "Vasco", "Botafogo",
           "Palmeiras", "São Paulo", "Corinthians", "Santos")
pontos <- c(45, 38, 22, 30, 48, 27, 32, 25)
classificação <-c(2, 4, 19, 11, 1, 13, 9, 14)

#notem que a classifição é um vetor numérico, mas diferente dos pontos
#Esses números devem ser interpretados como fatores.Então fazemos

classificação<-as.factor(classificação)

#notem no enviroment que a natureza do objeto mudou!

#pequeno exemplo do uso dos operadores lógicos;. Rodem e observem a saída:

pontos>=35

times!="Flamengo"

times%in%"Botafogo"

times[3]

#observação - sequências numéricas

seq(from=1, to=10, by=0.5 ) #cria uma sequência de 1 a 10 de meio em meio

#====Matrizes====

exemplodematriz<-matrix(c(0,1,2,3,5,6,9,0,1), 
                        ncol=3)
exemplodematriz

#====Dataframes====

brasileirao<-data.frame(times, pontos, classificação)

brasileirao

#vamos confirmar como o R está lendo esses dados?

str(brasileirao)

#Quais são as dimensões do nosso data-frame

dim(brasileirao)

#Vamos organizar segundo a classificação?

brasileirao<-brasileirao %>%
             arrange(classificação)
brasileirao

#que tal um gráfico básico?
x11()                                         #abre uma janela
barplot(brasileirao$pontos,                    #níveis da variável     
        names.arg = brasileirao$times,          #rótulos do eixo x
        main = "Pontos no Brasileirão por time", #legenda principal
        ylab = "Pontos",                         #legenda eixo y
        las=2)                                   #coloca os nomes na vertical
#podemos mudar os limites de y com 'ylim='


#====AJUDA====

?arrange #além de uma explicação dos argumentos da função, ao final temos exemplos
