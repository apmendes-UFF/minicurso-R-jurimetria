#====O TEXTO COMO DADO ====

#====BIBLIOTECAS ====
#install.packages("tidytext")
#install.packages("stringr")
#install.packages("stopwords")
#install.packages("quanteda")
#install.packages("quanteda.textstats")
#install.packages("quanteda.testplots")
#install.packages("wordcloud")
#install.packages("igraph")
#install.packages("ggraph")

library(tidyverse)
library(tidytext)
library(stringr)
library(stopwords)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(wordcloud)
library(readxl)
library(igraph)
library(ggraph)

#====PRÉ-PROCESSAMENTO====

#Leiutura dos Dados

comentarios01 <- read_xlsx("comentarios_video01.xlsx") %>%
  slice(-1) %>%                                             #Retira comentário fixado
  filter(tipo == "comentario_principal") %>%                #filtra as linhas
  filter(
    like_count >= quantile(like_count, 0.90, na.rm = TRUE) #novo filtro, por curtidas
  ) %>%
  select(id, text_original) %>%                            #seleciona as colunas de interesse
  drop_na()                                                #exclui NAs

# Remove pontuação, números, caracteres especiais
comentarios01$text_original<-gsub("[^\\p{L}\\p{N}\\s]", " ", comentarios01$text_original, perl = TRUE)
comentarios01$text_original <- gsub("[[:punct:]]", " ", comentarios01$text_original)
comentarios01$text_original <- gsub("[[:digit:]]", " ", comentarios01$text_original)

# Remove múltiplos espaços
comentarios01$text_original <- gsub("\\s+", " ", comentarios01$text_original)

# Transforma para minúsculas
comentarios01$text_original <- tolower(comentarios01$text_original)

#remove Na depois da limpeza

comentarios01 <- comentarios01 %>%
  filter(trimws(text_original) != "")

# TOKEMIZAÇÃO REMOÇÃO DE STOPWORDS

stopwordsPT<-as.data.frame(stopwords(language="pt"))
colnames(stopwordsPT)<-c("word")


#Tokemização e remoção de stopwords
comentarios01_token<-comentarios01 %>%
  unnest_tokens(word, text_original) 

comentarios01_token <- anti_join(
  comentarios01_token,
  stopwordsPT,
  by = "word"
)

#====ANÁLISE EXPLORATÓRIA====

#====Análise de Vocabulário====

#CONTAGEM SIMPLES
comentarios01_count<-comentarios01_token %>%
  count(word) %>% 
  mutate(word=reorder(word,n))

#número de vezes que uma palavra foi utilizada
summary(comentarios01_count$n)

#Tamanho dos comentários em número de palavras
tamanho_comentarios <- comentarios01_token %>%
  count(id, name = "n_palavras")

mean(tamanho_comentarios$n_palavras)

#Gráficos

#TOPWORDS - Gráfico de barras (geom_col)


comentarios01_token %>% 
  count(word,sort = TRUE) %>%                   #conta palavras
  mutate(word=reorder(word,n)) %>%              #reordena por frequência
  slice_head(n=20) %>%                          #pega as 20 mais frequentes
  ggplot()+
  geom_col(aes(x=n,y=word), fill="dodgerblue2")+ #define o gráfico de barras
  labs(y="Palavra",                              #rótulos dos eixos
       x="n",
       title = "Palavras mais mencionadas")+
  theme(text = element_text(size = 12))+
  theme_classic()

#NUVEM DE PALAVRAS

library(wordcloud)
#Paletas de cores para wordcloud
colorVec2 = rep(c('lightskyblue1','deepskyblue2','dodgerblue4'), 
                length.out=nrow(comentarios01_count))

x11()
set.seed(123) 
wordcloud(words = comentarios01_count$word, freq = comentarios01_count$n, 
          min.freq = 1,
          scale=c(5,0.5),     # Tamanho mínimo e máximo na visualização
          max.words=300,      # número máximo de palavras na nuvem
          random.order=FALSE, # Palavras em frequência decrescente
          rot.per=0.35,       # % de palavras na vertical
          use.r.layout=TRUE,  # detectar colisão
          colors=colorVec2)   #vetor de cores previamente definidos


#====Análise de Coocorrência====

#leitura e tokenização no padrão do quanteda
comentarios_quanteda <- corpus(comentarios01, text_field = "text_original")

tokens_quanteda <- tokens(
  comentarios_quanteda,
  remove_punct = TRUE,
  remove_numbers = TRUE) %>%
  tokens_remove(pattern=stopwordsPT$word)


#Data-frame com as palavras chave em contexto
palavrachave<-kwic(tokens_quanteda, pattern = c("bilionário", "pobre", "mentalidade",
                                  "deus", "brasil", "rico"), window = 7)

#Gerar a matriz de coocorrências para medir proximidades
fcmat <- fcm(tokens_quanteda,
             context = "window",
             window = 5,
             count = "weighted")

#garantir que o R lê os valores como vetor numérico
assoc_bilionario <- as.numeric(fcmat["bilionário", ])
assoc_pobre <-as.numeric(fcmat["pobre",])
assoc_mentalidade <- as.numeric(fcmat["mentalidade", ])
assoc_deus <-as.numeric(fcmat["deus",])
assoc_brasil <- as.numeric(fcmat["brasil", ])
assoc_rico <-as.numeric(fcmat["rico",])

#recupera os nomes de cada entrada da matriz (palavras que coocorrem)
names(assoc_bilionario) <- featnames(fcmat)
names(assoc_pobre) <- featnames(fcmat)
names(assoc_mentalidade) <- featnames(fcmat)
names(assoc_deus) <- featnames(fcmat)
names(assoc_brasil) <- featnames(fcmat)
names(assoc_rico) <- featnames(fcmat)

#organiza em ordem decrescente
sort(assoc_bilionario, decreasing = TRUE)[1:20]
sort(assoc_pobre, decreasing=T) [1:20]
sort(assoc_mentalidade, decreasing = TRUE)[1:20]
sort(assoc_deus, decreasing=T) [1:20]
sort(assoc_brasil, decreasing = TRUE)[1:20]
sort(assoc_rico, decreasing=T) [1:20]


library(igraph)
library(ggraph)

#GRÁFICO DE REDE SEMÂNTICA
topfeat <- textstat_frequency(dfm(tokens_quanteda), n = 30)

fcm_select <- fcm_select(
  fcmat,
  pattern = topfeat$feature
)

textplot_network(
  fcm_select,
  min_freq = 5
)
