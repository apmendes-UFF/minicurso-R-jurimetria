#==== ANALISANDO DADOS REAIS DE CRIMES NO ERJ =====


#Biblioteca
library(tidyverse)

#====IMPORTAÇÃO E LEITURA INICIAL ====
crimes<-read.csv("DOMensalEstadoDesde1991.csv", #nome do arquivo já na pasta do projeto
                 sep=";",                       #forma de separação dos dados no csv
                 header=TRUE)                   #mantém os títulos das colunas


view(crimes)

str(crimes) #O R está lendo como 'integer' - números inteiros, todas as  colunas


#==== MODOS DE RESUMIR e VISUALIZAR OS DADOS ====

#Verificando NAs

nas <- colSums(is.na(crimes[, 3:58])) #soma, coluna a coluna, quantos nas
nas

# Comando do R BASE para separar letalidade policial sem NAs
letalidadepolicial <- crimes$hom_por_interv_policial[!is.na(crimes$hom_por_interv_policial)]

#mesmo comando na linguagem tidy usando filter( ) e pull( )

letalidadepolicial<- crimes %>%
                     filter(!is.na(hom_por_interv_policial)) %>%
                     pull(hom_por_interv_policial)


#Resumindo os dados de letalidade policial

#Verificando tamanho do vetor
length(letalidadepolicial)

#Sumário estatístico
summary(letalidadepolicial)

#Desvio-padrão
sd(letalidadepolicial)

#Coeficiente de variação
cvar<-(sd(letalidadepolicial)/mean(letalidadepolicial))*100
cvar

boxplot(letalidadepolicial, main="Homicídios por Intervenção Policial")
abline(h=0)

#Série temporal anual

letalidadeanual<-crimes[85:427,] %>%  #linhas selecionadas para não pegar na
                 group_by(ano) %>%    #agrupa por ano, retirando as repetições
                 summarise(registros = sum(hom_por_interv_policial, na.rm=TRUE)) 
#mantém apenas o crime de interesse e soma os registros

ano<-1998:2026 #criando nosso eixo x


plot(ano,letalidadeanual$registros,
                          type="l", 
                          col="red", 
     xlab="Homicídios Registrados",
     ylab="Ano",
     main="Homicídios por intervenção policial")

#====VERIFICANDO ASSOCIAÇÕES ====

#Homicidios Dolosos e Roubos

#Teste de correlação de Pearson
cor.test(crimes$hom_doloso, crimes$total_roubos)

#Diagrama de Dispersão do R base (Scatterplot)
plot(crimes$hom_doloso, crimes$total_roubos) 

#Usando o GGplot - Gráfico mais bonito

ggplot(data= crimes) +                                 #origem dos dados
     geom_point(
       mapping=aes(x=hom_doloso, y=total_roubos),      #Valores dos eixos
       color="blue") +
       labs(
         x= "Homicídios Dolosos",
         y= "Roubos")


#Conjunto de Dados do 8 de Janeiro

#install.packages("readxl")

library(readxl)

aps<-read_xlsx("8jan23.xlsx")

str(aps)

#Verificando associação entre tipo de pena e genero

#tabela de frequências
tabela<- table(aps$sexo, aps$privacao_liberdade)
addmargins(tabela)
colnames(tabela)<-c("Liberdade", "Prisão")         

#tabela de proporções
tabprop<-round(prop.table(tabela,1)*100,1) #calcular as proporções por linha e arredondar p 1 casa decimal
tabprop
addmargins(tabprop,2)

#Gráfico de Barras das Proproporções
x11()
barplot(t(tabprop), #linhas em colunas
        xlab="Gênero", ylab="Porcentagem",
        axes=F, #não desenha automaticamente os eixos
        col=c("blue","green"), 
        ylim=c(0,130)) #definir o limite do eixo y
axis(2, at=seq(0,100,20)) #como o eixo y aparecerá
legend("topright",        #legenda do gráico
       legend=rev(colnames(tabela)), 
       fill=rev(c("blue", "green")),
       cex=0.9)

#teste qui-quadrado para nossa tabela de dupla-entrada

chisq.test(tabela)


#testando associação entre número de páginas dos acórdãos, votos e tipo de pena

ggplot(aps, aes(x = n_pag, y = n_pag_voto_relator)) +
  geom_point(aes(fill = factor(privacao_liberdade)), 
             shape = 21, color = "black", size = 2) +
  scale_fill_manual(values = c("0" = "dodgerblue4", "1" = "lightskyblue1")) +
  labs(fill = "Privação de Liberdade") +
  theme_minimal() +
  theme(
    # Linhas dos eixos X e Y
    axis.line.x = element_line(color = "black", linewidth = 1),
    axis.line.y = element_line(color = "black", linewidth = 1),
    
    # Maior destaque nas linhas de grade (opcional)
    panel.grid.major = element_line(color = "gray80", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray90", linewidth = 0.5))




