dados <- read.csv("~/FCM 2025/R/Pokemon_full.csv")

getwd()


library(tidyverse)

names(dados)

#Seleciona colunas
select(dados, name, hp, speed, attack)

#Filtra colunas
filter(dados, attack<50)

#Operações
mutate(dados, x= attack+speed) #Cria nova variável
mutate(dados, attack=attack/2) #Modifica variável
mutate(dados, IMC = weight/(height*height)) #Modifica variável

dados <- mutate(dados, IMC = weight/(height*height))

#Exemplo Operador
df <- select(dados, name, hp, attack, speed)
df <- filter(df, attack < 50)
df <- mutate(df, x = attack+speed)
df

# O operador pipe pega o que está à esquerda dele e coloca como primeiro argumento da próxima função
df <- dados %>% 
  select(name, hp, attack, speed) %>% 
  filter(attack<50) %>% 
  mutate(x=attack+speed) #acabou função do pipe

# gsub modifica strings
x=c("Thais", "Fernando", "Thomas")

# Se colocar ponto, o pipe ele rastreia e substitui. Se não colocar, ele põe como primeiro argumento.
x %>% 
  gsub("Th", "th", .)
# |> pipe nativo do R

dados %>%
  filter(height > 10) %>% #Altura maior que 10
  select(name, height, weight)%>% #Seleciona nome, altura e peso
  mutate(imc = weight/(height*height)) %>% #Calculo de IMC
  ggplot()+
  geom_density(aes( x = imc)) #Gráfico de densidade

head(dados)
dados %>% head

# comando interessante
glimpse(dados)
summary(dados)
str(dados)

dados %>% pull(IMC) #Retorna vetor
dados %>% select(IMC) #Retorna uma coluna

mean(c(1, 2, 3, 4))

dados %>% 
  mutate(media = mean(IMC)) #Cria e preenche uma coluna com o mesmo valor

dados %>% 
  summarise(media = mean(IMC), desvio = sd(IMC)) #Resume os dados, retornando uma coluna para cada variável

dados %>%
  group_by(type) %>% 
  summarise(media = mean(IMC), desvio = sd(IMC)) #Resume os dados, retornando uma coluna para cada variável


dados %>% 
  group_by(type) %>% 
  mutate(media = mean(IMC)) %>% View #Cria e preenche uma coluna com o mesmo valor

dados %>% 
  group_by(type) %>% 
  mutate(media = mean(IMC)) %>% 
  filter(IMC > media) %>% View

df <- dados %>% 
  group_by(type) %>% 
  mutate(media = mean(IMC))

df %>% 
  ungroup() %>% 
  mutate(media2 = mean(IMC)) %>% View

grep("saur", dados$name)
grepl("saur", dados$name)

# Busca padrões
# Aceita Regular Expression (ReGex)
grep("saur|fly", dados$name) #sauro ou fly
grepl("saur|fly", dados$name)

grep("[Ss]aur", dados$name) #Ele procura sauro com S ou s

x
grep("Th[oa]", x)

x
grep("Tho", x)

n <- c("097.765.986-90", "123.765.98-37")
grepl("\\d{3}\\.\\d{3}\\.\\d{3}-\\d{2}", n)

x <- c(
  "Amonia",
  "Ferro",
  "Dióxido de enxofre",
  "Dioxido de Enxofre",
  "Manganês",
  "Dióxido  de  Enxofre",
  "dioxido de  Enxofre",
  "dioxidode  Enxofre"
)

# + um ou mais
# * Zero ou mais
grepl("[Dd]i[óo]xido *de\\s+[eE]nxofre", x)

grepl(".", c("a", "b", "c", "0", " "))

dados %>% 
  filter(attack > 50)

dados$attack > 50

dados %>% 
  filter(grepl("saur|fly", name), attack > 50, type != "grass")

"saur" == "ivysaur"
grepl("saur", "ivysaur")

#### Trabalhando juntando dados

# Bind row

df1 <- dados %>% 
  filter(attack > 70)

df2 <- dados %>% 
  filter(attack <= 70)


rbind(df1, df2) #Juntar linhas

#Com colunas diferentes

df1 <- dados %>% 
  select(attack, speed, weight) %>% 
  filter(attack > 70)

df2 <- dados %>% 
  select(attack, weight, height, hp) %>% 
  filter(attack <= 70)

rbind(df1, df2) #Juntar linhas - não aceita dimensoes e nomes diferentes

bind_rows(df1, df2) #Nuntar linhas - completa se não bater


# juntar colunas

df1 <- dados %>% head(100)
df2 <- dados %>% tail(100)

cbind(df1, df2) %>% names

bind_cols(df1, df2, .name_repair = "check_unique")

###############

df_resumo <- dados %>%
  group_by(type) %>% 
  summarise(media = mean(IMC), desvio = sd(IMC)) #Resume os dados, retornando uma coluna para cada variável

#Fazendo join
#left, right, full, inner

left_join(dados, df_resumo, by = c("type")) %>% View

df_resumo_mis <- df_resumo %>%  filter(type != "grass")

left_join(dados, df_resumo_mis, by = c("type")) %>% View
right_join(dados, df_resumo_mis, by = c("type")) %>% View

df_resumo_mis$type[5] = "thomas" 
right_join(dados, df_resumo_mis, by = c("type")) %>% View
left_join(dados, df_resumo_mis, by = c("type")) %>% View

full_join(dados, df_resumo_mis, by = c("type")) %>% View
inner_join(dados, df_resumo_mis, by = c("type")) %>% View
