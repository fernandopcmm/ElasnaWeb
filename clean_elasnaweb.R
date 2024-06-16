library(dplyr)
library(tidytext)
library(tm)
library(textstem)

# Importação e limpeza individual na caixa, sep semicolon
# Importar dados txt
add <- as.data.frame(roda01) #Para dados em Excel, antes de tokenizar

roda01 <- add[ ,1] #para que ncol = 1 no Corpus
roda01_df <- tibble(line = 1:405, text = roda01) #dados em tibble
roda01_df %>%
  tidytext::unnest_tokens(word, text) #tokenization, banco de dados em palavras

#limpeza, aproveitando stopwords em pt do pacote tm
removeWords=c("como","sobre","vezes","com", "uma", "para",
              "por","meu", "devido", "pois", "ainda", "nao", 
              "assim", "sim", "é", "sei")
stop_words = data_frame(word = c(tm::stopwords("pt"),removeWords))
tidy_1 <- roda01_df %>%
  tidytext::unnest_tokens(word,text)%>%
  anti_join(stop_words)

# Mais limpezas
tidy_1 <- tidy_1 %>%
  mutate(word = gsub("[[:punct:]]", "", word)) %>%
  mutate(word = tolower(word)) %>%
  mutate(word = gsub("[[:digit:]]", "", word)) %>%
  mutate(word = gsub("[^[:alnum:]\\s]", "", word)) %>%
  filter(nchar(word) > 3)

#Lematização
tidy_1l <- tidy_1  %>%
  mutate(word = textstem::lemmatize_words(word, language = "pt"))
tidy_1l %>%
  count(word, sort = T)

#Importação e limpeza de vários arquivos

# Diretório onde os arquivos estão localizados
dir_path <- "/Users/fernandolhamas/Documents/Estudos_R/ElasnaWeb"  # Substitua pelo caminho correto

# Lista de nomes de arquivos
nomes_arquivos <- paste0("roda0", 1:6, ".txt")

# Lista para armazenar os data frames
arquivos <- list()

# Laço para ler cada arquivo
for (i in 1:length(nomes_arquivos)) {
  file_path <- file.path(dir_path, nomes_arquivos[i])
  arquivos[[i]] <- read.delim(file_path, sep = "\t", encoding = "UTF-8", header = FALSE)
}

# Nomear cada data frame no resultado
names(arquivos) <- paste0("roda0", 1:6)

# Laço nos 6 arquivos

# Lista para armazenar os resultados
resultados <- list()

# Função para processar cada arquivo
processar_arquivo <- function(add) {
  # Convertendo para data frame
  add <- as.data.frame(add)
  
  # Extraindo a coluna de texto
  texto <- add[, 1]
  
  # Convertendo para tibble
  df <- tibble(line = 1:length(texto), text = texto)
  
  # Tokenização
  tidy_df <- df %>%
    tidytext::unnest_tokens(word, text)
  
  # Stopwords
  removeWords <- c("como", "sobre", "vezes", "com", "uma", "para", "por", "meu", "devido", "pois", "ainda", "nao", "assim", "sim", "é", "sei")
  stop_words <- data_frame(word = c(tm::stopwords("pt"), removeWords))
  
  # Remover stopwords e mais limpezas
  tidy_df <- tidy_df %>%
    anti_join(stop_words) %>%
    mutate(word = gsub("[[:punct:]]", "", word)) %>%
    mutate(word = tolower(word)) %>%
    mutate(word = gsub("[[:digit:]]", "", word)) %>%
    mutate(word = gsub("[^[:alnum:]\\s]", "", word)) %>%
    filter(nchar(word) > 3)
  
  # Lematização
  tidy_df <- tidy_df %>%
    mutate(word = textstem::lemmatize_words(word, language = "pt"))
  
  # Contagem de palavras
  tidy_df %>%
    count(word, sort = TRUE)
}

# Executando o laço para cada arquivo e armazenando os resultados
for (i in 1:length(arquivos)) {
  resultados[[i]] <- processar_arquivo(arquivos[[i]])
}

# Nomear cada data frame no resultado
names(resultados) <- c("resultados_roda01", "resultados_roda02", "resultados_roda03", 
                       "resultados_roda04", "resultados_roda05", "resultados_roda06")

# Atribuir cada data frame a um objeto distinto
resultados_roda01 <- resultados[[1]]
resultados_roda02 <- resultados[[2]]
resultados_roda03 <- resultados[[3]]
resultados_roda04 <- resultados[[4]]
resultados_roda05 <- resultados[[5]]
resultados_roda06 <- resultados[[6]]

# Repete o processo com Aulas

# Diretório onde os arquivos estão localizados
dir_path <- "/Users/fernandolhamas/Documents/Estudos_R/ElasnaWeb"  # Substitua pelo caminho correto

# Lista de nomes de arquivos
nomes_arquivos <- paste0("Aula", 1:7, ".txt")

# Lista para armazenar os data frames
arquivos <- list()

# Laço para ler cada arquivo
for (i in 1:length(nomes_arquivos)) {
  file_path <- file.path(dir_path, nomes_arquivos[i])
  arquivos[[i]] <- read.delim(file_path, sep = "\t", encoding = "UTF-8", header = FALSE)
}

# Nomear cada data frame no resultado
names(arquivos) <- paste0("Aula", 1:7)

# Laço nos 7 arquivos

# Lista para armazenar os resultados
resultados <- list()

# Função para processar cada arquivo
processar_arquivo <- function(add) {
  # Convertendo para data frame
  add <- as.data.frame(add)
  
  # Extraindo a coluna de texto
  texto <- add[, 1]
  
  # Convertendo para tibble
  df <- tibble(line = 1:length(texto), text = texto)
  
  # Tokenização
  tidy_df <- df %>%
    tidytext::unnest_tokens(word, text)
  
  # Stopwords
  removeWords <- c("como", "sobre", "vezes", "com", "uma", "para", "por", "meu", "devido", "pois", "ainda", "nao", "assim", "sim", "é", "sei")
  stop_words <- data_frame(word = c(tm::stopwords("pt"), removeWords))
  
  # Remover stopwords e mais limpezas
  tidy_df <- tidy_df %>%
    anti_join(stop_words) %>%
    mutate(word = gsub("[[:punct:]]", "", word)) %>%
    mutate(word = tolower(word)) %>%
    mutate(word = gsub("[[:digit:]]", "", word)) %>%
    mutate(word = gsub("[^[:alnum:]\\s]", "", word)) %>%
    filter(nchar(word) > 3)
  
  # Lematização
  tidy_df <- tidy_df %>%
    mutate(word = textstem::lemmatize_words(word, language = "pt"))
  
  # Contagem de palavras
  tidy_df %>%
    count(word, sort = TRUE)
}

# Executando o laço para cada arquivo e armazenando os resultados
for (i in 1:length(arquivos)) {
  resultados[[i]] <- processar_arquivo(arquivos[[i]])
}

# Nomear cada data frame no resultado
names(resultados) <- c("resaula01", "resaula02", "resaula03", 
                       "resaula04", "resaula05", "resaula06", "resaula07")

# Atribuir cada data frame a um objeto distinto
resaula01 <- resultados[[1]]
resaula02 <- resultados[[2]]
resaula03 <- resultados[[3]]
resaula04 <- resultados[[4]]
resaula05 <- resultados[[5]]
resaula06 <- resultados[[6]]
resaula07 <- resultados[[7]]