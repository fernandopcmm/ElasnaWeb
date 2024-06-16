library(tidyverse)
library(tidytext)

# Lista de nomes de arquivos e nomes para os objetos de saída
arquivos <- paste0("roda0", 1:6, ".txt")
nomes_objetos <- paste0("roda0", 1:6, "_df")

# Função para importar e tokenizar os dados
processar_arquivo <- function(nome_arquivo) {
  # Importar dados
  add <- read_delim(nome_arquivo, delim = "\t", col_names = FALSE, col_types = cols(.default = "c"))
  texto <- add[[1]]
  
  # Criar tibble com linha e texto
  df <- tibble(line = 1:length(texto), text = texto)
  
  # Tokenizar o texto
  df_tokenizado <- df %>%
    unnest_tokens(word, text)
  
  return(df_tokenizado)
}

# Lista para armazenar os data frames tokenizados
lista_dfs <- list()

# Laço para processar cada arquivo e armazenar na lista
for (i in seq_along(arquivos)) {
  lista_dfs[[nomes_objetos[i]]] <- processar_arquivo(arquivos[i])
}

# Verificando a estrutura de um dos data frames resultantes
str(lista_dfs[[1]])

library(syuzhet)
# dados na forma original
dados <- bind_rows(lista_dfs)
# Adicionando uma nova coluna de sentimento
dados$sentimento <- sapply(dados$word, function(word) {
  sentiment <- get_sentiment(word, method = "syuzhet")
  if(sentiment > 0) {
    return("positivo")
  } else if(sentiment < 0) {
    return("negativo")
  } else {
    return("neutro")
  }
})
# Há uma coluna nova com o sentimento do discurso
discursos_positivos <- dados %>% filter(sentimento == "positivo")

# Para visualizar alguns discursos positivos
head(discursos_positivos)
table(dados$sentimento)
# Tem mais neutros. Indica certa desconfiança e inabilidade no uso

#Aulas

# Lista de nomes de arquivos e nomes para os objetos de saída
arquivos <- paste0("Aula", 1:7, ".txt")
nomes_objetos <- paste0("Aula", 1:7, "_df")

# Função para importar e tokenizar os dados
processar_arquivo <- function(nome_arquivo) {
  # Importar dados
  add <- read_delim(nome_arquivo, delim = "\t", col_names = FALSE, col_types = cols(.default = "c"))
  texto <- add[[1]]
  
  # Criar tibble com linha e texto
  df <- tibble(line = 1:length(texto), text = texto)
  
  # Tokenizar o texto
  df_tokenizado <- df %>%
    unnest_tokens(word, text)
  
  return(df_tokenizado)
}

# Lista para armazenar os data frames tokenizados
lista_dfs <- list()

# Laço para processar cada arquivo e armazenar na lista
for (i in seq_along(arquivos)) {
  lista_dfs[[nomes_objetos[i]]] <- processar_arquivo(arquivos[i])
}

# Verificando a estrutura de um dos data frames resultantes
str(lista_dfs[[1]])

library(syuzhet)
# dados na forma original
dados <- bind_rows(lista_dfs)

# Adicionando uma nova coluna de sentimento
dados$sentimento <- sapply(dados$word, function(word) {
  sentiment <- get_sentiment(word, method = "syuzhet")
  if(sentiment > 0) {
    return("positivo")
  } else if(sentiment < 0) {
    return("negativo")
  } else {
    return("neutro")
  }
})
# Há uma coluna nova com o sentimento do discurso
discursos_positivos <- dados %>% filter(sentimento == "positivo")

# Para visualizar alguns discursos positivos
head(discursos_positivos)
table(dados$sentimento)
# Tem mais neutros. Indica certa desconfiança e inabilidade no uso