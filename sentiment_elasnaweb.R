library(lexiconPT)
library(tidyverse)
library(tidytext)

#definindo objeto para dicionário do lexiconPT
sentiment = data.frame(word = sentiLex_lem_PT02$term ,
                       polarity = sentiLex_lem_PT02$polarity) %>% 
  mutate(sentiment = if_else(polarity>0,"positive",if_else(polarity<0,"negative","neutro")),
         word = as.character(word)) %>% 
  as_tibble()

# Plotando os resultados
ggplot(sentimentos, aes(word, sentiment, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Análise de Sentimentos", x = "Palavra", y = "Sentimento") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Função para análise de sentimentos
analise_sentimentos <- function(df, source_name) {
  df %>%
    left_join(sentiment, by = "word") %>%
    mutate(sentiment = if_else(is.na(sentiment), "neutro", sentiment)) %>%
    count(word, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative,
           source = source_name)
}

# Executando a análise para cada objeto
resultados_list <- list(
  analise_sentimentos(resultados_roda01, "resultados_roda01"),
  analise_sentimentos(resultados_roda02, "resultados_roda02"),
  analise_sentimentos(resultados_roda03, "resultados_roda03"),
  analise_sentimentos(resultados_roda04, "resultados_roda04"),
  analise_sentimentos(resultados_roda05, "resultados_roda05"),
  analise_sentimentos(resultados_roda06, "resultados_roda06")
)

# Combinando os resultados em um único data frame
resultados_combinados <- bind_rows(resultados_list)

# Plotando os resultados com facet_wrap
ggplot(resultados_combinados, aes(word, sentiment, fill = sentiment > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ source, ncol = 3, scales = "free_x") +
  labs(title = "Análise de Sentimentos", x = "Palavra", y = "Sentimento") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remove os rótulos do eixo x
        axis.ticks.x = element_blank())+ # Remove os ticks do eixo x
  scale_y_continuous(limits = c(-1, 1), 
        breaks = c(-1, 0, 1)) # Limita o eixo y e define os rótulos

# Aulas
# Função para análise de sentimentos
analise_sentimentos <- function(df, source_name) {
  df %>%
    left_join(sentiment, by = "word") %>%
    mutate(sentiment = if_else(is.na(sentiment), "neutro", sentiment)) %>%
    count(word, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative,
           source = source_name)
}

# Executando a análise para cada objeto
resultados_list <- list(
  analise_sentimentos(resaula01, "resaula01"),
  analise_sentimentos(resaula02, "resaula02"),
  analise_sentimentos(resaula03, "resaula03"),
  analise_sentimentos(resaula04, "resaula04"),
  analise_sentimentos(resaula05, "resaula05"),
  analise_sentimentos(resaula06, "resaula06"),
  analise_sentimentos(resaula07, "resaula07")
)

# Combinando os resultados em um único data frame
resultados_combinados <- bind_rows(resultados_list)

# Plotando os resultados com facet_wrap
ggplot(resultados_combinados, aes(word, sentiment, fill = sentiment > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ source, ncol = 3, scales = "free_x") +
  labs(title = "Análise de Sentimentos", x = "Palavra", y = "Sentimento") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remove os rótulos do eixo x
        axis.ticks.x = element_blank())+ # Remove os ticks do eixo x
  scale_y_continuous(limits = c(-1, 1), 
                     breaks = c(-1, 0, 1)) # Limita o eixo y e define os rótulos

#teste com porcentagens
# Calculando porcentagens de sentimentos, ignorando neutros
sentimento_summary <- resultados_combinados %>%
  group_by(source) %>%
  summarize(total_positive = sum(positive, na.rm = TRUE),
            total_negative = sum(negative, na.rm = TRUE),
            positive_pct = total_positive / (total_positive + total_negative) * 100,
            negative_pct = total_negative / (total_positive + total_negative) * 100) %>%
  mutate(label = paste0("Positivo: ", round(positive_pct, 1), "%\nNegativo: ", round(negative_pct, 1), "%"))

ggplot(resultados_combinados, aes(word, sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ source, ncol = 2, scales = "free_x") +
  labs(title = "Análise de Sentimentos", x = "Palavra", y = "Sentimento") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Remove os rótulos do eixo x
    axis.ticks.x = element_blank(), # Remove os ticks do eixo x
    strip.text = element_text(size = 7)) +# Ajusta o tamanho do texto das facetas
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) + # Limita o eixo y e define os rótulos
  geom_text(data = sentimento_summary, aes(x = Inf, y = 1, label = label),
            hjust = 1.1, vjust = 1, color = "black", size = 5)

