
library(readtext)
library(tm)
library(tidyverse)
library(ggplot2)
library(wordcloud)
library(topicmodels)
library(tidytext)
library(FactoMineR)
library(factoextra)
library(RColorBrewer)
library(stringr)



main_path <- "C:/Users/rcarl/OneDrive/Desktop/Esame NetText/PDF UFFICIALI"
texts <- readtext(file.path(main_path, "*.pdf"))

corpus <- Corpus(VectorSource(texts$text))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(function(x) gsub("\n", " ", x)))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords("english"))  # <-- inglese
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)


dtm <- DocumentTermMatrix(corpus, control = list(wordLengths = c(4, Inf)))
dtm <- dtm[rowSums(as.matrix(dtm)) > 0, ]


dtm_mat <- as.matrix(dtm)


word_freq <- sort(colSums(dtm_mat), decreasing = TRUE)


word_df <- data.frame(word = names(word_freq), freq = word_freq)

wordcloud(words = word_df$word,
          freq = word_df$freq,
          min.freq = 2,
          max.words = 100,  
          random.order = FALSE,
          scale = c(3, 0.7),  
          colors = brewer.pal(8, "Dark2"))



k_seq <- 2:10
loglikelihoods <- c()

for (k in k_seq) {
  cat("Calcolo modello LDA con k =", k, "\n")
  lda_model_k <- LDA(dtm, k = k, control = list(seed = 1234))
  loglikelihoods <- c(loglikelihoods, logLik(lda_model_k))
}

plot(k_seq, loglikelihoods, type = "b", pch = 19,
     xlab = "Numero di Topic (k)", ylab = "Log-Likelihood",
     main = "Valutazione LDA per diversi k")


best_k <- 4 


lda_model <- LDA(dtm, k = best_k, method = "Gibbs",
                 control = list(seed = 1234, alpha = 0.1, delta = 0.01, burnin = 1000, iter = 1000, thin = 100))



lda_topics <- tidy(lda_model, matrix = "beta")

top_terms <- lda_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup()

ggplot(top_terms, aes(reorder(term, beta), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top Terms per Topic", x = "Termine", y = "Peso")



lda_topics <- tidy(lda_model, matrix = "beta")


top_words <- lda_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>%
  ungroup() %>%
  arrange(topic, -beta)


topic_names <- c("Mercati Emergenti", "Politica Economica", "Prestiti e Aziende", "Crisi Finanziaria")


custom_theme <- theme(
  plot.title = element_text(size = 14, face = "bold"),
  axis.title = element_text(size = 12),
  axis.text = element_text(size = 10),
  strip.text = element_text(size = 12, face = "bold"),
  legend.title = element_text(size = 12),
  legend.text = element_text(size = 10)
)


top_words <- top_words %>%
  mutate(topic_name = factor(topic, labels = topic_names))

ggplot(top_words, aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic_name, scales = "free") +
  coord_flip() +
  labs(title = "Top Terms per Topic",
       x = "Term",
       y = "Beta Value",
       fill = "Topic") +
  scale_fill_brewer(palette = "Set3") +
  custom_theme


dtm_ca <- as.matrix(dtm)


top_terms <- names(sort(colSums(dtm_ca), decreasing = TRUE))[1:70]
dtm_ca_reduced <- dtm_ca[, top_terms]


dtm_ca_reduced <- dtm_ca_reduced[rowSums(dtm_ca_reduced) > 0, ]


library(FactoMineR)
library(factoextra)

ca_result <- CA(dtm_ca_reduced, graph = FALSE)

fviz_ca_biplot(ca_result,
               repel = TRUE,
               col.col = "#E74C3C",    
               col.row = "#2E86C100", 
               label = "col",         
               invisible = "row",     
               title = "Correspondence Analysis (Top 70 Terms)") +
  theme_minimal()


library(tidytext)
library(dplyr)
library(ggplot2)


text_df <- data.frame(document = texts$doc_id,
                      text = texts$text,
                      stringsAsFactors = FALSE)


tokens_sentiment <- text_df %>%
  unnest_tokens(word, text)


data("stop_words")
tokens_sentiment <- tokens_sentiment %>%
  anti_join(stop_words, by = "word")


bing <- get_sentiments("bing")
tokens_sentiment <- tokens_sentiment %>%
  inner_join(bing, by = "word")


sentiment_by_doc <- tokens_sentiment %>%
  count(document, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment_score = (positive - negative) / (positive + negative + 1))


gamma <- tidy(lda_model, matrix = "gamma") %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%
  ungroup() %>%
  select(document, topic)


gamma$document <- texts$doc_id[as.numeric(gamma$document)]


sentiment_topic <- sentiment_by_doc %>%
  inner_join(gamma, by = "document") %>%
  group_by(topic) %>%
  summarize(avg_sentiment = mean(sentiment_score, na.rm = TRUE),
            n_docs = n())


sentiment_topic$topic_name <- recode_factor(as.character(sentiment_topic$topic),
                                            `1` = "Mercati Emergenti",
                                            `2` = "Politica Economica",
                                            `3` = "Prestiti e Aziende",
                                            `4` = "Crisi Finanziaria")


ggplot(sentiment_topic, aes(x = reorder(topic_name, avg_sentiment), y = avg_sentiment, fill = topic_name)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Sentiment Medio per Topic",
       x = "Topic", y = "Sentiment Normalizzato") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14)



library(tidytext); library(dplyr); library(ggplot2)


text_df <- texts %>%
  mutate(source = ifelse(str_detect(doc_id, "FT_"), "FT", "WS")) %>%
  select(document = doc_id, text, source)


tokens <- text_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")


sent_doc <- tokens %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(source, document, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(score = (positive - negative) / (positive + negative + 1))


sent_source <- sent_doc %>%
  group_by(source) %>%
  summarize(avg_score = mean(score, na.rm = TRUE),
            median_score = median(score, na.rm = TRUE),
            n_docs = n()) %>%
  ungroup()


ggplot(sent_source, aes(x = source, y = avg_score, fill = source)) +
  geom_col(show.legend = FALSE) +
  geom_errorbar(aes(ymin = median_score, ymax = avg_score), width = 0.2) +
  labs(title = "Sentiment medio per Fonte",
       x = "Giornale", y = "Sentiment Normalizzato (Avg)") +
  theme_minimal()




library(dplyr)
library(ggplot2)
library(stringr)


texts$anno <- str_extract(texts$doc_id, "(19|20)\\d{2}")
texts <- texts %>%
  filter(!is.na(anno)) %>%
  filter(as.numeric(anno) >= 1990 & as.numeric(anno) <= 2025)


gamma <- tidy(lda_model, matrix = "gamma") %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%
  ungroup() %>%
  select(document, topic)


gamma$document <- texts$doc_id[as.numeric(gamma$document)]
gamma$anno <- str_extract(gamma$document, "(19|20)\\d{2}")


topic_year <- gamma %>%
  filter(!is.na(anno)) %>%
  count(anno, topic) %>%
  group_by(anno) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup()


topic_year$topic <- recode_factor(as.character(topic_year$topic),
                                  `1` = "Mercati Emergenti",
                                  `2` = "Politica Economica",
                                  `3` = "Prestiti e Aziende",
                                  `4` = "Crisi Finanziaria")



library(ggplot2)
library(scales)



ggplot(topic_year, aes(x = as.numeric(anno), y = percent, color = topic, group = topic)) +
  geom_line(size = 1.5) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "black", linewidth = 0.8) +
  ggplot2::annotate("text", x = 2008.5, y = 1.05, label = "Crisi 2008", hjust = 0, size = 4) +
  labs(title = "Evoluzione Temporale dei Topic (1990–2020)",
       x = "Anno",
       y = "Frequenza relativa (%)",
       color = "Topic") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(1990, 2020, 5)) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")




