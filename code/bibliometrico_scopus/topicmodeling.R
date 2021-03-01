library(tidyverse)
library(quanteda)
library(stm)
library(tidytext)
library(textstem)


metadata <- readr::read_csv(file = "./data/metadata.csv")
keywords <- metadata %>% filter(tag=="keyword")

articles <- metadata %>%
  select(-source) %>% 
  filter(!tag == "keyword") %>%
  pivot_wider(id_cols = id, names_from = tag, values_from = value) %>%
  left_join(keywords %>% count(id) %>% rename(keywords=n)) %>%
  mutate(
    year=as.integer(year),
    cited=as.integer(cited)
  ) %>%
  filter(
    year>=2010,
    !is.na(abstract)
  )

abstracts <- metadata %>%
  filter(tag=="abstract") %>%
  select(id,value) %>%
  left_join(articles %>% select(id,year)) %>% filter(year>=2010)

# hasta tm --------------

limpiar <- function(txt) {
  txt <- tolower(txt)
  txt <- gsub("[^a-z ]","",txt)
  txt <- gsub("big data","big_data",txt, fixed = TRUE)
  txt <- gsub("â"," ",txt, fixed = TRUE)
  txt <- trimws(txt)
  return(txt)
}

abstracts$value <- limpiar(abstracts$value)

abs_tidy <- abstracts %>%
  unnest_tokens(output = word, input = value) %>%
  anti_join(stop_words) %>% # remove stopwords
  mutate(len = str_length(word)) %>% filter(len>2) %>% select(-len) %>% # remove 1-2 char words
  mutate(word=textstem::lemmatize_words(x = word %>% unlist()))  

abs_word_keep <- abs_tidy %>% count(id, word, sort = TRUE) %>% 
  bind_tf_idf(., word, id, `n`) %>% 
  select(word, tf_idf) %>% 
  arrange(desc(tf_idf)) %>% 
  filter(tf_idf > quantile(tf_idf, 0.9, na.rm = T)) %>% pull(word)

abs_dfm <- abs_tidy %>%
  filter(word %in% abs_word_keep) %>% 
  count(id, word, sort = TRUE) %>%
  cast_sparse(id, word, n)

# estimamos K ----------------------

library(furrr)
plan(multiprocess)

many_models <- data_frame(K = c(10, 20, 50, 100)) %>%
  mutate(topic_model = future_map(K, ~stm(abs_dfm, K = .,
                                          verbose = TRUE)))

heldout <- make.heldout(abs_dfm)

k_result <- many_models %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, abs_dfm),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, abs_dfm),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

k_result %>%
  transmute(K,
            `Lower bound` = lbound,
            Residuals = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (number of topics)",
       y = NULL,
       title = "Model diagnostics by number of topics")

k_result %>%
  select(K, exclusivity, semantic_coherence) %>%
  unnest() %>%
  mutate(K = as.factor(K)) %>%
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence")

topic_model <- k_result %>% 
  filter(K == 50) %>% 
  pull(topic_model) %>% 
  .[[1]]

topic_model



# ahora si, a darle atomos ------------


abs_tm <- stm(abs_dfm, K = 10, verbose = TRUE, init.type = "Spectral",
              data = abstracts %>% select(id,year), # covariamos con fecha
              prevalence=~year)

td_beta <- tidy(abs_tm)

td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) 

td_gamma <- tidy(abs_tm, matrix = "gamma", 
                 document_names = rownames(abs_dfm))

library(ggthemes)

top_terms <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(100, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest(cols=c(terms))

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

gamma_terms %>%
  #top_n(50, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005 ) +
  coord_flip() +
  #scale_y_continuous(expand = c(0,0),
  #  limits = c(0, 0.09)) +
  theme_tufte(ticks = FALSE) +
  theme(plot.title = element_text(size = 16 ),
        plot.subtitle = element_text(size = 13)) +
  labs(x = NULL, y = expression(gamma),
       title = "Top topics by prevalence")

gamma_terms %>%
  select(topic, gamma, terms) %>%
  knitr::kable(digits = 3, 
               col.names = c("Topic", "Expected topic proportion", "Top 7 terms"))

write_csv(td_gamma, "./data/tm_gamma_25.csv")
write_csv(td_beta, "./data/tm_beta_25.csv") 

# graficos --------------------

# para ver cambios en el tiempo ?
td_gamma %>% 
  group_by(topic) %>%
  slice_max(order_by = desc(gamma), n=100) %>%
  arrange(topic,desc(gamma)) %>%
  left_join(articles %>% select(document=id,year)) %>%
  filter(year>2012)%>%
  group_by(year) %>% mutate(n_anio=n()) %>% ungroup() %>%
  count(topic,n_anio,year) %>%
  ggplot(aes(x=year,y=n/n_anio,group=topic)) + geom_line(aes(colour=as.factor(topic)))

# para ver cambios en el tiempo ?
td_gamma %>% 
  group_by(topic) %>%
  slice_max(order_by = desc(gamma), n=100) %>%
  arrange(topic,desc(gamma)) %>%
  left_join(articles %>% select(document=id,year)) %>%
  filter(year>2012)%>%
  group_by(year) %>% mutate(n_anio=n()) %>% ungroup() %>%
  count(topic,n_anio,year) %>%
  ggplot(aes(x=year,y=n/n_anio)) + 
    geom_col() + 
  facet_wrap(~topic)

gamma_terms %>%
  select(topic, gamma) %>%
  knitr::kable(digits = 3)


# resultados interpretar --------------------

glimpse(tm_25)

topics <- tm_25 %>% group_by(Group) %>% summarise(t=paste(Topic, collapse = " , "))

topics

tm_gamma %>%
  filter(topic %in% as.integer(str_split(
    topics$t[3], 
    pattern = ",", simplify = TRUE) %>% unlist(.))) %>%
  left_join(metadata %>% 
              filter(tag=="keyword") %>%
              select(document=id, value)) %>%
  top_n(1000, wt = gamma) %>%
  select(-topic) %>%
  count(value, sort = TRUE)
