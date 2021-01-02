library(tidyverse)
library(foreach)

# importar datos --------------------------------------------


if (!exists("metadata")) {metadata <- readr::read_csv(file = "./data/metadata.csv")}
if (!exists("ocr")) {ocr <- readr::read_csv(file = "./data/ocr.csv")}

table(metadata$tag)

# muestra metadatos --------------------------------------------

campos <- c("abstract", "keyword", "references", "journal", 
             "year", "autor", "doi", "titulo", "cited")

print("tabla de metadatos")

foreach::foreach(k=campos, .combine = rbind) %do% {
  metadata %>% filter(tag==k) %>% count(source) %>% mutate(tag=k)
} %>% 
  pivot_wider(data = ., names_from = tag, values_from = n) %>% 
  print()

rm(k,campos)

# sample2 <- sample %>% select(-source,-titulo) / sample$titulo
# sample2 <- sample2 %>% rename_if(is.numeric, .funs = funs(paste0(.,"_p")))
# cbind(sample,sample2) %>% readr::write_csv(path = "./data/sample.csv")
# print(cbind(sample,sample2))


