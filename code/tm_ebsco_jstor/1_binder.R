library(tidyverse)






# metadata ----------------

# solo estoy usando el dataset de scopus

ebsco_metadata <- readr::read_csv(file = "./data/ebsco_metadata.csv")
jstor_dfr_art_metadata <- readr::read_csv(file = "./data/jstor_dfr_art_metadata.csv")
scopus_metadata <- readr::read_csv(file = "./data/scopus_metadata.csv")

metadata <- rbind(
  jstor_dfr_art_metadata %>% mutate(source="jstor"),
  ebsco_metadata %>% mutate(source="ebsco"),
  scopus_metadata %>% mutate(source="scopus")
)
metadata <- rbind(
  scopus_metadata %>% mutate(source="scopus")
)

readr::write_csv(x = metadata, path = "./data/metadata.csv")

rm(jstor_dfr_art_metadata,ebsco_metadata,scopus_metadata)


# ocr -------------------------


jstor_dfr_art_ocr <- readr::read_csv(file = "./data/jstor_dfr_art_ocr.csv")
ebsco_ocr <- readr::read_csv(file = "./data/ebsco_ocr.csv")

ocr <- rbind(
  jstor_dfr_art_ocr %>% mutate(source="jstor"),
  ebsco_ocr %>% mutate(source="ebsco")
)

rm(jstor_dfr_art_ocr,ebsco_ocr)

readr::write_csv(x = ocr, path = "./data/ocr.csv")


# autores -------------------------

scopus_autores <- readr::read_csv(file = "./data/scopus_autores.csv")

autores <- rbind(
  scopus_autores %>% mutate(source="scopus")
)

readr::write_csv(x = autores, path = "./data/autores.csv")

rm(scopus_autores)







# desduplicar --------------



# opcion 1:
# por el nivel de repetición y suciedad, dejar afuera los de ebsco

# opcion 2:
# meter los ocr de ebsco en jstor
ebsco_ocr2 <- ebsco_ocr %>% inner_join(ebsco_metadata %>% filter(tag=="doi") %>% select(id,doi=value))




glimpse(metadata)
table(metadata$tag)
table(metadata$source)


metadata$registro <- seq_along(1:nrow(metadata))

sep1 <- metadata %>% filter(tag=="doi") 
table(sep1$source)

sep2 <- metadata %>% filter(tag=="doi") %>% distinct(value, .keep_all = TRUE)
table(sep2$source)

sep2 <- metadata %>% filter(tag=="doi" & source=="scopus") %>% distinct(value, .keep_all = TRUE)
sep2 <- metadata %>% filter(tag=="doi" & source=="ebsco") %>% distinct(value, .keep_all = TRUE)
table(sep2$source)

sep <- metadata[duplicated(sep$value),]

table(sep$source)

