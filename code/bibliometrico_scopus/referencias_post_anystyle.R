options(scipen = 999)

library(tidyverse)
library(igraph)
library(ggraph)
library(tidygraph)
library(bib2df)


metadata <- readr::read_csv(file = "./data/metadata.csv")
ocr <- readr::read_csv(file = "./data/ocr.csv")
autores <- readr::read_csv(file = "./data/autores.csv")
references <- readr::read_csv(file = "./data/scopus_references.csv")

# Referencias

# esto fue parseado con anystyle para ruby
# 
# https://anystyle.io/
#   https://rubygems.org/pages/download
# https://gist.github.com/zuphilip/a035db2329db7b1eb6c846c738c3e63f?short_path=856488d
# https://notebooks.gesis.org/binder/jupyter/user/zuphilip-a035db-6c846c738c3e63f-ft0p86h6/notebooks/sample.ipynb
# 
# los exporté como bib
# y limpie  BOM (byte order mark)
# https://stackoverflow.com/questions/30251576/reading-a-non-standard-csv-file-into-r
# los importé con bibdf

# anystyle -f bib parse references_limpio.txt > parseados.bib

# exporto las referencias para anystyle

glimpse(references)

references2 <- iconv(
  x = references$reference, 
  "UTF-8", "UTF-8", sub='') ## replace any non UTF-8 by ''

write.table(x = references2, 
            file = "./data/references_limpio.txt", 
            sep = "", quote = FALSE,row.names = FALSE, 
            col.names = FALSE, fileEncoding = "utf8")

# importo de anystyle

# refjson <- jsonlite::read_json(path = "../data/parseados.json")
# bibdf <- bib2df::bib2df(file = "../data/parseados.bib")

path <- "./data/parseados.bib"
csvLines = readLines(path, encoding="UTF-16LE", skipNul=T, warn=F)
csvLines2 <- csvLines %>% gsub(pattern = "ÿþ", replacement = "", x = csvLines)
write.table( x = paste(csvLines2, collapse="\n"),
             file = "./data/parseados_simbolos.bib")
bibdf <- bib2df::bib2df(file = "./data/parseados_simbolos.bib")

glimpse(bibdf)

table(bibdf$CATEGORY)

rm(bib2df,path,csvLines,csvLines2,references2)
rm(bibdf_1,bibdf_2,bibdf_3)

# borro el primer registro porque siempre se pierde en anystyle

references <- references[-c(1),]
bibdf$id <- references$id

saveRDS(bibdf, file = "./data/referencias.rds")
