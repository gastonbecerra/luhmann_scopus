# import dataset "Jstor (DfR)"
# - importar metadata
# - importar ocr
# - importar referencias
# mismo dataset tiene otro corpus (chapters) que no estoy incluyendo 


library(tidyverse)
library(foreach)
library(xml2)



# importar metadata ----------------------




path <- "./data/raw/jstor_dfr/metadata/"
xml_list <- list.files(path = path , pattern = "journal*.*")

## campos disponibles en los json 

xml_campos <- foreach::foreach(xml_name=xml_list, .combine = c ) %dopar% {
  xml_file <- xml2::read_xml(paste(path, xml_name, sep = ""))
  xml_children <- xml2::xml_children(xml2::xml_find_all(x = xml_file, xpath = "//journal-meta|//article-meta"))
  xml_names <- xml2::xml_name(xml_children)
  return(xml_names)
}

xml_campos <- unique( xml_campos )
xml_campos

campos_parents <- c("journal-meta","article-meta")
campos1 <- c("journal-title-group","publisher","issn")
campos2 <- c("title-group",
             "contrib-group/contrib/string-name/surname",
             "contrib-group/contrib/string-name/given-names",
             "aff/institution",
             "self-uri",
             "abstract",
             "custom-meta-group",
             "product","article-categories","kwd-group","counts",
             "trans-abstract",
             "pub-date[1]/day","pub-date[1]/month", "pub-date[1]/year")
campos <- c( paste(campos_parents[1],campos1,sep = "/") , paste(campos_parents[2],campos2,sep = "/") )
rm(campos_parents,campos1,campos2)

## extraer valores de los campos elegidos

extraer_campos <- function(campo, xml_nodos, xml_name){
  contenidos1 <- contenidos2 <- contenidos3 <- NULL
  contenidos1 <- xml2::xml_find_all(x=xml_nodos,xpath=paste0("//",campo))
  if (!is.null(contenidos1)) {
    contenidos2 <- xml2::as_list(x = contenidos1)
    if (!is.null(contenidos2)) {
      # print(unlist(contenidos2))
      contenidos3 <- paste(unlist(contenidos2),collapse = " $ ")
      registro <- cbind(tag=campo, value=contenidos3, id=xml_name)
      return(as.matrix(registro))
    }
  }
}

xml_metadata_lst <- foreach::foreach(xml_name=xml_list) %dopar% {
  xml_file <- xml2::read_xml(paste(path, xml_name, sep = ""))
  xml_campos <- lapply(X = campos, FUN = extraer_campos, xml_nodos = xml_file, xml_name = xml_name)
  xml_campos_tbl <- do.call(rbind, xml_campos)
}
xml_metadata <- do.call(rbind,xml_metadata_lst)
metadata <- data.frame(xml_metadata, stringsAsFactors = FALSE)
metadata <- metadata %>% filter(!is.na(value), value != "")

rm(xml_metadata_lst, xml_metadata)
rm(campos, xml_campos)
rm(extraer_campos)
rm(xml_list)



# limpio los tags

data <- metadata
rm(metadata)

table(data$tag)


# normalizo los id

limpiar_filename <- function( txt ) {
  txt <- str_match(string = txt, pattern = "[^/]+$")
  txt <- stringr::str_remove(txt, ".xml")
  txt <- stringr::str_remove(txt, ".txt")
  return(txt)
}

data$id <- limpiar_filename(data$id)

rm(limpiar_filename)

# relleno los abstracts que faltan

data %>% filter(tag=="abstract2") %>% glimpse()
data %>% filter(tag=="article-meta/abstract") %>% glimpse()

separar <- c("article-meta/abstract","abstract2")
sep <- data %>% filter(tag %in% separar) %>% 
  pivot_wider( names_from = tag, values_from = value ) %>% 
  rename(abstract1=2) %>%
  mutate(abstract = if_else(condition = is.na(abstract1),
                            true = abstract2,
                            false = abstract1)) %>%
  mutate(tag="abstract") %>%
  select(id,tag,value=abstract)
data <- data %>%  filter(!tag %in% separar) 
data <- rbind(data, sep)

# divido keywords 

separar <- c("article-meta/kwd-group")
separadores <- function(x) {
  y <- x
  # y <- gsub("<br>" , "$", y, fixed = TRUE)
  # y <- gsub("." , "$", y, fixed = TRUE)
  # y <- gsub("," , "$", y, fixed = TRUE)
  # y <- gsub(";" , "$", y, fixed = TRUE)
  # y <- gsub(":" , "$", y, fixed = TRUE)
  # y <- gsub("– " , "$", y, fixed = TRUE)
  # y <- gsub("  " , "$", y, fixed = TRUE)
  y <- gsub("$" , " $ ", y, fixed = TRUE)
  return(y)
} 
sep <- data %>% filter(tag %in% separar) # primero los filtro. mismo criterio para reemplazar
sep <- sep %>% mutate(value3=separadores(value)) # los proceso, divido, y rejunto
kps <- as.data.frame(str_split_fixed(sep$value3, pattern = fixed("$"), n=50),stringsAsFactors = FALSE)
sep <- cbind(sep,kps)
rm(kps)
sep2 <- sep %>% 
  select(-value,-value3) %>%
  tidyr::pivot_longer(cols = c(-tag, -id), names_to = "v", values_to = "value") %>% 
  transform(value = gsub("[^[:alnum:][:space:]]","",value) %>% tolower() %>% trimws() ) %>%
  filter(value != "") %>%
  select(tag,value,id)
sep2 <- sep2 %>% mutate(tag="keyword") 
data <- data %>% anti_join( data %>% filter(tag %in% separar) )
data <- rbind(data,sep2)

data %>% filter(tag=="keyword") %>% glimpse()
data %>% filter(tag=="keyword") %>% select(id) %>% distinct(id) %>% pull(id) %>% length()

rm(sep,separar,separadores,sep2)

# fuentes 

sep <- data %>% filter(tag == "journal-meta/journal-title-group")
sep <- sep %>% mutate(
  value = tolower(trimws(value)), 
  tag = "journal"
)
data <- data %>% anti_join( data %>% filter(tag == "journal-meta/journal-title-group") )
data <- rbind(data,sep)
rm(sep)

# anio 

sep <- data %>% filter(tag == "article-meta/pub-date[1]/year")
sep <- sep %>% mutate(value = as.integer(sep$value), tag = "year")
sep <- sep %>% filter(!is.na(value))
# data <- data %>% anti_join( data %>% filter(tag == "fuente") )
data <- rbind(data,sep)
rm(sep)

# titulo

sep <- data %>% filter(tag == "article-meta/title-group") %>% mutate(tag="titulo")
data <- data %>% anti_join( data %>% filter(tag == "article-meta/title-group") )
data <- rbind(data,sep)
rm(sep)


# filtro metatags relevantes 


separar <- c("id", "abstract", "keyword", "references", "journal", "year", "autor", "aff", "doi", "orcid", "titulo")

data2 <- data %>% filter(tag %in% separar)

table(data2$tag)

readr::write_csv(x = data2 %>% select(id,tag,value), path = "./data/jstor_dfr_art_metadata.csv")

rm(limpiar_filename, ocr_txt)






# importar ocr --------------







path <- './data/raw/jstor_dfr/ocr/'
ocr_list <- list.files(path = path, pattern = "journal*.*")

ocr_txt <- foreach::foreach(xml_name=ocr_list, .combine = c) %dopar% {
  xml_file <- readr::read_file(paste(path, xml_name, sep = ""))
}
ocr_txt <- data.frame( cbind(id=ocr_list, fulltext=ocr_txt) , stringsAsFactors = FALSE)

rm(ocr_list, path)

# limpiar ocr rapido

ocr_txt$fulltext <- gsub("<.*?>", "", ocr_txt$fulltext)

ocr_txt$abstract2 <- stringr::word(string = ocr_txt$fulltext, start = 1, end = 300)

readr::write_csv(x = ocr_txt %>% select(id,fulltext) , 
                  path = "./data/jstor_dfr_art_ocr.csv")



# 2do: usar texto para los abstracts faltantes
# esto hay que rehacerlo, y volver a grabar

# metadata <- rbind(metadata,
#                   ocr_txt %>% 
#                     rename(value=abstract2) %>% 
#                     mutate(tag="abstract2") %>% select(tag,value,id))
# metadata <- metadata %>% filter(value != "")
# 
# 
# readr::write_csv(x = data2 %>% select(id,tag,value), path = "./data/jstor_dfr_art_metadata.csv")




# autores --------------------



# autores

corregir_nombres <- function(p) {
  retTag <- c()  
  for (i in 1:length(p) ) {
    x <- p[i]
    y <- str_split(pattern = fixed("$$"), string = x, simplify = TRUE, n = 2)
    apellidos <- trimws(y[1])
    nombres <- trimws(y[2])
    z1 <- str_split(pattern = fixed("$"), string = apellidos, simplify = TRUE)
    z2 <- str_split(pattern = fixed("$"), string = nombres, simplify = TRUE)
    n <- ""
    for (j in 1:length(z1)) {
      n <- paste(n, trimws(z1[j]) , ", " , trimws(z2[j]), sep = "" )
      if (j < length(z1)) {  n <- paste(n, " ; ", sep = "" ) }
    }
    retTag <- c(retTag, n)
  }
  return(retTag)
}

sep <- data %>% filter(tag %in% c("article-meta/contrib-group/contrib/string-name/given-names" ,
                                  "article-meta/contrib-group/contrib/string-name/surname")) %>% 
  pivot_wider(names_from = "tag", values_from = "value") %>%
  mutate(autores = paste(
    `article-meta/contrib-group/contrib/string-name/surname` ,
    `article-meta/contrib-group/contrib/string-name/given-names` ,
    sep = " $$ " )) %>%
  select(id, autores) 
sep$autores2 <- corregir_nombres(sep$autores)
kps <- as.data.frame(str_split_fixed(sep$autores2, pattern = fixed(";"), n=25),stringsAsFactors = FALSE)
sep <- cbind(sep,kps)
rm(kps)
autores <- sep %>% 
  tidyr::pivot_longer(cols = c(-autores, -id, -autores2), names_to = "v", values_to = "value") %>% 
  transform(
    value = value %>% tolower() %>% trimws(),
    orden = gsub("[^[:digit:]]","",v)
    ) %>%
  filter(value != "") %>%
  select(id,autor=value,orden)


glimpse(autores)

sep2 %>% select(id) %>% distinct(id) %>% pull(id) %>% length() # documentos con autores

rm(sep, sep2, corregir_nombres)

# afiliaciones

# podría hacer lo con afiliaciones (article-meta/aff) pero hay muy poco caso
# asi que intentamos con orcid

library('rorcid')

autores_jstor <- autores %>% pull(autor) %>% unique(.)

orcid_jstor <- foreach::foreach(i=1:length(autores_jstor), .combine = rbind) %do% {
  x2<-str_split_fixed(string = autores_jstor[i], pattern = ", ", n = 2)
  x3<-rorcid::orcid_search(given_name = x2[1,2], family_name = x2[1,1])  
  x3$criteria<-autores_jstor[i]
  print(paste(i, autores_jstor[i]))
  Sys.sleep(time = 0.5)
  return(x3)
}

rm(x,x2,x3,autores_jstor,i)

glimpse(orcid_jstor)

library(stringdist)

orcid_jstor2 <- orcid_jstor %>% 
  mutate(criteria2=paste(last,first, sep = ", ") %>% tolower()) %>%
  mutate(d=stringdist(criteria,criteria2)) %>%
  filter(d<5) 

orcid_jstor2 <- orcid_jstor2[!duplicated(orcid_jstor2$criteria2), ]

orcid_jstor2

autores <- autores %>% left_join(orcid_jstor2 %>%
                        select(autor=criteria,orcid))

length(unique(autores$orcid))

rm(orcid_jstor2,orcid_jstor)

autores_orcid <- autores %>% select(orcid) %>% 
  filter(!is.na(.)) %>%
  distinct(orcid)  %>%
  pull(orcid)

autores_orcid[1:5]

glimpse(autores)

autores %>% mutate(
  aff=NA,
  aid=NA,
  pais=NA
) %>% select(
  id, autor, orden, aff, aid, pais, orcid
) %>% 
  readr::write_csv(. , path = "./data/jstor_dfr_art_autores.csv")

# x<-rorcid::orcid_person(autores_orcid[1])
# x<-orcid_employments(orcid = "0000-0002-1642-628X")

rm(autores_orcid)





# importar referencias ------------





path <- './data/raw/jstor_dfr/metadata/'
xml_list <- list.files(path = path, pattern = "journal*.*")

xml_refs <- foreach::foreach(xml_name=xml_list ) %dopar% {
  xml_file <- xml2::read_xml(paste(path, xml_name, sep = ""))
  xml_content <- xml2::xml_find_all(x = xml_file, xpath = "//ref/mixed-citation")
  xml_content2 <- xml2::xml_text(x = xml_content)
  return(cbind(id=xml_name,value=paste(xml_content2, collapse = " $ ")))
}
xml_refs2 <- do.call(what = rbind, xml_refs) %>% as.data.frame()
xml_refs3 <- as.data.frame(
  cbind(
    id=xml_refs2$id,
    value=str_split_fixed( xml_refs2$value, pattern = fixed("$"), n=300)
  ), stringsAsFactors = FALSE) %>%
  tidyr::pivot_longer(
    cols = -id, names_to = "order", values_to = "ref_text",
    values_drop_na = TRUE
  ) %>%
  mutate(ref_text=str_trim(ref_text)) %>%
  filter(ref_text!="")

glimpse(xml_refs3)

xml_refs3 %>% rename(reference=ref_text) %>% select(-order) %>% 
  readr::write_csv(path = "./data/jstor_dfr_art_references.csv")

rm(xml_refs, xml_refs2)
rm(xml_list)
rm(path, xml_refs3)

