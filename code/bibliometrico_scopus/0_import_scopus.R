# import dataset "SCOPUS"
# - importar metadata
# - importar ocr
# - importar referencias



library(tidyverse)
library(foreach)
library(xml2)

# importar metadata ------------

# leemos los txt y los metemos en una lista donde cada txt es un elemento
# que contiene las lineas del txt como vectores unidimensionales

path <- "./data/raw/scopus2/"
txt_file_list <- list.files(path = path, recursive = TRUE) 
txt_lines <- foreach::foreach(txt_file=txt_file_list) %do% {
  return( readLines(con = paste0(path, txt_file)) )
}

# cortamos el contenido de los txt en registros 
# (1 registro = vector con x strings, 1 por cada linea )

t <- NULL
loger <- NULL
i=0
j=0
registros <- list()
txt_articles_file <- list()

for (k in 1:length(txt_lines)) {
  txt <- txt_lines[[k]]
  for (i in 1:length(txt)) {
    # los primeros 3 lineas de cada txt son encabezados
    if (i>3) { 
      # guardamos cada linea del txt en un vector
      if (txt[i] != "") { loger <- c(loger, txt[i]) }
      # si llegamos al final del registro, exportamos el log
      if (txt[i] == "SOURCE: Scopus") { 
        j=j+1
        #print(j)
        registros[[j]] <- loger
        loger <- NULL
      }
    }
  }
  txt_articles_file[[k]] <- registros
}

rm(t,i,loger,j,txt,k)

# parseamos cada linea de texto (vectores en registros)
# todo este codigo hay qeu optimizarlo! sobre 5000 registros puede tomar 1 hora
# el cuelgue está en la forma de agregar un elemento a la lista

metadata_lst <- list()
for (k in 1:length(registros)) { 
  r <- registros[[k]]  
  for (i in 1:length(r)) {
    newdata <- NULL
    if (i < 6) { # los primeros 3 identifican el articulo
      newdata <- as.data.frame( cbind( 
        registro = k,
        tag = switch (i, "autores", "orcid", "titulo", "fuente", "url" ) , 
        value = as.character(r[i])) , 
        stringsAsFactors = FALSE )
    } else {
      x <- str_split(string = r[[i]], pattern = ":", simplify = TRUE)  
      if (length(x) > 1) { # hay encabezado?
        if (x[1]==toupper(x[1])) { # si es todo mayuscula, es encabezado
          last_tag <- tolower(trimws(x[1]))
          newdata <- as.data.frame( cbind( 
            registro = k,
            tag = last_tag , 
            value = as.character( paste( x[2:length(x)] , collapse = ":" ))) , 
            stringsAsFactors = FALSE )
        } else {
          if (last_tag == "references") {
            newdata <- as.data.frame( cbind( 
              registro = k,
              tag = last_tag , 
              value = as.character( paste( x[1:length(x)] , collapse = ":" ))) , 
              stringsAsFactors = FALSE )        
          }
        }
      } else {
        if (last_tag == "references") {
          newdata <- as.data.frame( cbind( 
            registro = k,
            tag = last_tag , 
            value = as.character( x[1] )), 
            stringsAsFactors = FALSE )        
        }
        if (last_tag == "affiliations") {
          newdata <- as.data.frame( cbind( 
            registro = k,
            tag = last_tag , 
            value = as.character( x[1] )), 
            stringsAsFactors = FALSE )        
        }        
      } 
    }
    if (!is.null(newdata)) {
      # esto habria que optimizarlo
      metadata_lst[[ paste0(k,"_",i) ]] <- newdata
      # print(paste0(k,"_",i))
    }
  }
}
rm(r,x,i,last_tag,k,p,q)
rm(newdata)

data <- bind_rows(metadata_lst)

table(data$tag)

# les pongo un id

data <- data %>% mutate(id = paste0("scopus_",registro))
glimpse(data)

# limpio los tags

data$tag <- gsub("[^[:alnum:][:space:]]","",data$tag) %>% tolower() %>% trimws()
table(data$tag)

# se piantaron algunos tag, del parseo de registros, los borro

table(data$tag)
tags_sueltos <- data %>% group_by(tag) %>% summarise(n=n()) %>% 
  filter(n<20) %>% pull(tag) 
data <- data %>% anti_join( data %>% filter(tag %in% tags_sueltos) )
table(data$tag)
rm(tags_sueltos)
rm(registros,txt_articles_file,txt_file,txt_file_list,txt_lines, metadata_lst, path)

saveRDS(object = data, file = "./data/scopus_data.rds")
data <- readRDS(file = "./data/scopus_data.rds")


# metadata -----------------


## divido las keywrods

separar <- c("author keywords","index keywords")
separadores <- function(x) {
  y <- x
  y <- gsub("<br>" , "$", y, fixed = TRUE)
  #y <- gsub("." , "$", y, fixed = TRUE)
  #y <- gsub("," , "$", y, fixed = TRUE)
  y <- gsub(";" , "$", y, fixed = TRUE)
  y <- gsub(":" , "$", y, fixed = TRUE)
  y <- gsub("– " , "$", y, fixed = TRUE)
  y <- gsub("  " , "$", y, fixed = TRUE)
  y <- gsub("$" , " $ ", y, fixed = TRUE)
  return(y)
} 
sep <- data %>% filter(tag %in% separar) # primero los filtro. mismo criterio para reemplazar
sep %>% group_by(tag) %>% tally(sort = TRUE)
sep <- sep %>% mutate(value3=separadores(value)) # los proceso, divido, y rejunto
kps <- as.data.frame(str_split_fixed(sep$value3, pattern = fixed("$"), n=50),stringsAsFactors = FALSE)
sep <- cbind(sep,kps)
rm(kps)
sep2 <- sep %>% 
  select(-value,-value3) %>%
  tidyr::pivot_longer(cols = c(-registro, -tag, -id), names_to = "v", values_to = "value") %>% 
  transform(value = gsub("[^[:alnum:][:space:]]","",value) %>% tolower() %>% trimws() ) %>%
  filter(value != "") %>%
  select(registro,tag,value,id)
sep2 <- sep2 %>% mutate(tag="keyword") 
data <- data %>% anti_join( data %>% filter(tag %in% separar) )
data <- rbind(data,sep2)

rm(sep,separar,separadores,sep2)

data %>% filter(tag=="keyword") %>% select(id) %>% distinct(id) %>% pull(id) %>% length()

## año

extraer_anio <- function(fuente) {
  retTag <- c()  
  for (i in 1:length(fuente)) {
    y2 <- y <- fuente[i]
    
    y <- stringr::str_extract(y2, "\\(\\d{4}\\)") # (2019)
    y <- parse_number(y)
    tag=as.integer(y)
    retTag <- c(retTag,tag)
  }
  return( retTag )
}
sep <- data %>% filter(tag == "fuente")
sep <- sep %>% mutate(value = extraer_anio(sep$value), tag = "year")
data <- rbind(data , sep)
rm(sep, extraer_anio)

## fuentes 

extraer_fuente <- function( value) {
  value2 <- value
  value2 <- substr(x = value, start = 8, stop=length(value))
  value2 <- gsub("^(.+?)(?=\\d).*", "\\1", value2, perl = TRUE) 
  value2 <- trimws(value2)
  value2 <- substr(x = value2, start = 1, stop=nchar(value2)-1)
  value2 <- tolower(value2) %>% trimws()
  return(value2)
}
fuente <- data %>% filter(tag=="fuente") %>%
  mutate(
    value = extraer_fuente(value),
    tag = "journal"
  )
data <- rbind(data , fuente)
rm(fuente, extraer_fuente)

## cited 

cited <- data %>% filter(tag=="fuente") %>%
  mutate(
    value = gsub("[^[:digit:]]","", sapply(strsplit(value, "Cited"), "[", 2) ) %>% as.integer() ,
    tag = "cited"
  ) %>% 
  filter(!is.na(value))
data <- rbind(data , cited)
rm(cited)


separar <- c("abstract", "keyword", "journal", 
             "year", "doi", "titulo", "cited")

data2 <- data %>% filter(tag %in% separar) %>% select(id,tag,value)

glimpse(data2)
table(data2$tag)

readr::write_csv(x = data2, path = './data/scopus_metadata.csv')



# references -----------------------------------------



separar <- c("references")

data2 <- data %>% filter(tag %in% separar) %>% select(id,reference=value)

glimpse(data2)

readr::write_csv(x = data2, path = './data/scopus_references.csv')




# autores --------------------------



sep <- data %>% filter(tag=="autores")
kps <- as.data.frame(str_split_fixed(sep$value, pattern = fixed(".,"), n=50),stringsAsFactors = FALSE)
sep <- cbind(sep,kps)
rm(kps)
autores <- sep %>% 
  select(-value) %>%
  tidyr::pivot_longer(cols = c(-registro, -tag,- id), names_to = "v", values_to = "value") %>% 
  transform(
    value = gsub("[^[:alnum:][:space:]]","",value) %>% tolower() %>% trimws() ,
    orden = gsub("[^[:digit:]]","",v)
    ) %>%
  filter(value != "") %>%
  mutate(orden=as.integer(orden)) %>%
  select(id,autor=value,orden)
rm(sep)

aff <- data %>% filter(tag=="affiliations") %>%
  mutate(value = gsub("[^[:alnum:][:space:][:punct:]]","",value) %>% tolower() %>% trimws()) %>%
  group_by(id) %>%
  mutate(orden = row_number()) %>%
  filter(value != "") %>%
  select(id,aff=value,orden)

sep <- data %>% filter(tag=="orcid")
kps <- as.data.frame(str_split_fixed(sep$value, pattern = fixed(";"), n=50),stringsAsFactors = FALSE)
sep <- cbind(sep,kps)
rm(kps)
aid <- sep %>% 
  select(-value) %>%
  tidyr::pivot_longer(cols = c(-registro, -tag, -id), names_to = "v", values_to = "value") %>% 
  transform(
    value = gsub("[^[0-9]","",value),
    orden = gsub("[^[:digit:]]","",v)
    ) %>%
  filter(value != "") %>%
  mutate(orden=as.integer(orden)) %>%
  select(id,aid=value,orden)
rm(sep)

autores <- autores %>% 
  left_join( aff ) %>%
  left_join( aid ) 

rm(aff,aid)

extraer_pais <- function( value ) {
  retTag <- c() 
  for(i in 1:length(value)) {
    if (!is.na(value[i])) {
      value2 <- str_split(value[i],pattern = ",")
      value3 <- value2[[1]][[length(value2[[1]])]] 
      value3 <- gsub("[^[:alpha:][:space:]]","",value3) %>% tolower() %>% trimws()
      retTag <- c(retTag,value3)  
    } else {
      retTag <- c(retTag,NA)  
    }
  }
  return(retTag)
}

autores$pais <- extraer_pais(autores$aff) 
rm(extraer_pais)

glimpse(autores)
table(autores$pais)
sum(!is.na(autores$pais))
length(unique(autores$pais))

readr::write_csv(x = autores, path = './data/scopus_autores.csv')

## 2do: puedo conseguir orcid?

# library('rorcid')
# # bsucar x nomrbe?
# autores_scopus <- autores %>% pull(autor) %>% unique(.)
# autores_scopus
# orcid_scopus <- foreach::foreach(i=1:length(autores_scopus), .combine = rbind) %do% {
#   x2<-str_split_fixed(string = autores_jstor[i], pattern = ", ", n = 2)
#   x3<-rorcid::orcid_search(given_name = x2[1,2], family_name = x2[1,1])  
#   x3$criteria<-autores_jstor[i]
#   print(paste(i, autores_jstor[i]))
#   Sys.sleep(time = 0.5)
#   return(x3)
# }
# 
# # buscar por doi?
# autores$aid
# x <- data %>% filter(tag=="doi") %>% pull(value)
# dois = x[1:5]
# dois
# x<-rorcid::orcid_doi(dois = x[1:5])
# y<-rorcid::orcid_id(orcid = "0000-0002-5909-9661")
# 
# rm(x,x2,x3,autores_scopus,i,separar)




# ocr ---------------------


# 2do: puedo tomar los ocr con fulltext o cminer?
# library(fulltext)
# https://cran.r-project.org/web/packages/fulltext/fulltext.pdf


