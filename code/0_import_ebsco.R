# import dataset ebsco
# - importar metadata
# - separar / importar ocr 
# - separar / importar referencias



library(tidyverse)
library(foreach)
library(xml2)
library(rvest)
library(doParallel)
library(jsonlite)

# import ebsco metadata y ocr -------------------

path <- './data/raw/ebsco_full/'
global_metadata <- data.frame()
global_content <- data.frame()
html_todos <- list.files(path = path, recursive = TRUE) 
html_metadata_list <- foreach::foreach(html_file=html_todos, .combine = list) %do% {
  html_content <- xml2::read_html(paste0(path,html_file))
  
  # metatags
  registros <- xml2::xml_find_all(x=html_content ,xpath='//*[@id="records"]/dl')
  i=0
  for (i in 1:length(registros)) {
    tags <- xml2::xml_children(registros[[i]]) %>% xml_name()
    for (j in 1:length(tags)) {
      # busco secuencia
      if ((tags[j] == "dt") && (tags[j+1] == "dd")) {  
        x<-data.frame( 
          registro = i ,
          tag = xml2::xml_child(registros[[i]], search = j) %>% xml_text() ,
          value = xml2::xml_child(registros[[i]], search = j+1) %>% xml_text() %>% as.character() ,
          html = xml2::xml_child(registros[[i]], search = j+1) %>% xml_contents() %>% as.character() ,
          file = html_file
        )
        global_metadata <- rbind(global_metadata,x)
      }
    }
    print(paste(html_file,i))          
  
  }

  # full-content
  content <- xml2::xml_find_all(x=html_content, xpath='//*[@id="records"]/div[@class="print-citation"]')
  for (j in 1:length(content)) {
    title <- xml2::xml_find_all(x=content[[j]], xpath='h1') %>% xml2::xml_text() %>% as.character()
    x<-data.frame( 
      title = title,
      tag = 'ocr',
      value = as.character(content[j]),
      file = html_file
    )
    global_content <- rbind(global_content,x)
    
    print(paste("content: ",title))
  }
  
}

rm(html_todos, html_metadata_list, html_content, html_file, registros, x, i, j, 
   tags, content, title, path)
global_metadata <- global_metadata %>% 
  mutate_if(is.factor, .funs = as.character) %>%
  mutate(id= paste0("ebsco_",gsub("[^[:alnum:][:space:]]","",file),"_",registro)) %>%
  select(id,tag,value,html)


## matcheo ocr y metatags

limpiar <- function(txt) {
  txt = tolower(gsub("[^[:alnum:][:space:]]","",txt))
  return(stringr::str_trim(txt))
}

global_content2 <- global_content %>% 
  mutate(title=limpiar(title)) %>% 
  inner_join(
    global_metadata %>% filter(tag=="Título:") %>% mutate(title=limpiar(value)) %>%
  select(title,id), 
  by="title") %>% 
  select(id,fulltext=value)

rm(limpiar)


## limpiar ocr rapido

global_content2$fulltext <- gsub("<.*?>", "", global_content2$fulltext)

global_content2 %>% readr::write_csv(x = ., path = './data/ebsco_ocr.csv')


glimpse(global_refs)
glimpse(global_content2)
glimpse(global_metadata)



## limpieza 


# junto metadata y referencias

data <- rbind(global_metadata , global_refs %>% mutate(html=""))

glimpse(data)

# limpio los tags

data$tag <- gsub("[^[:alnum:][:space:]]","",data$tag) %>% tolower() %>% trimws()

table(data$tag)

# divido los que tienen mas de 1 valor

separar <- c("materias","palabras clave proporcionadas por el autor",
             "términos temáticos","author supplied keywords", "términos geográficos", "empresaentidad", 
             "gente", "revisiones y productos")
separadores <- function(x) {
  y <- x
  y <- gsub('<span class="medium-normal">', "", y, fixed = TRUE)
  y <- gsub('</span>', "", y, fixed = TRUE)
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
sep <- sep %>% mutate(value3=separadores(html)) # los proceso, divido, y rejunto
kps <- as.data.frame(str_split_fixed(sep$value3, pattern = fixed("$"), n=50),stringsAsFactors = FALSE)
sep <- cbind(sep,kps)
rm(kps)
sep2 <- sep %>% 
  select(-value,-value3) %>%
  tidyr::pivot_longer(cols = c(-tag, -id, -html), names_to = "v", values_to = "value") %>% 
  transform(value = gsub("[^[:alnum:][:space:]]","",value) %>% tolower() %>% trimws() ) %>%
  filter(value != "") %>%
  select(id,tag,value,html)
sep2 <- sep2 %>% mutate(tag="keyword") 
data <- data %>% anti_join( data %>% filter(tag %in% separar) )
data <- rbind(data,sep2)

sep2 %>% select(id) %>% distinct(id) %>% pull(id) %>% length()

rm(sep,separar,separadores,sep2)

# fuentes 

extraer_fuente <- function(fuente) {
  retTag <- c()  
  for (i in 1:length(fuente)) {
    y <- fuente[i]
    y <- gsub("." , "$", y, fixed = TRUE)
    y <- gsub(";" , "$", y, fixed = TRUE)
    tag=sub("\\$.*", "", y)
    retTag <- c(retTag,tag)
  }
  return( retTag )
}
sep <- data %>% filter(tag == "fuente")
sep <- sep %>% mutate(value = extraer_fuente(sep$value), tag = "journal")
data <- rbind(data,sep)
rm(sep, extraer_fuente)

# anios 

extraer_anio <- function(fuente) {
  retTag <- c()  
  for (i in 1:length(fuente)) {
    y2 <- y <- fuente[i]
    y <- stringr::str_extract(y, "[a-zA-Z]{3,}\\d{4}") # Sep2019
    if (is.na(y)){
      y <- stringr::str_extract(y2, "/\\d{4}") # /2015
      if (is.na(y)){
        y <- stringr::str_extract(y2, " \\d{4}") # /2015
      }
    }
    y <- parse_number(y)
    tag=as.integer(y)
    retTag <- c(retTag,tag)
  }
  return( retTag )
}
sep <- data %>% filter(tag == "fuente")
sep <- sep %>% mutate(value = extraer_anio(sep$value), tag = "year")
# data <- data %>% anti_join( data %>% filter(tag == "fuente") )

table(sep$value)

data <- rbind(data,sep)
rm(sep, extraer_anio)

# resumen 

data <- data %>% mutate(tag = ifelse(test = tag == "resumen inglés", yes = "resumen", no = tag))
sep <- data %>% filter(tag == "resumen")
sep <- sep %>% mutate(tag = "abstract")
# data <- data %>% anti_join( data %>% filter(tag == "fuente") )
data <- rbind(data,sep)
rm(sep)

# titulo

sep <- data %>% filter(tag == "título") %>% mutate(tag="titulo")
data <- data %>% anti_join( data %>% filter(tag == "título") )
data <- rbind(data,sep)
rm(sep)


separar <- c("abstract", "keyword", "references", "journal", "year", "autor", "aff", "doi", "orcid", "titulo")

data2 <- data %>% filter(tag %in% separar) %>% select(id,tag,value)

table(data2$tag)
glimpse(data2)
readr::write_csv(x = data2, path = './data/ebsco_metadata.csv')




# importar autores -------------------------




extraer_autores <- function(autores) {
  retTag <- c()  
  for (i in 1:length(autores)) {
    y <- autores[i]
    y <- gsub('<span class="medium-normal">', "", y, fixed = TRUE)
    y <- gsub('</span>', "", y, fixed = TRUE)
    y <- gsub('(AUTHOR)', "", y, fixed = TRUE)
    y <- gsub('<sup>', "|", y, fixed = TRUE)
    z <- str_split(string = y, pattern = "<br>", simplify = TRUE)
    x <- ""
    for (j in 1:length(z)) {
      x <- paste( x , sub("\\|.*", "", trimws(z[j])), sep = "$")
    }
    retTag <- c(retTag, x)
  }
  return( retTag )
}

sep <- data %>% filter(tag == "autores")
sep <- sep %>% mutate(value = extraer_autores(sep$html))
kps <- as.data.frame(str_split_fixed(sep$value, pattern = fixed("$"), n=25),stringsAsFactors = FALSE)
sep <- cbind(sep,kps)
rm(kps)
sep2 <- sep %>% 
  select(-value) %>%
  tidyr::pivot_longer(cols = c(-tag, -id, -html), names_to = "v", values_to = "value") %>% 
  # transform(value = gsub("[^[:alnum:][:space:]]","",value) %>% tolower() %>% trimws() ) %>%
  transform(value = value %>% tolower() %>% trimws() ) %>%
  filter(value != "") %>%
  select(id,tag,value)
sep2 <- sep2 %>% mutate(tag="autor") 
data <- rbind(data,sep2 %>% mutate(html=""))

autores <- sep2 %>% select(id,autor=value) %>%
  mutate(orden=NA, aff=NA, aid=NA, pais=NA, orcid=NA) 

autores %>% readr::write_csv(path = "./data/ebsco_autores.csv")

rm(sep, sep2, extraer_autores)

data %>% filter(tag=="autor") %>% select(id) %>% distinct(id) %>% pull(id) %>% length()


# afiliaciones -------------------------

# al borrar las posiciones, los autores y las afiliaciones no estan matcheadas

extraer_afiliaciones <- function(autores) {
  retTag <- c()  
  for (i in 1:length(autores)) {
    y <- autores[i]
    y <- gsub('<span class="medium-normal">', "", y, fixed = TRUE)
    y <- gsub('</span>', "", y, fixed = TRUE)
    y <- gsub('(AUTHOR)', "", y, fixed = TRUE)
    #y <- gsub('<sup>', "|", y, fixed = TRUE)
    z <- str_split(string = y, pattern = "<br>", simplify = TRUE)
    x <- ""
    for (j in 1:length(z)) {
      # x <- paste( x , sub("\\|.*", "", trimws(z[j])), sep = "$")
      f <- substr(trimws(z[j]), start = str_length("<sup>1</sup>")+1, stop=str_length(trimws(z[j])))
      x <- paste( x , f, sep = "$")
      
    }
    retTag <- c(retTag, x)
  }
  return( retTag )
}

sep <- data %>% filter(tag == "afiliaciones del autor")
sep <- sep %>% mutate(value = extraer_afiliaciones(sep$html))
kps <- as.data.frame(str_split_fixed(sep$value, pattern = fixed("$"), n=25),stringsAsFactors = FALSE)
sep <- cbind(sep,kps)
rm(kps)
sep2 <- sep %>% 
  select(-value) %>%
  tidyr::pivot_longer(cols = c(-tag, -id, -html), names_to = "v", values_to = "value") %>% 
  # transform(value = gsub("[^[:alnum:][:space:]]","",value) %>% tolower() %>% trimws() ) %>%
  transform(value = value %>% tolower() %>% trimws() ) %>%
  filter(value != "") %>%
  select(id,tag,value)
sep2 <- sep2 %>% mutate(tag="aff") 

sep2

rm(sep, sep2, extraer_afiliaciones)

data %>% filter(tag=="aff") %>% select(id) %>% distinct(id) %>% pull(id) %>% length()





# importar referencias ---------------------




global_refs <- data.frame()
for (i in 1:length(global_content2$fulltext)) {
  print(i)
  p <- xml2::read_html(global_content2$fulltext[i]) %>% 
    xml2::xml_find_all(xpath = '//div[@class="print-citation"]') %>%
    xml2::xml_children()
  id <- global_content2$id[i]
  first <- last <- NULL
  j=0
  for(pp in p) {
    j=j+1
    if (grepl( "ref_toc", as.character(pp), fixed = TRUE)) {first=j}
    if (xml2::xml_text(pp)=="~~~~~~~~") {last=j}
  }
  if (!is.null(first) && !is.null(last)) {
    global_refs <- rbind(global_refs ,
                         data.frame(cbind( id = id, 
                                           tag = "references",
                                           value = xml_text(p[(first+1):(last-1)]) %>%
                                             trimws()
                         )))
  }
}

rm(p,first,last,j,i,pp,id)

global_refs %>% select(id) %>% distinct(id) %>% pull(id) %>% length()








