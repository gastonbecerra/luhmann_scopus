bibtex_accented_characters <- function(x) {
  rep_str = c("{\\'{a}}"="á", "{\\'{e}}"="é", "{\\'{i}}"="í", "{\\'{o}}"="ó", "{\\'{u}}"="ú", "{\\~{n}}"="ñ", "{\\&}"="&")
  return(str_replace_all(x, pattern = fixed(rep_str)))
}

file_name <- function(x) {
  n <- length(x)
  z <- vector()
  for (i in 1:n) {
    y = NULL
    y <- str_split_1(x[i], pattern = fixed(":"))[3]
    y = paste0("e:", y)
    z[i] = y
  }
  return(z)
}
