## utils 
#' @export
removeAccents <- function(string){
  accents <- "àèìòùÀÈÌÒÙáéíóúýÁÉÍÓÚÝäëïöüÄËÏÖÜâêîôûñÑç"
  translation <- "aeiouAEIOUaeiouyAEIOUaeiouAEIOUaeiouÂÊÎÔÛAEIOUnNc"
  chartr(accents, translation, string)
}  

#' @export
dictionaryMatch <- function(inputStr,dict){
  l <- lapply(inputStr, function(inputStr){    
    inputStr <- tolower(inputStr)
    inputStr <- removeAccents(inputStr)
    dict_tmp <- tolower(dict)
    dict_tmp <- removeAccents(dict_tmp)
    tmp <- adist(inputStr, dict_tmp)
    tmp <- as.vector(tmp)
    dict[which.min(tmp)]
  })
  unlist(l)
}


