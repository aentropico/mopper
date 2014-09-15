#library(stringi)
library(dplyr)
library(plyr)

#' Lastword
#' @name mopLastWord
#' @description lastWord
#' @param string
#' @return logical TRUE if successful, FALSE if directory already exists
#' @export
#' @examples \dontrun{
#' }
mopLastWord <- function(s, cols=NULL){
    getLastWord <- function(str){
      #str <- "Caserío La Mesa-Veredas La Mesa, La Danta y Mulatos"
      stringi:::stri_extract_last(str, regex="\\w+")
      ##stringi:::stri_extract_last(s, regex="[:alnum:]+$")   
    } 
   f <- getLastWord 
  if(class(s)=="character"){
    out <- f(s) 
  }
  if("data.frame" %in% class(s)){ 
    df <- tbl_df(s)
    #cols <- c("fuente","implicado")
    colNums <- match(cols,names(df)) %||% 1
    out <- df %>%
            select(colNums) %>%
            rowwise() %>%
            mutate_each(funs(f))
  }
  out
}


mopSplitFixedPattern <- function(s, pattern, splitLength = 2, cols=NULL){  
    stringSplitFun <- function(pattern, splitLength){      
      function(str){
        as.data.frame(stringr::str_split_fixed(str,pattern,splitLength))
      }
    } 
    f <- stringSplitFun(pattern = pattern, splitLength = splitLength)     
    #library(pryr)
    #unenclose(f)
    if(class(s)=="character"){
      out <- f(s) 
    }    
    if("data.frame" %in% class(s)){ 
      df <- tbl_df(s)
      colNums <- match(cols,names(df)) %||% 1      
      out <- f(df[,colNums])     
    }
    names(out) <- paste0("col",1:splitLength)
    out
}

mopWhiteSpace <- function(s, cols=NULL){
  trimWhite <- function(str){
    #str <- "  Caserío La Mesa-Veredas La Mesa, La Danta y Mulatos "
    stringr:::str_trim(str)  
  } 
  f <- trimWhite 
  if(class(s)=="character"){
    out <- f(s) 
  }
  if("data.frame" %in% class(s)){ 
    df <- tbl_df(s)
    #cols <- c("fuente","implicado")
    colNums <- match(cols,names(df)) %||% 1
    out <- df %>%
      select(colNums) %>%
      rowwise() %>%
      mutate_each(funs(f))
  }
  out  
}

mopStrChop <- function(s, start = 1, end = 2, cols=NULL){
  strChop <- function(start,end){
    function(str){
      #str <- "Caserío La Mesa-Veredas La Mesa, La Danta y Mulatos "
      substr(str, start = start, stop = end)  
    }
  }
  f <- strChop(start = start, end = end)
  if(class(s)=="character"){
    out <- f(s) 
  }
  if("data.frame" %in% class(s)){ 
    df <- tbl_df(s)
    #cols <- c("fuente","implicado")
    colNums <- match(cols,names(df)) %||% 1
    out <- df %>%
      select(colNums) %>%
      rowwise() %>%
      mutate_each(funs(f))
  }
  out  
} 

mopAccents <- function(s, cols=NULL){
  f <- removeAccents
  if(class(s)=="character"){
    out <- f(s) 
  }
  if("data.frame" %in% class(s)){ 
    df <- tbl_df(s)
    #cols <- c("fuente","implicado")
    colNums <- match(cols,names(df)) %||% 1
    out <- df %>%
      select(colNums) %>%
      rowwise() %>%
      mutate_each(funs(f))
  }
  out  
}

mopDictionaryMatch <- function(s, dict, cols=NULL){  
  dicMatch <- function(dict){
    function(str){
      #str <- "Caserío La Mesa-Veredas La Mesa, La Danta y Mulatos "
      dictionaryMatch(str,dict)
    }
  }
  f <- dicMatch(dict)

  if(class(s)=="character"){
    out <- f(s) 
  }
  if("data.frame" %in% class(s)){ 
    dd <- tbl_df(s)
    #cols <- c("fuente","implicado")
    colNums <- match(cols,names(dd)) %||% 1
    
    df <- tbl_df(s)
    colNums <- match(cols,names(df)) %||% 1      
    out <- f(df[,colNums])  
    
    out <- dd %>%
      select(colNums) %>%
      rowwise() %>%
      mutate_each(funs(f))
  }
  out  
}


  
 ## utils 
removeAccents <- function(string){
  accents <- "àèìòùÀÈÌÒÙáéíóúýÁÉÍÓÚÝñÑç"
  translation <- "aeiouAEIOUaeiouyAEIOUYnNc"
  chartr(accents, translation, string)
}  


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


