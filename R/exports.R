
#' mopWord
#' @name mopWord
#' @description mopWord
#' @param s string or dataframe
#' @param pos string, "first" or "last"
#' @return string
#' @export
#' @examples \dontrun{
#' }
mopWord <- function(s, pos ="first",cols=NULL){
    getLastWord <- function(pos){
      #str <- "Caserío La Mesa-Veredas La Mesa, La Danta y Mulatos"
      #stringi:::stri_extract_last(str, regex="\\w+")
      ##stringi:::stri_extract_last(s, regex="[:alnum:]+$")   
      #str_extract(str, '\\w+$')
      function(str){
        
        if(pos=="first"){ 
          out <- word(str,1)
        }
        else if(pos=="last"){
          out <- word(str,-1)
        }

#         else if(!pos %in% c("first","last") ){
#           stop("Specify pos='first' or pos='last'")
#         }
        out
      }
    } 
   f <- getLastWord(pos = pos)
  if(class(s)=="character"){
    out <- f(s) 
  }
  if("data.frame" %in% class(s)){ 
    df <- tbl_df(s)
    #cols <- c("fuente","implicado")
    colNums <- match(cols,names(df)) %||% 1:ncol(df)
    out <- df %>%
            select(colNums) %>%
            rowwise() %>%
            mutate_each(funs(f))
  }
  out
}

#' mopSplitFixedPattern
#' @name mopSplitFixedPattern
#' @description mopSplitFixedPattern
#' @param string
#' @return string
#' @export
#' @examples \dontrun{
#' }
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
      colNums <- match(cols,names(df)) %||% 1:ncol(df)      
      out <- f(df[,colNums])     
    }
    names(out) <- paste0("col",1:splitLength)
    out
}

#' mopWhiteSpace
#' @name mopWhiteSpace
#' @description mopWhiteSpace
#' @param string
#' @return string
#' @export
#' @examples \dontrun{
#' }
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
    colNums <- match(cols,names(df)) %||% 1:ncol(df)
    out <- df %>%
      select(colNums) %>%
      rowwise() %>%
      mutate_each(funs(f))
  }
  out  
}

#' mopStrChop
#' @name mopStrChop
#' @description mopStrChop
#' @param string
#' @return string
#' @export
#' @examples \dontrun{
#' }
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
    colNums <- match(cols,names(df)) %||% 1:ncol(df)
    out <- df %>%
      select(colNums) %>%
      rowwise() %>%
      mutate_each(funs(f))
  }
  out  
} 

#' mopAccents
#' @name mopAccents
#' @description mopAccents
#' @param string
#' @return string
#' @export
#' @examples \dontrun{
#' }
mopAccents <- function(s, cols=NULL){
  f <- removeAccents
  if(class(s)=="character"){
    out <- f(s) 
  }
  if("data.frame" %in% class(s)){ 
    df <- tbl_df(s)
    #cols <- c("fuente","implicado")
    colNums <- match(cols,names(df)) %||% 1:ncol(df)
    out <- df %>%
      select(colNums) %>%
      rowwise() %>%
      mutate_each(funs(f))
  }
  out  
}

#' mopDictionaryMatch
#' @name mopDictionaryMatch
#' @description mopDictionaryMatch
#' @param string
#' @return string
#' @export
#' @examples \dontrun{
#' }
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
    colNums <- match(cols,names(dd)) %||% 1:ncol(df)
    
    out <- dd %>%
      select(colNums) %>%
      rowwise() %>%
      mutate_each(funs(f))
  }
  out  
}

#' mopDates
#' @name mopDates
#' @description mopDates
#' @param string
#' @return string
#' @export
#' @examples \dontrun{
#' }
mopDates <- function(s, from, to = NULL, cols=NULL){  
  transDate <- function(from, to){
    function(str){
      transformDate(str,from, to)
    }
  }
  f <- transDate(from = from, to = to)
  #unenclose(f)
  if(class(s)=="character"){
    out <- f(s) 
  }
  if("data.frame" %in% class(s)){ 
    dd <- tbl_df(s)
    #cols <- c("fuente","implicado")
    colNums <- match(cols,names(dd)) %||% 1:ncol(df)

    out <- dd %>%
      select(colNums) %>%
      rowwise() %>%
      mutate_each(funs(f))

  }
  out  
}

  
