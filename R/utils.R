`%||%` <- function (x, y) 
{
  if (is.empty(x)) 
    return(y)
  else if (is.null(x) || is.na(x)) 
    return(y)
  else if (class(x) == "character" && nchar(x) == 0) 
    return(y)
  else x
}

is.empty <- function (x) 
{
  !as.logical(length(x))
}

toJSONArray <- function(obj, json = TRUE, nonames = TRUE){
  list2keyval <- function(l){
    keys = names(l)
    lapply(keys, function(key){
      list(key = key, values = l[[key]])
    })
  }
  obj2list <- function(df){
    l = plyr::alply(df, 1, as.list)
    if(nonames){ names(l) = NULL }
    return(l)
  }
  if (json){
    RJSONIO::toJSON(obj2list(obj))
  } else {
    obj2list(obj)
  }
}

write_file <- function (x, file) 
{
  dir.create(dirname(file), recursive = TRUE)
  writeLines(x, file)
}
