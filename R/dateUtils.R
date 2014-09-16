transformDate <- function(s, from = "dmy1", to = NULL){ 
  l <- lapply(s, function(s){
    dateFormats <- list(
      dmy1 = list(fromFun = dmy, toFormat = "%d/%m/%Y", sample = "21/09/2014"),
      dmy2 = list(fromFun = dmy, toFormat = "%d-%m-%Y", sample = "21-09-2014"),
      dmy3 = list(fromFun = dmy, toFormat = "%d-%b-%Y", sample = "21-Sep-2014"),
      mdy1 = list(fromFun = mdy, toFormat = "%m/%d/%Y", sample = "09/21/2014"),
      mdy2 = list(fromFun = mdy, toFormat = "%m/%d/%Y", sample = "9/21/14"),
      mdy3 = list(fromFun = mdy, toFormat = "%m/%d/%Y", sample = "09-21-14"),
      mdy4 = list(fromFun = mdy, toFormat = "%m/%d/%Y", sample = "September 21, 2014"),
      ymd1 = list(fromFun = ymd, toFormat = "%Y-%m-%d", sample = "2014-09-21") 
    )  
    fromFun <- dateFormats[[from]]$fromFun
    date <- fromFun(s)
    if(is.null(to)) {out <- date}
    else{
      toFormat <- dateFormats[[to]]$toFormat
      out <- do.call(format, list(date,toFormat))    
    }
    out
    #toFun <- dateFormatFuns[[to]]$toFun
  })
  do.call(c,l)
}

# s <-"21/09/2014" 
# s <- c("21-Sep-2014","20-Sep-2014")
# str(transformDate(s, from="dmy1"))
# str(transformDate(s, from="dmy1", to="mdy1"))
# transformDate(s, from="dmy1", to="mdy1")
# s <- "21-09-2014"
# transformDate(s, from="dmy2")
# transformDate(s, from="dmy2", to="mdy3")
# s <- "21-Sep-2014"
# transformDate(s, from="dmy3")
# transformDate(s, from="dmy3", to="mdy2")
# s <- "09/21/2014"
# transformDate(s, from="mdy1")
# transformDate(s, from="mdy1", to="mdy1")
# s <- "9/21/14"
# transformDate(s, from="mdy2")
# transformDate(s, from="mdy2", to="mdy1")
# s <- "09-21-14"
# transformDate(s, from="mdy3")
# transformDate(s, from="mdy3", to="dmy1")
# s <- "September 21, 2014"
# transformDate(s, from="mdy4")
# transformDate(s, from="mdy4", to="dmy2")
# s <- "2014-09-21" 
# transformDate(s, from="ymd1")
# transformDate(s, from="ymd1", to="dmy3")
