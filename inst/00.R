library(devtools)
#library(datapackager)
#library(reshape2)
#library(lubridate)

load_all()
document()



dateFormats <- list(
  dm = "21-Sep",
  md1 = "Sep 21",
  md2 = "Sep-21"
  md1 = "9/21",
  md2 = "09-21",
  dmy1 = "21/09/2014",
  dmy2 = "21-09-2014",
  mdy1 = "09/21/2014",
  mdy2 = "9/21/14",
  mdy3 = "09-21-14",
  mdy4 = "September 21, 2014",
  dmy3 = "21-Sep-2014",
  ymd1 = "2014-09-21"  
  )



### Get las word

library(devtools)
library(dplyr)
library(tidyr)
load_all()

path <- system.file("masacres.csv", package="mopper")

df <- read.csv(path, stringsAsFactors=FALSE)

### Mop last word
mopLastWord(df, cols=c("fuente","implicado"))

### Split fixed
s <- "Caserío La Mesa-Veredas- La Mesa, La D-anta y Mulatos"
s <- "fdaf"
mopSplitFixed(s, pattern="-", splitLength=3)
df <- tbl_df(df)
s <- df
select(s, lugar)
tmp <- mopSplitFixed(s, pattern="-", splitLength=3, cols=c("lugar"))
tmp <- tbl_df(tmp)

### Trim white
s <- "    CaseríoMulatos "
mopWhiteSpace(s)
df <- data.frame(a = rep(s,10), b=rep(s,10))
mopWhiteSpace(df, cols=c('b','a'))


### Chop string
s <- "CCaseríoMulatos "
mopStrChop(s, start=3, end=40)
df <- data.frame(a = rep(s,10), b=rep(s,10))
mopStrChop(df, start=3, end= 7,cols=c('b'))

### Mop accents
s <- "CCaseríoMulatosáá''d "
mopAccents(s)
df <- data.frame(a = rep(s,10), b=rep(s,10))
mopAccents(df,cols=c('b'))

### dictionaryMatch
mopDictionaryMatch("uss.a",c("USA","united"))
df <- data.frame(a = sample(c("usa","unitted"),20, replace=TRUE))
df$match <- mopDictionaryMatch(df,dict = c("US.A","united"))
df



### locations form IP




##





## IP

library(httr)
#V1
#http://heuristically.wordpress.com/2013/05/20/geolocate-ip-addresses-in-r/
#https://freegeoip.net/json/

#V2
ip <- '189.122.89.181'
# http://www.iptolatlng.com/
url <- "http://www.iptolatlng.com?ip=189.122.89.181&type=json"
res <- GET(url = url)

GET(url = url, query = c(ip = ip, type='json'))

  
  
  
  
  



