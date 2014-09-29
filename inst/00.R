library(devtools)
#library(datapackager)
#library(reshape2)
#library(lubridate)

load_all()
document()





###   Test Mopper with Dp's
#### TODO test column selection for runMopper
library(devtools)
load_all()
document()
path <- system.file("masacres.csv", package="mopper")
df <- read.csv(path, stringsAsFactors=FALSE)
df <- df[,c("fuente","implicado")]
dp <- newDatapkg(df)

opts <- list(pos = "last")
runMopper(dp, "mopWord", opts)

opts <- list(pattern = "-", splitLength = 3)
cleanId <- "mopSplitFixedPattern"
dpout <- runMopper(dp, cleanId, opts)




### Get last word
path <- system.file("masacres.csv", package="mopper")
df <- read.csv(path, stringsAsFactors=FALSE)

### Mop last word
s <- "Caserío La Mesa-Veredas- La Mesa, La D-anta y Mulatos"
s <- "Guerrilla-FARC u otro"
mopWord(s, pos="first")
mopWord(s, pos="last")
mopWord(df$implicado, pos="last")
mopWord(mopWhiteSpace(df$implicado), pos="last")
mopWord(df[,c("fuente","implicado")])
str(mopWord(df, cols = c("fuente")))
mopWord(df$fuente)



### Split fixed
load_all()
s <- "Caserío La Mesa-Veredas- La Mesa, La D-anta y Mulatos"
s <- "fdaf"
mopSplitFixedPattern(s, pattern="-", splitLength=3)
s <- df
tmp <- mopSplitFixedPattern(s, pattern="-", splitLength=3, cols=c("lugar"))
str(tmp)
tmp <- mopSplitFixedPattern(s, pattern="-", splitLength=3, cols=c("lugar","implicado"))
str(tmp)

### Trim white
s <- "    CaseríoMulatos "
mopWhiteSpace(s)
df <- data.frame(a = rep(s,10), b=rep(s,10))
mopWhiteSpace(df, cols=c('b','a'))


### Chop string
s <- "CCaseríoMulatos "
mopStrChop(s, start=3, end=40)
df <- data.frame(a = rep(s,10), b=rep(s,10))
mopStrChop(df, start=3, end= 7,cols=c('b','a'))

### Mop accents
s <- "CCaseríoMulatosáá''d "
mopAccents(s)
df <- data.frame(a = rep(s,10), b=rep(s,10))
mopAccents(df,cols=c('b','b'))

### dictionaryMatch
mopDictionaryMatch("uss.a",c("USA","united"))
df <- data.frame(a1 = sample(c("us.a","unitted"),20, replace=TRUE))
df$match <- mopDictionaryMatch(df,dict = c("USA","united"))
df

### Date parser
s <- "21-Sep-2014"
mopDates(s, from="dmy3")
df <- data.frame(a = sample(c("21-Sep-2014","20-Aug-2011"),20, replace=TRUE))
str(mopDates(df, from="dmy3"))
str(mopDates(df, from="dmy3", to="mdy2"))






### locations form IP

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



