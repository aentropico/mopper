## dplyr-intro

# http://rpubs.com/justmarkham/dplyr-tutorial

library(devtools)
load_all()

path <- system.file("masacres.csv", package="mopper")

df <- read.csv(path, stringsAsFactors=FALSE)

masacres <- tbl_df(df)
masacres

# masacres en antioquia entre 1980 y 1989
filter(masacres, departamento == "Antioquia", ano %in% c(1980:1989))

select(masacres, ano, departamento, n_victimas)
# note: `starts_with`, `ends_with`, and `matches` (for regular expressions) can also be used to match columns by name
# you can also select by matching column names with contains()
select(masacres, contains("depa"))
# or put ranges 
select(masacres, dia:ano)


filter(select(masacres, ano, departamento, n_victimas), n_victimas >10 )

#chaining
masacres %>%
  select(ano, departamento, n_victimas) %>%
  filter(n_victimas > 10)

masacres %>%
  select(departamento, n_victimas) %>%
  arrange(desc(departamento))
  
masacres %>%
  select(dia:ano,n_victimas) %>%
  mutate(date = paste(ano,mes,dia,sep="-"))
  
masacres %>%
  mutate(date = paste(ano,mes,dia,sep="-")) %>%
  select(date, departamento, n_victimas) %>%
  arrange(desc(n_victimas))

masacres %>%
  mutate(date = paste(ano,mes,dia,sep="-")) %>%
  select(date, departamento, n_victimas) %>%
  group_by(departamento) %>%
  summarise(promedioDepto = max(n_victimas, na.rm=TRUE)) %>%
  arrange(desc(promedioDepto)) 

masacres %>%
  mutate(date = paste(ano,mes,dia,sep="-")) %>%
  select(date, departamento, n_victimas) %>%
  group_by(departamento) %>%
  summarise(totalDepto = sum(n_victimas, na.rm=TRUE)) %>%
  arrange(desc(totalDepto))

masacres %>%
  mutate(date = paste(ano,mes,dia,sep="-")) %>%
  select(date, departamento, n_victimas) %>%
  group_by(departamento) %>%
  summarise(n_masacres = n()) %>%
  arrange(desc(n_masacres)) 

masacres %>%
  mutate(date = paste(ano,mes,dia,sep="-")) %>%
  select(date, departamento, n_victimas) %>%
  group_by(departamento) %>%
  summarise_each(funs(min(., na.rm=TRUE), max(., na.rm=TRUE), n()), n_victimas) %>%
  arrange(departamento)

masacres %>%
  select(ano, departamento, n_victimas) %>%
  group_by(departamento) %>%
  tally(sort = TRUE) # wrapper for summarise count and sort

masacres %>%
  select(ano, departamento, n_victimas) %>%
  group_by(departamento) %>%
  summarise(count = n(), distinctNVictims = n_distinct(n_victimas))

# for each implicado which 2 days of the year they had their largest masacres
masacres %>%
  group_by(implicado) %>%
  select(dia:ano, n_victimas) %>%
  filter(min_rank(desc(n_victimas)) <= 2) %>%
  arrange(implicado, desc(n_victimas))

masacres %>%
  group_by(implicado) %>%
  select(dia:ano, n_victimas) %>%
  top_n(2) %>%
  arrange(implicado, desc(n_victimas))

# for each year calculate the number of victims and the change from the previous year
masacres %>%
  group_by(ano) %>%
  summarise(totalVictimas = sum(n_victimas)) %>%
  mutate(change = totalVictimas - lag(totalVictimas))
# same as before but simpler with tally
masacres %>%
  group_by(ano) %>%
  tally(n_victimas) %>%
  mutate(change = n - lag(n))

# Random sample
masacres %>% sample_n(5) #wihtout replacement

masacres %>% sample_frac(0.005, replace=TRUE) #sample fraction

str(masacres)
glimpse(masacres)


