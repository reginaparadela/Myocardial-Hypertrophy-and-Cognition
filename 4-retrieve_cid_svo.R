setwd("/Users/rparadela/Desktop/Análises R/Pós-doc/Biobanco/Myocardial hypertrophy and cognition - IC Heberti")

# Loading packages ####
library(tidyverse)

# Read in the data ####

df_lvh <- read.csv("df_lvh_clean.csv")

glimpse(df_lvh)

# Merge with dataset with cause of death #####

depre_atero <- read.csv("depre_atero_clean.csv") %>% select(nsvo, causa)#banco com a causa da morte 

data <- merge(df_lvh, depre_atero, by.x = 'nsvo', all.x = T)

table(duplicated(data))

table(is.na(data$causa))#169 NA

#Separar os casos p/ recuperar 

data_subset <- data %>% dplyr::filter(is.na(data$causa)) %>% 
  select(nsvo, causa) %>% 
  separate(nsvo, c('numsvoc', 'ano'), 
           remove = FALSE, fill = 'left') %>% #'remove = F' mantem a variavel original; fill determina a posicao dela
  select(nsvo, ano)


##Retrieve SVO files#####

svoFiles <- new.env()  # um dicion?rio ou hashmap. acho mais f?cil lidar com esse tipo de estrutura de dados

for (f in list.files(path = "/Users/rparadela/Desktop/Análises R/Pós-doc/Biobanco/SVO_files/SVO", full.names = TRUE)) {   # Aqui voc? pode colocar o caminho para encontrar os seus arquivos, no meu caso est?o dentro desta pasta
  key <- gsub(pattern = '[^0-9]', replacement = "", x = f)
  svoFiles[[key]] <- f
}

redcapSample <- data_subset

grouppedByYear <- redcapSample %>% mutate(nsvo = as.character(nsvo)) %>% group_by(ano) %>% summarise(nsvos = list(nsvo))

grouppedByYear

getSVOData <- function(row) {
  key <- as.character(row[1]) #Key ? o ano 
  key <- sprintf("20%s", key) #essa fun??o faz algo parecido com o paste. Ela junto o 20 ao 05 sem deixar espa?o (antes dava erro pq tava procurando por 05)
  message(paste("Finding SVO year file: ", key))
  toRetrieve <- as.data.frame(matrix(unlist(row[2]))) %>% rename(nsvo = V1) %>% mutate(nsvo = as.character(nsvo))
  fileSvo <- svoFiles[[key]]
  print(fileSvo)
  df <- read_csv2(file = fileSvo, col_select = c("numsvoc", "cid1", "ano")) %>%
    mutate(
      nsvo = paste(numsvoc, str_sub(ano, -2, -1), sep = "/"),
      cid = gsub(x = cid1, pattern = '[^A-TV-Z$]', replacement = "")
    ) 
  
  df <- df %>% inner_join(toRetrieve, by = "nsvo")
  message(paste("I was looking for", nrow(toRetrieve), "rows.\nI've found ", nrow(df), " rows!"))
  return(df)
}

foundNsvo <- data.frame()

for (i in 1:nrow(grouppedByYear)) {
  row <- grouppedByYear[i,]
  foundNsvo <- foundNsvo %>% bind_rows(getSVOData(row))
}

#merge do foundNsvo com o df_lvh

foundNsvo <-  foundNsvo %>% rename(causa = cid)

data <- merge(data, foundNsvo, by = 'nsvo', all.x = T)

data$causa <- coalesce(data$causa.x, data$causa.y)

table(is.na(data$causa))

write.table(data, "df_lvh_clean_cid.csv", sep = ",")


