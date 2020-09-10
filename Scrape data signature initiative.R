####################################################################
########## Scraping signatures collections #########################
####################################################################
rm(list=ls())
gc()
library(rvest)
library(dplyr)
library(stringr)
library(pdftools)
####################################################################
###### 1. get the links from the page ##############################
####################################################################

## Get main url
url <- "https://www.bk.admin.ch/ch/f/pore/vi/vis_2_2_5_1.html"

## Get all the links for each project
links <- read_html(url) %>% 
  html_nodes("td") %>% 
  html_nodes("a") %>% 
  html_attr("href")

## Get text infos for each project
text <- read_html(url) %>% 
  html_nodes("td") %>% 
  html_text()


text <- text[-1]
text <- as.data.frame(text)

texts <- str_split_fixed(text$text, "\n", 3)
texts <- as.data.frame(texts)
texts$V3 <- str_replace(texts$V3, "                     ", "")

## Defines the state of the ballot
texts <- texts %>% 
  mutate(type = ifelse(substr(V3, 1, 2)=="Ab", "Aboutissement",
                       ifelse(substr(V3, 1, 2)=="Ex", "Examen prelim", 
                              ifelse(substr(V3, 1, 2)=="Me", "Message", 
                                     ifelse(substr(V3, 1, 2)=="Re", "Retrait", 
                                            ifelse(substr(V3, 1, 2)=="In", "Initiative retirée", 
                                                   ifelse(substr(V3, 1, 2)=="Ar", "Arrêté parlement", 
                                                          ifelse(substr(V3, 1, 2)=="Vo", "Votation", 
                                                                 ifelse(substr(V3, 1, 2)=="En", "Entrée", 
                                                                        ifelse(substr(V3, 1, 2)=="No", "No entry", 
                                                                               ifelse(substr(V3, 1, 2)=="Dé", "Declared invalid", NA)))))))))))

texts$V1 <- links

## Only keep message where signature collections are published in a PDF
texts <- texts[texts$type=="Aboutissement" |
                 texts$type=="Message" |
                 texts$type=="Arrêté parlement" |
                 texts$type=="Votation",]

texts$link_number <- as.numeric(substr(texts$V1, 4, nchar(texts$V1)-5))

## If older, no PDF published
texts <- texts[texts$link_number>=274,]

########## When type is Aboutissement #########


####### Get links for the PDF ######

# Defines two variable to fill with the next loop
texts$Links_PDF <- NA
texts$Name_vot <- NA


# Get the links for the PDF and names of the ballots
for (i in 1:86) {
  
  url <- paste0("https://www.bk.admin.ch/ch/f/pore/vi/", texts[i,]$V1)
  
  links <- read_html(url) %>% 
    html_nodes("td") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  
  if (i>=76) {
    links <- links[length(links)]
  } else {
    links <- links[length(links)-1]
  }
  
  date <- read_html(url) %>% 
    html_nodes("td") %>% 
    html_nodes("a") %>% 
    html_text()
  
  name <- read_html(url) %>% 
    html_nodes("h2.text-inline") %>% 
    html_text()
  if (length(name)==0) {
    texts[i,]$Name_vot <- ""
  } else if (length(name)>0) {
    texts[i,]$Name_vot <- name
  } 
  
  if (length(links)==0) {
    texts[i,]$Links_PDF <- ""
  } else if (length(links)>0) {
    texts[i,]$Links_PDF <- links
  } 
  
  print(paste("Get links pdf for ballot:", name))
  closeAllConnections()
  
}


data_PDF <- as.data.frame(list(Canton = NA,
                               Valid = NA,
                               Non_valid=NA,
                               project = NA,
                               number=NA,
                               date=NA))


# Gets the tables in the PDF that list the number fo signature by canton
for (i in 1:86) {
  url <- paste0("https://www.bk.admin.ch/ch/f/pore/vi/", texts[i,]$V1)
  name <- read_html(url) %>% 
    html_nodes("h2.text-inline") %>% 
    html_text()
  if (length(name)==0) {
    name <- ""
  }
  
  date <- read_html(url) %>% 
    html_nodes("td") %>% 
    html_text()
  
  if (length(date)>30) {
    date <- date[5]
    date <- trimws(date)
  } else {
    date <- ""
  }
  
  
  pdf <- texts[i,]$Links_PDF
  pdftext <- pdf_text(pdf)
  if (i ==28) {
    pdftext <- pdftext[3]
  } else {
    pdftext <- pdftext[2]
  }
  pdftext <- str_split_fixed(pdftext, "\n", 35)
  pdftext <- pdftext[-c(1:2)]
  pdftable <- as.data.frame(do.call('rbind', strsplit(pdftext, '\\s{2,}')))
  colnames(pdftable) <- c("Canton", "Valid", "Non_valid")
  pdftable$project <- name
  pdftable$number <- i
  pdftable$date <- date
  
  data_PDF <- rbind(data_PDF, pdftable)
  
  print(paste("Get PDF from:", pdf))
  
  closeAllConnections()
  
  
}


# Recode the cantons (Particularly Unterwald le-haut and le-bas)
data_PDF <- data_PDF %>% 
  mutate(canton_names = ifelse(substr(Canton, 1, 6)=="Zurich", "Zurich", 
                               ifelse(substr(Canton, 1, 5)=="Berne", "Berne",
                                      ifelse(substr(Canton, 1, 7)=="Lucerne", "Lucerne",
                                             ifelse(substr(Canton, 1, 3)=="Uri", "Uri",
                                                    ifelse(substr(Canton, 1, 6)=="Schwyz", "Schwyz",
                                                           ifelse(substr(Canton, 1, 6)=="Obwald" | substr(Canton, 1, 17)=="Unterwald-le-Haut", "Obwald",
                                                                  ifelse(substr(Canton, 1, 7)=="Nidwald" | substr(Canton, 1, 16)=="Unterwald-le-Bas", "Nidwald",
                                                                         ifelse(substr(Canton, 1, 6)=="Glaris", "Glaris",
                                                                                ifelse(substr(Canton, 1, 4)=="Zoug", "Zoug",
                                                                                       ifelse(substr(Canton, 1, 8)=="Fribourg", "Fribourg",
                                                                                              ifelse(substr(Canton, 1, 7)=="Soleure", "Soleure",
                                                                                                     ifelse(substr(Canton, 1, 10)=="Bâle-Ville", "Bâle-Ville",
                                                                                                            ifelse(substr(Canton, 1, 13)=="Bâle-Campagne", "Bâle-Campagne",
                                                                                                                   ifelse(substr(Canton, 1, 11)=="Schaffhouse", "Schaffhouse",
                                                                                                                          ifelse(substr(Canton, 1, 21)=="Appenzell Rhodes-Ext." | substr(Canton, 1, 18)=="Appenzell Rh.-Ext.", "Appenzell Rhodes-Ext.",
                                                                                                                                 ifelse(substr(Canton, 1, 21)=="Appenzell Rhodes-Int." | substr(Canton, 1, 18)=="Appenzell Rh.-Int.", "Appenzell Rhodes-Int.",
                                                                                                                                        ifelse(substr(Canton, 1, 10)=="Saint-Gall" | substr(Canton, 1, 8)=="St-Gall.", "Saint-Gall",
                                                                                                                                               ifelse(substr(Canton, 1, 7)=="Grisons", "Grisons",
                                                                                                                                                      ifelse(substr(Canton, 1, 7)=="Argovie", "Argovie",
                                                                                                                                                             ifelse(substr(Canton, 1, 9)=="Thurgovie", "Thurgovie",
                                                                                                                                                                    ifelse(substr(Canton, 1, 6)=="Tessin", "Tessin",
                                                                                                                                                                           ifelse(substr(Canton, 1, 4)=="Vaud", "Vaud",
                                                                                                                                                                                  ifelse(substr(Canton, 1, 6)=="Valais", "Valais",
                                                                                                                                                                                         ifelse(substr(Canton, 1, 9)=="Neuchâtel", "Neuchâtel",
                                                                                                                                                                                                ifelse(substr(Canton, 1, 6)=="Genève", "Genève",
                                                                                                                                                                                                       ifelse(substr(Canton, 1, 4)=="Jura", "Jura",NA)))))))))))))))))))))))))))



## Drop observations that were added for unknown reason. 
data_PDF2 <- data_PDF[is.na(data_PDF$canton_names)==F,]





write.csv(texts, file = "Links to the pdf with signature collection.csv")
write.csv(data_PDF2, "Data signature collection 1999-2020.csv")
