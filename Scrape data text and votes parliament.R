############# Scraping data from parliament ##############
library(rvest)
library(dplyr)

##### 1. Scrape the text ############

#### Number of page

Data_frame <- as.data.frame(list(Number = NA,
                            Title = NA,
                            Description = NA))


Data_frame <- Data_frame[-1,]


Num_page <- 47

for (i in 1:Num_page) {
  webpage <-read_html(paste0("http://ws-old.parlament.ch/affairsummaries?pageNumber=", i)) %>% 
    html_nodes("table") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  for (j in 1:length(webpage)) {
    description <- read_html(paste0("http://ws-old.parlament.ch", webpage[j])) %>% 
      html_nodes("dd")
    summary <- description[4] %>% html_text() 
    summary <- gsub("\r\n            ", "", summary)
    summary <- gsub("\r\n        ", "", summary)
    summary <- gsub("<p>", "", summary)
    summary <- gsub("</p>", "", summary)
    summary <- gsub("<b>", "", summary)
    summary <- gsub("</b>", "", summary)
    
    
    title <- description[3] %>% html_text()
    title <- gsub("\r\n            ", "", title)
    title <- gsub("\r\n        ", "", title)
    title <- gsub("<p>", "", title)
    title <- gsub("</p>", "", title)
    title <- gsub("<b>", "", title)
    title <- gsub("</b>", "", title)
    
    Number <- description[1] %>% html_text()
    Number <- gsub("\r\n            ", "", Number)
    Number <- gsub("\r\n        ", "", Number)
    
    DF <- as.data.frame(list(Number = Number,
                             Title = title,
                             Description = summary))
    
    Data_frame <- rbind(Data_frame, DF)
    
    print(paste("Get description for project", Number))
    
    closeAllConnections()
    
    
  }
  
}


write.csv(Data_frame, file = "Description from parliamentary project.csv")


##### 2. Scrape the votes ##########

Concillor_votes_data <- as.data.frame(list(Concillor_num=NA,
                                           Affair_id = NA,
                                           Affair_title = NA,
                                           Submission_text = NA,
                                           Division_text = NA, 
                                           Meaning_yes = NA,
                                           Meaning_no = NA,
                                           Concillor_vote_id = NA,
                                           Concillor_vote = NA,
                                           Vote_date = NA, 
                                           Update = NA))


Concillor_votes_data <- Concillor_votes_data[-1,]


closeAllConnections()

urls <- NULL



url <- read_html(paste0("http://ws-old.parlament.ch/votes/councillors?pageNumber=", 2)) %>% 
  html_nodes("table") %>% 
  html_nodes("a") %>% 
  html_attr("href")

url <- url[11:length(url)]
url

for (i in 1:11) {
  url <- read_html(paste0("http://ws-old.parlament.ch/votes/councillors?pageNumber=", i)) %>% 
    html_nodes("table") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  url <- url[11:length(url)]
  
  urls <- c(urls, url)
  
  print(paste0("Url for page ", i))
}

closeAllConnections()


data_url <- as.data.frame(list(url = NA,
                               Number = NA))

data_url <- data_url[-1,]


for (i in 1:length(urls)) {
  Number_page <- read_html(paste0("http://ws-old.parlament.ch", urls[i])) %>% 
    html_nodes("div") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  Number_page <- Number_page[length(Number_page)]
  
  Number_page <- gsub("?pageNumber=", "", Number_page)
  Number_page <- substr(Number_page,2, nchar(Number_page))
  
  Number_page <- as.numeric(as.character(Number_page))
  
  data_number_url <- as.data.frame(list(url = urls[i],
                                        Number = Number_page))
  
  data_url <- rbind(data_url, data_number_url)
  
  print(paste0("Number extraction for page ", i))
  
}


which(is.na(data_url$Number))

data_url[112, 2] <- 1
  
links <- NULL

for (i in 1:nrow(data_url)) {
  for (j in 1:data_url[i,]$Number){
    link <- paste0("http://ws-old.parlament.ch", data_url[i,]$url, "?pageNumber=", j)
    links <- c(links, link)
    print(paste0("get infos mp ", data_url[i,]$url, " for page ", j))
  }
    
}

save(links, file = "links for all votes.rda")

Concillor_votes_data <- as.data.frame(list(Concillor_num=NA,
                                           Affair_id = NA,
                                           Affair_title = NA,
                                           Submission_text = NA,
                                           Division_text = NA, 
                                           Meaning_yes = NA,
                                           Meaning_no = NA,
                                           Concillor_vote_id = NA,
                                           Concillor_vote = NA,
                                           Vote_date = NA, 
                                           Update = NA))


for (i in 14858:length(links)) {
  
  concillor <- read_html(links[i]) %>% 
    html_nodes("table") %>% 
    html_nodes("td") %>% 
    html_text()
  
  concillor <- concillor[31:length(concillor)]
  
  for (l in 1:length(concillor)) {
    concillor[l] <- gsub("\r\n                ", "", concillor[l])
    concillor[l] <- gsub("\r\n            ", "", concillor[l])
  }
  
  items <- seq(1, length(concillor), 11)
  
  concillor_num <- concillor[items]
  
  Affair_id <- concillor[1+items]
  
  Affair_title <- concillor[2+items]
  
  submission_text <- concillor[3+items]
  
  Division_text <- concillor[4+items]
  
  Meaning_yes <- concillor[5+items]
  
  Meaning_no <- concillor[6+items]
  
  concillor_vote_id <- concillor[7+items]
  
  concillor_vote <- concillor[8+items]
  
  Vote_date <- concillor[9+items]
  
  Update <- concillor[10+items]
  
  vote_concillor <- as.data.frame(list(Concillor_num=concillor_num,
                                       Affair_id = Affair_id,
                                       Affair_title = Affair_title,
                                       Submission_text = submission_text,
                                       Division_text = Division_text, 
                                       Meaning_yes = Meaning_yes,
                                       Meaning_no = Meaning_no,
                                       Concillor_vote_id = concillor_vote_id,
                                       Concillor_vote = concillor_vote,
                                       Vote_date = Vote_date, 
                                       Update = Update))
  
  
  Concillor_votes_data <- rbind(Concillor_votes_data, vote_concillor)
  
  print(paste0("get votes for the url ", i))
  
  closeAllConnections()
  
}


#### 211 to 220, 512 to 550, 824 to 880 did not extisted





closeAllConnections()



concillor <- read_html(links[1]) %>% 
  html_nodes("table") %>% 
  html_nodes("td") %>% 
  html_text()

concillor <- concillor[31:length(concillor)]

for (l in 1:length(concillor)) {
  concillor[l] <- gsub("\r\n                ", "", concillor[l])
  concillor[l] <- gsub("\r\n            ", "", concillor[l])
}





for (j in 1:length(url)) {
    
    Number_page <- read_html(paste0("http://ws-old.parlament.ch", url[j])) %>% 
      html_nodes("div") %>% 
      html_nodes("a") %>% 
      html_attr("href")
    Number_page <- Number_page[length(Number_page)]
    
    Number_page <- gsub("?pageNumber=", "", Number_page)
    Number_page <- substr(Number_page,2, nchar(Number_page))
    
    Number_page <- as.numeric(as.character(Number_page))
    
    
    
    for (k in 1:Number_page) {
      concillor <- read_html(paste0("http://ws-old.parlament.ch", url[j], "?pageNumber=", k)) %>% 
        html_nodes("table") %>% 
        html_nodes("td") %>% 
        html_text()
      
      concillor <- concillor[31:length(concillor)]
      
      for (l in 1:length(concillor)) {
        concillor[i] <- gsub("\r\n                ", "", concillor[l])
        concillor[i] <- gsub("\r\n            ", "", concillor[l])
      }
      
      items <- seq(1, length(concillor), 11)
      
      concillor_num <- concillor[items]
      
      Affair_id <- concillor[1+items]
      
      Affair_title <- concillor[2+items]
      
      submission_text <- concillor[3+items]
      
      Division_text <- concillor[4+items]
      
      Meaning_yes <- concillor[5+items]
      
      Meaning_no <- concillor[6+items]
      
      concillor_vote_id <- concillor[7+items]
      
      concillor_vote <- concillor[8+items]
      
      Vote_date <- concillor[9+items]
      
      Update <- concillor[10+items]
      
      vote_concillor <- as.data.frame(list(Concillor_num=concillor_num,
                                           Affair_id = Affair_id,
                                           Affair_title = Affair_title,
                                           Submission_text = submission_text,
                                           Division_text = Division_text, 
                                           Meaning_yes = Meaning_yes,
                                           Meaning_no = Meaning_no,
                                           Concillor_vote_id = concillor_vote_id,
                                           Concillor_vote = concillor_vote,
                                           Vote_date = Vote_date, 
                                           Update = Update))
      
      
      Concillor_votes_data <- rbind(Concillor_votes_data, vote_concillor)
      
      print(paste0("get page ", k, "of vote for the url ", url[j]))
      
      closeAllConnections()
    }

  }
  
}






url <- read_html("http://ws-old.parlament.ch/votes/councillors?pageNumber=11") %>% 
  html_nodes("table") %>% 
  html_nodes("a") %>% 
  html_attr("href")
url <- url[11:length(url)]
url


Number_page <- read_html("http://ws-old.parlament.ch/votes/councillors/3044") %>% 
  html_nodes("div") %>% 
  html_nodes("a") %>% 
  html_attr("href")
Number_page <- Number_page[length(Number_page)]

Number_page <- gsub("?pageNumber=", "", Number_page)
Number_page <- substr(Number_page,2, nchar(Number_page))

Number_page <- as.numeric(as.character(Number_page))


concillor <- read_html("http://ws-old.parlament.ch/votes/councillors/3044?pageNumber=135") %>% 
  html_nodes("table") %>% 
  html_nodes("td") %>% 
  html_text()

concillor <- concillor[31:length(concillor)]

for (i in 1:length(concillor)) {
  concillor[i] <- gsub("\r\n                ", "", concillor[i])
  concillor[i] <- gsub("\r\n            ", "", concillor[i])
}

items <- seq(1, length(concillor), 11)

concillor_num <- concillor[items]

Affair_id <- concillor[1+items]

Affair_title <- concillor[2+items]

submission_text <- concillor[3+items]

Division_text <- concillor[4+items]

Meaning_yes <- concillor[5+items]

Meaning_no <- concillor[6+items]

concillor_vote_id <- concillor[7+items]

concillor_vote <- concillor[8+items]

Vote_date <- concillor[9+items]

Update <- concillor[10+items]

vote_concillor <- as.data.frame(list(Concillor_num=concillor_num,
                                     Affair_id = Affair_id,
                                     Affair_title = Affair_title,
                                     Submission_text = submission_text,
                                     Division_text = Division_text, 
                                     Meaning_yes = Meaning_yes,
                                     Meaning_no = Meaning_no,
                                     Concillor_vote_id = concillor_vote_id,
                                     Concillor_vote = concillor_vote,
                                     Vote_date = Vote_date, 
                                     Update = Update))







Affair_ID <- 


concillor

##### 3. Run LDA #########


###### 4. Run IRT ########"



