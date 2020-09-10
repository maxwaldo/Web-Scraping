############# Scraping data from parliament ##############
library(rvest)
library(dplyr)

##### 1. Scrape the text ############

#### Database to store data

Data_frame <- as.data.frame(list(Number = NA,
                            Title = NA,
                            Description = NA))

### Remove the row of the data
Data_frame <- Data_frame[-1,]

## Defines the number of page
Num_page <- 47

for (i in 1:Num_page) {
  
  ## Get the links to votes in each of the 47 pages
  webpage <-read_html(paste0("http://ws-old.parlament.ch/affairsummaries?pageNumber=", i)) %>% 
    html_nodes("table") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  
  ## Foreach of this links, get the text for the description
  for (j in 1:length(webpage)) {
    description <- read_html(paste0("http://ws-old.parlament.ch", webpage[j])) %>% 
      html_nodes("dd")
    summary <- description[4] %>% html_text() 
    
    ## Remove html attributes from the summary of the project
    summary <- gsub("\r\n            ", "", summary)
    summary <- gsub("\r\n        ", "", summary)
    summary <- gsub("<p>", "", summary)
    summary <- gsub("</p>", "", summary)
    summary <- gsub("<b>", "", summary)
    summary <- gsub("</b>", "", summary)
    
    ## Same exercise for the title
    title <- description[3] %>% html_text()
    title <- gsub("\r\n            ", "", title)
    title <- gsub("\r\n        ", "", title)
    title <- gsub("<p>", "", title)
    title <- gsub("</p>", "", title)
    title <- gsub("<b>", "", title)
    title <- gsub("</b>", "", title)
    
    ## Store the number of the vote
    Number <- description[1] %>% html_text()
    Number <- gsub("\r\n            ", "", Number)
    Number <- gsub("\r\n        ", "", Number)
    
    ## Temporary dataframe to store the information
    DF <- as.data.frame(list(Number = Number,
                             Title = title,
                             Description = summary))
    
    ## Store this in the main database
    Data_frame <- rbind(Data_frame, DF)
    
    ## Print message wheen successfully reach the end of the loop. 
    print(paste("Get description for project", Number))
    
    closeAllConnections()
    
    
  }
  
}


write.csv(Data_frame, file = "Description from parliamentary project.csv")


##### 2. Scrape the votes ##########

# Defines the data for the concillor's votes on projects
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

urls <- NULL

## Loop to get the urls for each of pages
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

## For each of the url, we get the number of page for this observation
for (i in 1:length(urls)) {
  # Get the links of the page
  Number_page <- read_html(paste0("http://ws-old.parlament.ch", urls[i])) %>% 
    html_nodes("div") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  
  # Keeps the last link
  Number_page <- Number_page[length(Number_page)]
  # Get the page number
  Number_page <- gsub("?pageNumber=", "", Number_page)
  Number_page <- substr(Number_page,2, nchar(Number_page))
  Number_page <- as.numeric(as.character(Number_page))
  
  # Defines a dataframe with the links and the number of page related
  data_number_url <- as.data.frame(list(url = urls[i],
                                        Number = Number_page))
  data_url <- rbind(data_url, data_number_url)
  
  # Prints the message
  print(paste0("Number extraction for page ", i))
  
}

 
links <- NULL

## Get the links to all the vote for each concillor
for (i in 1:nrow(data_url)) {
  for (j in 1:data_url[i,]$Number){
    link <- paste0("http://ws-old.parlament.ch", data_url[i,]$url, "?pageNumber=", j)
    links <- c(links, link)
    print(paste0("get infos mp ", data_url[i,]$url, " for page ", j))
  }
    
}

save(links, file = "links for all votes.rda")


## Get all the concillor vote data
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


# For each link
for (i in 1:length(links)) {
  
  # We get some text
  concillor <- read_html(links[i]) %>% 
    html_nodes("table") %>% 
    html_nodes("td") %>% 
    html_text()
  
  # Keep information we are interested in
  concillor <- concillor[31:length(concillor)]
  
  # For each of these observation
  # Subset characters we do not want in our variables
  for (l in 1:length(concillor)) {
    concillor[l] <- gsub("\r\n                ", "", concillor[l])
    concillor[l] <- gsub("\r\n            ", "", concillor[l])
  }
  
  # Create an identifier to get all the different information
  items <- seq(1, length(concillor), 11)
  
  # Gets data we are interested in
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
  
  # Store this in a temporary datavase
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
  
  # Store it in the main database
  Concillor_votes_data <- rbind(Concillor_votes_data, vote_concillor)
  
  # Print message to keep track of the loop 
  print(paste0("get votes for the url ", i))
  
  closeAllConnections()
  
}


