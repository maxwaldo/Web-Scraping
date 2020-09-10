##### Scrape data on Brasilian election
rm(list=ls())
gc()

install.packages("rvest")
library(rvest)
library(dplyr)
## These is the main relative path
relative_url <- "https://eleicoes.poder360.com.br"

# The page number goes after this for the main page
relativ_main1 <- "/?ano=2014&nome=&municipio=&page="

# End of the path for the main page
relative_main2 <- "&uf=&cargo=6&partido="

# Data in which we paste the data we extract
data_all_candidates <- as.data.frame(list(Nome_completo =NA,
                                          Data_de_naciemento = NA,
                                          Municipo_de_naciemento = NA,
                                          Nacionalidade = NA,
                                          Genero = NA,
                                          Estado_civil = NA,
                                          Grau_de_instrucao = NA,
                                          Ocupação_principal = NA, 
                                          Cargo_disputado = NA,
                                          Cidade_UF_da_candidatura = NA,
                                          Situacao_da_candidatura = NA,
                                          Nome_da_urna = NA,
                                          Nome_do_partido = NA,
                                          No_do_partido = NA,
                                          Coligacao = NA, 
                                          Situacao_eleitoral = NA,
                                          Votos_no_1_turno = NA,
                                          Votos_no_2_turno = NA,
                                          election_year = NA))


# Data in which we store row data to parse into the main data frame
data_cand <- data_all_candidates


data_all_candidates <- read.csv("Data bresil election all.csv")

data_all_candidates <- data_all_candidates[,-1]

# For each of the page
for(i in 397:397) { ## !!!! If the code breaks
                  ## Replace 1:397 by x:397 where x = to the number of the page printed by the last message
  
  # Take each links in the page
  data_links <- read_html(paste0(relative_url, relativ_main1, i, relative_main2), options = "RECOVER") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  
  ## !!!! If the code breaks
  # replace i == 1 by i == to the number of the page printed by the last message
  if (i == 397 ) {
    
    ## !!!! If the code breaks
    # Replace m = 1 by m = the number of the candidate printed in the laste message
    m = 10
    
  } else {
    
    m = 1
  }
  # For each link in the page
  for (j in m:(length(data_links)-4)) {
    
    # Take all the table in the page
    data_candidate <- read_html(paste0(relative_url, data_links[j]), options = "RECOVER") %>%  
      html_table()
    # Take all the button in the page (for the number of elections)
    data_elections <- read_html(paste0(relative_url, data_links[j]), options = "RECOVER") %>% 
      html_nodes("button") %>% 
      html_text()
    
    # This will help use navigate through the tables of the page
    n = 1
    
    # For each election
    for (k in 1:length(data_elections)) {
      
      # if the nth observation if not the correct one it should correct this
      while (data_candidate[[n]][1,1]!="Nome completo" | is.na(data_candidate[[n]][1,1])==TRUE) {
        
        n = n+1
        
      }
      
      # Store the demographics
      demographics <- data_candidate[[n]][,2]
      
      n = n+1
      
      # store the political infos
      politics <- data_candidate[[n]][,2]
      
      # Some case have more info so we delete the one we do not need
      if (length(politics)>7) {
        
        politics <- politics[-2]
        
      }
      
      n = n+1 
      
      # Sometimes vote results are missing. 
      if (length(data_candidate[[n]])>2) {
        
        vote_info <-  c(NA, NA)
             
      } else {
        
        # This stores the vote info
        vote_info <- data_candidate[[n]][,2]
     
      }
      
      # if there was no second rounf, we add a NA
      if (length(vote_info)<3) {
        
        vote_info <- c(vote_info, NA)
        
      }
      
      # This combine the three vectors we created and the year of the election if the temporary dataset
      data_cand[1,] <- c(demographics, politics, vote_info, substr(data_elections[k], nchar(data_elections[k])-3,  nchar(data_elections[k])))
        
      # Bind this to the main dataset
      data_all_candidates <- rbind(data_all_candidates, data_cand)
      
      # print the message to know at which i and j we are
      print(paste("get vote for page", i, "of candate", j, "for election", k, "."))
      
      
    }
    
    
    closeAllConnections()
  }
}


write.csv(data_all_candidates, "Data bresil election all.csv", fileEncoding = "UTF-8")
library(haven)
write_dta(data_all_candidates, path = "Data bresil election all.dta")
