### LegiScan Data Pull and Search
#     Script to load LegInfo data and search it based on different criteria
#
#     By Patrick Rogers, California Research Bureau
#       Apr 2025
#
#     Uses the following Packages
#       {here}
#       {httr}
#       {xml2}
#       {base64enc}
#       {jsonlite}
#       {pdftools}
#
#     Uses the following data
#       

# To Do
#   Add separate code block to search for passed legislation by legislator
#   Extract date filed with SOS and add to additional column
#   Get LegiScan Data uploaded into MySQL
#   Add Option to use LegInfo appropriation flag instead of SBUD list
#   Status appears to  be "Intro, Engross, Enroll, Pass, Veto" and Chaptered?
#       After session is over, anything 1-4 is dead, 5 is vetoed and 6 is chaptered(?)

# Clear the workspace
remove(list=ls(all=TRUE))

# Load Packages
library(here)
library(uuid)

#library(httr)
#library(xml2)
#library(base64enc)
library(jsonlite)
library(pdftools)
library(RSQLite)

# Inits
setwd(here::here())

force_download_legiscan <- FALSE
force_download_sbud <- FALSE

chaptered_only <- FALSE

sessions <- c(2025, 2023, 2021) # Sessions start on odd year

# house housing rent tenant tenancy landlord 
#search_terms <-  c('housing', 'home', 'house', 'rent', 'tenant', 'tenancy')
search_terms <-  c('affordab', 'cost', 'saving', 'competiti', 'fee', 'charge', 'burden', 'deposit', 'subscription', 'cancellation', 'credit', 'consumer', 'debt', 'price', 'discount')
save_name <- 'general_affordability'

# Load Sourcefiles
#source(file = here::here('private', 'legiscan_api_key.R'))

source(file = here::here('src', 'legiscan_data_pull.R'))

# User Parameters

# Custom Functions ####

# Get LegiScan Bill Data ####
legiscan_dbs2 <- get_legiscan_data(force_download = force_download_legiscan)

# Iterate over each session ####
results <- NULL

for(session in sessions){
  bill_directory <- here::here('data',
                               paste0(session, '-', session+1, ' Regular Session'), 'CA',
                               paste0(session, '-', session+1, '_Regular_Session'), 'bill')
  legiscan_bills <- vector(mode = 'list', length = length(list.files(bill_directory))) # Empty vector
  names(legiscan_bills) <- gsub(".json", "",  list.files(bill_directory))
  for(i in seq_along(legiscan_bills)){
    # Get JSON
    current_bill <- jsonlite::fromJSON(txt = here::here(bill_directory, list.files(bill_directory)[i]))$bill
    
    # Parse and keep what we need
    legiscan_bills[[i]][["bill_number"]] <- current_bill[["bill_number"]]
    legiscan_bills[[i]][["session"]] <- current_bill[["session"]][["session_name"]]
    legiscan_bills[[i]][["status"]] <- current_bill[["status"]]
    legiscan_bills[[i]][["link"]] <- current_bill[["texts"]][["state_link"]][1]
    legiscan_bills[[i]][["title"]] <- current_bill[["title"]]
    legiscan_bills[[i]][["description"]] <- current_bill[["description"]]
  }
  match_matrix <- NULL
  for(search_term in search_terms){
    match_matrix <- cbind(match_matrix, unlist(lapply(legiscan_bills,  \(x) grepl(search_term, x[["title"]]))))
    match_matrix <- cbind(match_matrix, unlist(lapply(legiscan_bills,  \(x) grepl(search_term, x[["description"]]))))
  }
  results <- c(results,  legiscan_bills[which(rowSums(match_matrix) > 0)])
  
}

results_collapsed <- do.call(rbind.data.frame, results)
results_collapsed$link <- gsub("#.+$", '', results_collapsed$link)
write.csv(results_collapsed, file = here::here('output', paste0(uuid::UUIDgenerate(), '_', save_name, '.csv.')))
# EOF ####


