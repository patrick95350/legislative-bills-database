### LegiSlator bill counts
#     Script to load LegInfo data count number of bills passed for each legislator for each legislative year
#
#     By Patrick Rogers, California Research Bureau
#       Sept 2024
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
#   Remove committee listed as sponsors by checking for non-empty district  field

# Clear the workspace
remove(list=ls(all=TRUE))

# Load Packages
library(here)
library(jsonlite)
library(uuid)

#library(httr)
#library(xml2)
#library(base64enc)
#library(jsonlite)
#library(pdftools)
#library(RSQLite)

# Inits
setwd(here::here())
run_id <- uuid::UUIDgenerate()

# Load Sourcefiles
source(file = here::here('src', 'legiscan_data_pull.R'))

# User Parameters
force_download_legiscan <- FALSE
force_download_sbud <- FALSE
all_bills <- TRUE

# Custom Functions ####

# Get LegiScan Bill Data ####
legiscan_dbs2 <- get_legiscan_data(force_download = force_download_legiscan)

# Loop  over each session
session_folders <- list.files(here::here('data'), pattern = '^[0-9]{4}')
legislator_files <- vector(mode = 'list', length = length(session_folders))

# Get list of Legislators
for(i in seq_along(session_folders)){
  legislator_files[[i]] <- data.frame(session=session_folders[i], people=list.files(here::here('data', session_folders[i], 'CA', gsub(' ', '_', session_folders[i]), 'people')))
}

# Combine list of legislators by session into single list
legislator_files <- do.call('rbind', legislator_files)
# Sort newest session to top
legislator_files <- legislator_files[order(legislator_files$session, decreasing = TRUE),]
# Drop duplicated legislator records
legislator_files <- legislator_files[!duplicated(legislator_files$people),]

legislators <- vector(mode = 'list', length = nrow(legislator_files))

# Read in data from Legislator files
for(i in seq_along(legislator_files$people)){
  progress_bar <- txtProgressBar(min=1,max=length(legislator_files), style=3)
  path <- here::here('data', legislator_files$session[i], 'CA', gsub(' ', '_', legislator_files$session[i]), 'people', legislator_files$people[i])
  temp <- jsonlite::fromJSON(path)
  
  legislators[[i]] <- data.frame(id = temp$person$people_id,
                                 Chamber = temp$person$role,
                                 Name = temp$person$name,
                                 District =  temp$person$district,
                                 committee = temp$person$committee_id)
  
  
  setTxtProgressBar(progress_bar, i)
}

legislators <- do.call('rbind', legislators)

legislator_bills <- matrix(0, nrow = nrow(legislators), ncol = length(session_folders),
                           dimnames = list(rep(NULL,nrow(legislators)), session_folders))

# Loop over each session to read in bills and assign to legislator
bills <- vector(mode = 'list', length=length(session_folders))

bills_billdodd <- data.frame(session=NULL, biill=NULL)

for(i in seq_along(session_folders)){
  progress_bar <- txtProgressBar(min=1,max=length(session_folders), style=3)
  
  bill_files <- list.files(here::here('data', session_folders[i], 'CA', gsub(' ', '_', session_folders[i]), 'bill'))
  
  # Loop over each bill to get the sponsor
  for(j in seq_along(bill_files)){
    path <- here::here('data', session_folders[i], 'CA', gsub(' ', '_', session_folders[i]), 'bill', bill_files[j])
    bill <- jsonlite::fromJSON(path)
    
    # Check bill type
    if(bill$bill$bill_type == 'B' | all_bills){} else next
    
    # Passed bills have a status of 4
    if(bill$bill$status == 4){} else next
    
    # Match the people_id of the primary sponsor to legislators$id
    matched_id <- legislators$id %in% bill$bill$sponsors$people_id[bill$bill$sponsors$sponsor_type_id == 1]
    
    # Check for empty matches
    if(sum(matched_id) == 0){
      matched_id <- legislators$id %in% bill$bill$sponsors$people_id[1]
    }
    
    # Check if still empty
    if(sum(matched_id) > 0){} else next
    
    legislator_bills[matched_id,i] <- legislator_bills[matched_id,i] + 1
    
    # Special List for Bill Dodd
    if(16285 %in% legislators$id[matched_id]){
      bills_billdodd <- rbind(bills_billdodd,
                              data.frame(session = session_folders[i],
                                         bill = bill$bill$bill_number,
                                         date =  bill$bill$status_date,
                                         title = bill$bill$title,
                                         desc = bill$bill$description))
    }
  }
  setTxtProgressBar(progress_bar, i)
}

legislators <- cbind(legislators,
                     legislator_bills,
                     Total=rowSums(legislator_bills),
                     `Years in Data`=(rowSums(legislator_bills > 0)*2),
                     `Bills per Year`=(rowSums(legislator_bills) / (rowSums(legislator_bills > 0)*2)))

# Clean up Legiscan Labeling of Assembly
legislators$Chamber <- gsub('Rep', 'Asm', legislators$Chamber)
legislators$District <- gsub('HD-', 'AD-', legislators$District)

# Drop committees
legislators <- legislators[legislators$committee == 0,]

# Drop unused columns
legislators <- legislators[,!(colnames(legislators) %in% c('id', 'committee'))]

# Output ####
write.csv(legislators, file = here::here('output', paste0(run_id, '_all_bills_', all_bills, '_all_legislators.csv')), row.names = FALSE)
write.csv(bills_billdodd, file = here::here('output', paste0(run_id, '_all_bills_', all_bills, '_bill_dodd_bills.csv')), row.names = FALSE)

# EOF ####


