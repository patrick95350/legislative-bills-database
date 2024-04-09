### ASM Rodriguez Budget Wins
#     Script to load LegInfo data and search it form Freddie Rodriguez budget wins
#
#     By Patrick Rogers, California Research Bureau
#       April 2024
#
#     Uses the following Packages
#       {here}
#       {httr}
#       {base64enc}
#
#     Uses the following data
#       

# To Do
#   1. Get LegiScan Data uploaded into MySQL

# Clear the workspace
remove(list=ls(all=TRUE))

# Load Packages
library(here)

library(httr)
library(base64enc)
library(jsonlite)

# Inits
setwd(here::here())

# Load Sourcefiles
source(file = here::here('private', 'legiscan_api_key.R'))

# User Parameters

# Custom Functions ####

# Get LegiScan Bill Data ####
# List of  Databases
path <- here::here('data', 'legiscan_dbs_list.RData')
if(file.exists(path)){
  message('List of Legiscan databases already exists, loading from local file.')
  legiscan_dbs_list <- readRDS(path)
} else {
  request <- httr::GET('https://api.legiscan.com/',
                       query = list(key = '2ef76bb3922d1a826f444e5f7d1fc36e',
                                    op = 'getDataSetList',
                                    state = 'CA'))
  legiscan_dbs_list <- content(request)$datasetlist
  saveRDS(legiscan_dbs_list, path)
}

# Individual Database
legiscan_dbs <- vector(mode = 'list', length = length(legiscan_dbs_list))
progress_bar <- txtProgressBar(min=1,max=length(legiscan_dbs), style=3)

for(i in seq_along(legiscan_dbs)){
  # First check if we've already downloaded this dataset
  path <- here::here('data', legiscan_dbs_list[[i]]$session_title)
  if(dir.exists(file.path(path, 'CA'))){
    warning('Already a directory at ', path, '. Check if data actually needs to be downloaded again.')
    next
  }

  # Make API call to Legiscan for single year of bills
  request <- httr::GET('https://api.legiscan.com/',
                       query = list(key = '2ef76bb3922d1a826f444e5f7d1fc36e',
                                    op = 'getDataSet',
                                    access_key = legiscan_dbs_list[[i]]$access_key,
                                    id = legiscan_dbs_list[[i]]$session_id))
  
  if(request$status_code == 200){
    # If successful:
    # Create a temporary zip file
    temp <- tempfile('legiscan_db', fileext = '.zip')
    # Decode the zip file portion from Base64 to binary and save to temp file
    if(!dir.exists(path)){dir.create(path)}
    writeBin(base64decode(content(request)$dataset$zip), temp)
    unzip(temp, list=FALSE, exdir = path)
    
    } else {
      legiscan_dbs[[i]] <- NULL
      warning('Request to ', request$url, ' returned status code ', request$status_code, '.')
    }
  
  setTxtProgressBar(progress_bar, i)
}

# Get LegiScan Person Data ####
# Sessions are returned with most recent first, so for the current session just index to [[1]]
request <- httr::GET('https://api.legiscan.com/',
                     query = list(key = '2ef76bb3922d1a826f444e5f7d1fc36e',
                                  op = 'getSessionPeople',
                                  id = legiscan_dbs_list[[1]]$session_id))
legiscan_person_list <- do.call(rbind.data.frame, content(request)$sessionpeople$people)


# Step 2 ####
bill_test <- scan(here::here('data', '2009-2010 Regular Session', 'CA', '2009-2010_Regular_Session', 'bill', 'AB1.json'))
bill_test <- jsonlite::fromJSON(here::here('data', '2009-2010 Regular Session', 'CA', '2009-2010_Regular_Session', 'bill', 'AB1.json'))

# Step 3 ####