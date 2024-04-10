### ASM Rodriguez Budget Wins
#     Script to load LegInfo data and search it form Freddie Rodriguez budget wins
#
#     By Patrick Rogers, California Research Bureau
#       April 2024
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
#   1. Get LegiScan Data uploaded into MySQL
#   2. Add parameter to force re-downloading from Legiscan

# Clear the workspace
remove(list=ls(all=TRUE))

# Load Packages
library(here)

library(httr)
library(xml2)
library(base64enc)
library(jsonlite)
library(pdftools)

# Inits
setwd(here::here())

force_download_legiscan <- FALSE
force_download_sbud <- FALSE

# Load Sourcefiles
source(file = here::here('private', 'legiscan_api_key.R'))
source(file = here::here('src', 'sbud_budget_bill_links.R'))

# User Parameters

# Custom Functions ####

# Get LegiScan Bill Data ####
# List of  Databases
path <- here::here('data', 'legiscan_dbs_list.RData')
if(file.exists(path) & !force_download_legiscan){
  message('List of Legiscan databases already downloaded, loading from local file.')
  legiscan_dbs_list <- readRDS(path)
} else {
  request <- httr::GET('https://api.legiscan.com/',
                       query = list(key = legiscan_api_key,
                                    op = 'getDataSetList',
                                    state = 'CA'))
  legiscan_dbs_list <- content(request)$datasetlist
  saveRDS(legiscan_dbs_list, path)
}

# Individual Database
if(file.exists(here::here('data', 'legiscan_dbs.RData')) & !force_download_legiscan){
  message('Legiscan databases already downloaded, loading from local file.')
  legiscan_dbs <- readRDS(here::here('data', 'legiscan_dbs.RData'))
} else {
  legiscan_dbs <- vector(mode = 'list', length = length(legiscan_dbs_list))
  progress_bar <- txtProgressBar(min=1,max=length(legiscan_dbs), style=3)
  
  for(i in seq_along(legiscan_dbs)){
    # First check if we've already downloaded this dataset
    path <- here::here('data', legiscan_dbs_list[[i]]$session_title)
    if(dir.exists(file.path(path, 'CA')) & !force_download_legiscan){
      warning('Already a directory at ', path, '. Check if data actually needs to be downloaded again.')
      next
    }
    
    # Make API call to Legiscan for single year of bills
    request <- httr::GET('https://api.legiscan.com/',
                         query = list(key = legiscan_api_key,
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
      
      legiscan_dbs[[i]] <- content(request)
      
    } else {
      legiscan_dbs[[i]] <- NULL
      warning('Request to ', request$url, ' returned status code ', request$status_code, '.')
    }
    
    setTxtProgressBar(progress_bar, i)
  }
  saveRDS(legiscan_dbs, here::here('data', 'legiscan_dbs.RData'))
}

# Get Budget Bills from Senate Budget Committee ####
sbud_budget_bills <- vector(mode = 'list', length = length(sbud_budget_bills_links))
progress_bar <- txtProgressBar(min=1,max=length(sbud_budget_bills), style=3)

for(i in seq_along(sbud_budget_bills)){
  # Create filename for PDF
  pdf_file <- paste0(sbud_budget_bills_links[[i]][1], '_Budget_Bills.pdf')
  # Download
  if(!file.exists(here::here('downloads', pdf_file)) & !force_download_sbud){
    download.file(sbud_budget_bills_links[[i]][2],
                  here::here('downloads', pdf_file),
                  quiet = TRUE,
                  mode = 'wb')
  }

  # Parse text from PDF
  sbud_budget_bills_temp <- pdf_text(here::here('downloads', pdf_file)) |>
    strsplit('\n') |>
    unlist() |>
    gsub('^ +', '', x=_)

  # Keep only lines with Bills
  sbud_budget_bills_temp <- sbud_budget_bills_temp[grep('^[AS][BC]A? [0-9]', sbud_budget_bills_temp)]
  # Only need bill number
  sbud_budget_bills_temp <- gsub('^([AS][BC]A? [0-9]+).+', '\\1', sbud_budget_bills_temp)
  
  sbud_budget_bills[[i]] <- sbud_budget_bills_temp
  names(sbud_budget_bills)[i] <- sbud_budget_bills_links[[i]][1]
  
  setTxtProgressBar(progress_bar, i)
}

# Create List of Budget Bills with Legiscan Data ####
legiscan_budget_bills <- vector(mode = 'list', length = length(sbud_budget_bills))
names(legiscan_budget_bills) <- names(sbud_budget_bills)
progress_bar <- txtProgressBar(min=1,max=length(legiscan_budget_bills), style=3)

for(i in seq_along(legiscan_budget_bills)){
  # Determine session
  if((as.numeric(names(legiscan_budget_bills)[i]) %% 2) == 1){
    # Sessions start on odd years
    path <- here::here('data',
                       paste0(as.numeric(names(legiscan_budget_bills)[i]), '-', 
                              as.numeric(names(legiscan_budget_bills)[i]) + 1, ' Regular Session'),
                       "CA",
                       paste0(as.numeric(names(legiscan_budget_bills)[i]), '-', 
                              as.numeric(names(legiscan_budget_bills)[i]) + 1, '_Regular_Session'))
  } else {
    # Sessions end on even years
    path <- here::here('data',
                       paste0(as.numeric(names(legiscan_budget_bills)[i]) - 1, '-', 
                              as.numeric(names(legiscan_budget_bills)[i]), ' Regular Session'),
                       "CA",
                       paste0(as.numeric(names(legiscan_budget_bills)[i]) - 1, '-', 
                              as.numeric(names(legiscan_budget_bills)[i]), '_Regular_Session'))
  }
  
  # Create temporary list to store JSON from each budget bill for the current year
  legiscan_budget_bills_temp <- vector(mode = 'list', length = length(sbud_budget_bills[[i]]))
  names(legiscan_budget_bills_temp) <- sbud_budget_bills[[i]]
  
  # Process
  for(j in seq_along(legiscan_budget_bills_temp)){
    legiscan_budget_bills_temp[[j]] <- jsonlite::fromJSON(here::here(path, 'bill', paste0(gsub(' ', '', names(legiscan_budget_bills_temp)[j]), '.json')))$bill
  }

  # Save results to legiscan_budget_bills
  legiscan_budget_bills[[i]] <- legiscan_budget_bills_temp
  setTxtProgressBar(progress_bar, i)
}












# Download HTML versions of chaptered budget bills
# Create directory to save local copies of downloaded bills
path <- here::here('downloads', 'chaptered_budget_bills')
if(!dir.exists(path)){
  dir.create(path)
}
# Drop bills that don't have a chaptered version
chaptered_budget_bills_list <- lapply(legiscan_budget_bills, \(x) lapply(x, \(x) "Chaptered" %in% x$texts$type))

for(i in seq_along(legiscan_budget_bills)){
  chaptered_bills_this_year <- legiscan_budget_bills[[1]][unlist(chaptered_budget_bills_list[[1]])]
  
  for(j in seq_along(chaptered_bills_this_year)){
    # add check for local files
    
    request <- httr::GET('https://api.legiscan.com/',
                         query = list(key = legiscan_api_key,
                                      op = 'getDataSetList',
                                      state = 'CA'))
    
    url <- https://api.legiscan.com/?key=
      
      &op=getBillText&id=2976729
  }
  
  
  
  
}





chaptered_budget_bills_list <- legiscan_budget_bills[chaptered_budget_bills_list]
# Clean up spaces, split the year and bill number and use  those to generate Legiscan links
chaptered_budget_bills_list <- gsub(" ", '', names(chaptered_budget_bills_list)[chaptered_budget_bills_list])
chaptered_budget_bills_list <- strsplit(chaptered_budget_bills_list, '.', fixed = TRUE)
chaptered_budget_bills_list <- unlist(lapply(chaptered_budget_bills_list,
                                             \(x) paste0('https://legiscan.com/CA/text/', x[2], '/', x[1])))

progress_bar <- txtProgressBar(min=1,max=length(chaptered_budget_bills_list), style=3)

//*[@id="bill"]

for(i in seq_along(chaptered_budget_bills_list)){
  print(i)
  # Check if we've got all the files downloaded so we don't over-tax Legiscan
  if(!(length(list.files(path)) == length(chaptered_budget_bills_list)) | force_download_legiscan){
    # Get the HTML of the main Legiscan page for the bill and extract the link to the chaptered version
    bill_nodes  <- xml2::read_html(chaptered_budget_bills_list[i])
    bill_text <- xml_find_all(bill_nodes, "//*[@id='bill']")
    bill_filename <- xml_find_all(bill_nodes, "//div[@id='gaits-wrapper']//a")
    bill_filename <- xml_text(bill_filename[grep('-Chaptered\\.html', xml_text(bill_filename))])
    
    if(length(bill_text) == 1 & length(bill_filename) == 1){
      write_xml(bill_text,
                file = here::here(path, bill_filename))
    } else {
      warning('Could not find chaptered bill for ', chaptered_budget_bills_list[i], '.')
    }
  }
  Sys.sleep(.01)
  #setTxtProgressBar(progress_bar, i)
}



# Get LegiScan Person Data ####
# Sessions are returned with most recent first, so for the current session just index to [[1]]
# request <- httr::GET('https://api.legiscan.com/',
#                      query = list(key = legiscan_api_key,
#                                   op = 'getSessionPeople',
#                                   id = legiscan_dbs_list[[1]]$session_id))
# legiscan_person_list <- do.call(rbind.data.frame, content(request)$sessionpeople$people)

# Step 2 ####

# Step 3 ####