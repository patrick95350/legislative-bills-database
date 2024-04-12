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

# Download HTML versions of chaptered budget bills ####
# Create directory to save local copies of downloaded bills
path <- here::here('downloads', 'chaptered_budget_bills')
if(!dir.exists(path)){
  dir.create(path)
}
# Drop bills that don't have a chaptered version
chaptered_budget_bills_list <- lapply(legiscan_budget_bills, \(x) lapply(x, \(x) 'Chaptered' %in% x$texts$type))

progress_bar <- txtProgressBar(min=1,max=length(legiscan_budget_bills), style=3)
setTxtProgressBar(progress_bar, 1)

for(i in seq_along(legiscan_budget_bills)){
  current_year <- names(legiscan_budget_bills)[i]
  # Get a list of the chaptered budget bills for current year
  chaptered_bills_this_year <- legiscan_budget_bills[[i]][unlist(chaptered_budget_bills_list[[i]])]
  
  for(j in seq_along(chaptered_bills_this_year)){
    current_bill <- gsub(' ', '',names(chaptered_bills_this_year)[j])
    # Check if we've got all the files downloaded so we don't over-tax Legiscan
    if(!(length(list.files(path)) == sum(unlist(chaptered_budget_bills_list))) | force_download_legiscan){
      # Name the HTML files  with the year and bill number
      html_file <- paste0(current_year, '_', current_bill, '.html')
      # Get the doc id from the texts list from each bill, we want the last id, because that's the chaptered version
      doc_id <- chaptered_bills_this_year[[j]]$texts$doc_id
      doc_id <- doc_id[length(doc_id)]
      # Send an API call to LegiScan to get the bill text
      request <- httr::GET('https://api.legiscan.com/',
                           query = list(key = legiscan_api_key,
                                        op = 'getBillText',
                                        id = doc_id))
      # Extract the content from the response, decode it and write it as a bbinary to the HTML file
      writeBin(base64_dec(httr::content(request)$text$doc), here::here(path, html_file))
    }
  }
  setTxtProgressBar(progress_bar, i)
}

# Parse HTML to extract budget items ####
path <- here::here('downloads', 'chaptered_budget_bills')
budget_HTML_files_list <- list.files(path = path)

#budget_bill_appropriations <- vector(mode = 'list', length = length(budget_HTML_files_list))
pomona_bills <- vector(mode = 'list', length = length(budget_HTML_files_list))

# Loop over each bill and parse
for(i in seq_along(budget_HTML_files_list)){
  
  # Read  in HTML and extract text
  current_bill_text <- xml2::read_html(here::here(path, budget_HTML_files_list[i]), encoding = 'UTF-8', options = c('IGNORE_ENC')) |> xml2::xml_text()
  
  # Check if there are appropriations
  # if(!grepl('Appropriation: yes', current_bill_text)){
  #   #budget_bill_appropriations[[i]] <- NULL
  #   message('No appropriations found in ', budget_HTML_files_list[i], '.')
  # } else {
  #   message('Appropriations found in ', budget_HTML_files_list[i], '.')
  # }
  # Handle carriage returns and tabs
  #current_bill_text <- unlist(strsplit(current_bill_text, split = '\\n|\\t'))
  
  # Remove spaces and blanks
  #current_bill_text <- current_bill_text[!current_bill_text=='']
  #current_bill_text <- gsub('^ +', '', current_bill_text)
  
  # Drop Counsel's Digest
  #start_of_bill_pattern <- 'THE PEOPLE OF THE STATE OF CALIFORNIA DO ENACT AS FOLLOWS|Resolved by the Senate, the Assembly concurring|Resolved by the Assembly, the Senate concurring'
  #current_bill_text <- current_bill_text[(grep(start_of_bill_pattern, current_bill_text) + 1 ):length(current_bill_text)]
  
  # Add extra blank at end of bill for looping
  #current_bill_text <- c(current_bill_text, '')
  
  # Remove statutory sections
  
  # Condense listed items into single line
  # Patterns
  #   '^SEC[T\\.]I?O?N? [0-9]+' matches 'SECTION' and 'SEC.' headings
  #   '^\\(\\w+?\\)' matches '(a) (1) (A)' headings
  #   '^[0-9]+\\.  ' matches '69453.  ' statute headings
  # headings_pattern <- '^SEC[T\\.]I?O?N? [0-9]+|^\\(\\w+?\\)|^[0-9]+\\.'
  # line_item_index <- grep(headings_pattern, current_bill_text)
  # 
  # for(j in  seq_along(line_item_index)){
  #   k <- line_item_index[j] + 1
  #   while(!grepl(headings_pattern, current_bill_text[k]) & !(current_bill_text[k] == '')){
  #     current_bill_text[line_item_index[j]] <- paste(current_bill_text[line_item_index[j]], current_bill_text[k])
  #     current_bill_text[k] <- ''
  #     k <- k+1
  #   }
  # }
  
  # Search for POMONA in the text
  pomona_search <- grepl('POMONA', current_bill_text, ignore.case = TRUE)
  
  pomona_bills[[i]] <- sum(pomona_search)
  
  # ### STOPPED HERE IDENTIFYING INDIVIDUAL ITEMS, SWITCHING TO JUST SEARCHING FOR POMONA
  # 
  # # Process line items with dollar appropriations
  # # Find broad appropriations that aren't broken down, or just the broken out items
  # line_item_index <- which((grepl('The sum of .+? dollars \\(\\$[0-9,]+\\) is hereby appropriated', current_bill_text) &
  #                             !grepl('allocation pursuant to the following schedule', current_bill_text)) |
  #                            grepl('^\\(\\w+?\\) .+?\\$[0-9,]+', current_bill_text))
  # current_bill_line_items <- vector(mode = 'list', length = length(line_item_index))
  # 
  # for(j in seq_along(line_item_index)){
  #   current_bill_line_items[[j]] <- c(gsub('^\\(\\w+?\\) (.+?) +\\$[0-9\\,]+', '\\1', current_bill_text[line_item_index[j]]),
  #                                     gsub('^\\(\\w+?\\) .+? +(\\$[0-9\\,]+) *.*$', '\\1', current_bill_text[line_item_index[j]]))
  # }
  # 
  # temp <- do.call(rbind.data.frame, current_bill_line_items)
  # colnames(temp)  <- c('desc', 'amount')
  # temp$bill <- budget_HTML_files_list[i]
  # 
  # budget_bill_appropriations[[i]] <- temp
  
  
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