### LegiScan Data Pull and Search
#     Script to load LegInfo data and search it based on different criteria
#
#     By Patrick Rogers, California Research Bureau
#       Sept 2024


# Get LegiScan Bill Data ####
get_legiscan_data <- function(force_download=FALSE){
  # Packages
  require(here)       # Relative paths
  require(httr)       # http requests
  require(base64enc)  # Decode Base64 response from Legiscan
  
  # Load API key
  source(file = here::here('private', 'legiscan_api_key.R'))
  path <- here::here('data', 'legiscan_dbs_list.RData')
  
  # List of  Databases
  if(file.exists(path) & !force_download){
    message('List of Legiscan databases already downloaded, loading from local file.')
    legiscan_dbs_list <- readRDS(path)
  } else {
    request <- httr::GET('https://api.legiscan.com/',
                         query = list(key = legiscan_api_key,
                                      op = 'getDataSetList',
                                      state = 'CA'))
    legiscan_dbs_list <- httr::content(request)$datasetlist
    saveRDS(legiscan_dbs_list, path)
  }
  
  # Individual Database
  if(file.exists(path) & !force_download){
    message('Legiscan databases already downloaded, loading from local file.')
    legiscan_dbs <- readRDS(here::here('data', 'legiscan_dbs.RData'))
  } else {
    legiscan_dbs <- vector(mode = 'list', length = length(legiscan_dbs_list))
    progress_bar <- txtProgressBar(min=1,max=length(legiscan_dbs), style=3)
    
    for(i in seq_along(legiscan_dbs)){
      # First check if we've already downloaded this dataset
      path <- here::here('data', legiscan_dbs_list[[i]]$session_title)
      if(dir.exists(file.path(path, 'CA')) & !force_download){
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
        writeBin(base64enc::base64decode(httr::content(request)$dataset$zip), temp)
        unzip(temp, list=FALSE, exdir = path)
        
        legiscan_dbs[[i]] <- httr::content(request)
        
      } else {
        legiscan_dbs[[i]] <- NULL
        warning('Request to ', request$url, ' returned status code ', request$status_code, '.')
      }
      
      setTxtProgressBar(progress_bar, i)
    }
    saveRDS(legiscan_dbs, here::here('data', 'legiscan_dbs.RData'))
  }
  return(legiscan_dbs)
}