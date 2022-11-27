### === 1) PACKAGES === ###

library(shiny) # Shiny package
library(ggplot2) # Used to create plots which are then rendered by plotly
library(plotly) # Used for rendering interactive plots
library(tidyverse) # Staple R library
library(DT) # Used for rendering nice data tables
library(googledrive) # Used for storing scenarios between user sessions





### === 3) FUNCTIONS === ###

# This function checks which scenario files are already available locally and downloads only those which are missing.
downloadScenarios <- function(drive_files){
  downloadedScenarios <- list.files(here::here('data/scenarios'))
  for(ii in 1:nrow(drive_files)){

    if(drive_files$name[ii] %in% downloadedScenarios){ next()}
    else{
    googledrive::drive_download(file = drive_files$id[ii], path = paste0(here::here('data/scenarios/'), drive_files$name[ii]), overwrite = TRUE)
    }
  }
}


# This function writes the local scenarios to the drive folder. It matches names and only uploads scenarios which are unique to the current session.
writeScen2Drive <- function(){
  latestlocalscen <- list.files(here::here('data/scenarios'), full.names = TRUE)
  latestlocalscen_names <- list.files(here::here('data/scenarios'), full.names = FALSE)

  for(ii in 1:length(latestlocalscen)){
    if(latestlocalscen_names[ii] %in% drive_files$name){next()}
    else{
    googledrive::drive_upload(media = latestlocalscen[ii], path = shared_drive_loc$id)
    }
  }
}

# = FUNCTIONS = #
lan_expander <- function(user_lan){ #user_lan is a vector of characters from selectizeInput.
  user_lan <- user_lan %>% unlist()
  user_lan_conv <- c()
  for(lan in user_lan){
    user_lan_conv <- append(user_lan_conv, lan)
  }

  if('All' %in% user_lan){
    user_lan_conv <- c('Sheffield', 'Barnsley', 'Rotherham', 'Doncaster')
  }


  return(user_lan_conv)
} # Convert from user input Local Authorities to usable ones in data
ss_expander <- function(user_ss_group){
  user_ss_group <- user_ss_group %>% unlist()
  user_ss_conv <- c()
  for(user_ss in user_ss_group){
    user_ss_conv <- append(user_ss_conv, ss_expander_internal(user_ss))
  }

  return(user_ss_conv %>% unique())
} # Convert from user input SS to usable ones in data
ss_expander_internal <- function(user_ss){
  beis_min <- readr::read_csv(here::here('data/datasets/beis_min.csv'))
  commercial_ss <- beis_min$ss[beis_min$s == 'Commercial'] %>% unique()
  domestic_ss <- beis_min$ss[beis_min$s == 'Domestic'] %>% unique()
  industry_ss <- beis_min$ss[beis_min$s == 'Industry'] %>% unique()
  lulucf_ss <- beis_min$ss[beis_min$s == 'LULUCF'] %>% unique()
  public_ss <- beis_min$ss[beis_min$s == 'Public Sector'] %>% unique()
  transport_ss <- beis_min$ss[beis_min$s == 'Transport'] %>% unique()



  if('All Commercial' %in% user_ss){
    user_ss <- user_ss %>% gsub('All Commercial', commercial_ss)
  }
  if('All Domestic' %in% user_ss){
    user_ss <- user_ss %>% gsub('All Domestic', domestic_ss)
  }
  if('All Industry' %in% user_ss){
    user_ss <- user_ss %>% gsub('All Industry', industry_ss)
  }
  if('All LULUCF' %in% user_ss){
    user_ss <- user_ss %>% gsub('All LULUCF', lulucf_ss)
  }
  if('All Public Sector' %in% user_ss){
    user_ss <- user_ss %>% gsub('All Public Sector', public_ss)
  }
  if('All Transport' %in% user_ss){
    user_ss <- user_ss %>% gsub('All Transport', transport_ss)
  }
  if('All' %in% user_ss){
    user_ss <- c(commercial_ss, domestic_ss, industry_ss,lulucf_ss, public_ss, transport_ss)
  }

  return(user_ss)
} # Convert from user input SS to usable ones in data (internal to ss_expander())
apply_intervention <- function(beis_min, ir){

  data_groups <- beis_min %>% group_by(lan, ss) %>% group_split()
  data_keys <- beis_min %>% group_by(lan, ss) %>% group_keys()
  data_selections <- dplyr::select(ir, lan, ss)
  data_sk_int <- plyr::join.keys(data_keys, data_selections)
  data_selectkeys <- data_sk_int$y # Now identified all the selected lan/ss pairs to act on from the list created by group_split().

  # Proceed to perform row-wise changes to emissions values on each of the dataframes in the list which are identified by selectkeys and once completed, use bind_rows() to transform the list back into a single, modified dataframe.

  for(jj in data_selectkeys){


    moddf <- data_groups[[jj]]
    unit <- ir$unit %>% unique()
    value <- ir$value %>% unique()
    type <- ir$type %>% unique()
    start_year <- ir$start_year %>% unique()
    end_year <- ir$end_year %>% unique()



    # PERCENTAGE CHANGE #
    if(unit == '%'){

      value <- 1 + (value/100) # Convert value to percentage, otherwise leave alone
      if(type == 'Gradient'){
        for(kk in start_year:end_year){

          moddf$emissions[moddf$year == kk] <- moddf$emissions[moddf$year == kk-1] * value

        }

      }
      else if (type == 'Step'){
        for(kk in start_year:end_year){
          moddf$emissions[moddf$year == kk] <- moddf$emissions[moddf$year == kk] * value
        }
      }
    }
    # ABSOLUTE CHANGE #
    else{
      if(type == 'Gradient'){
        for(kk in start_year:end_year){
          moddf$emissions[moddf$year == kk] <- moddf$emissions[moddf$year == kk-1] + value
        }

      }
      else if (type == 'Step'){
        for(kk in start_year:end_year){
          moddf$emissions[moddf$year == kk] <- moddf$emissions[moddf$year == kk] + value
        }
      }

    }
    moddf$emissions[moddf$emissions <= 0] <- 0
    data_groups[[jj]] <- moddf


  }

  modified_data <- bind_rows(data_groups)
  return(modified_data)
} #Apply a specific intervention to the data
apply_scenario <- function(beis_data, scenario_df){
  new_data <- beis_data
  for(ii in 1:nrow(scenario_df)){

    ir <- scenario_df[ii,]
    ir_lan <- lan_expander(ir$lan)
    readr::write_csv(ir, here::here('data/debug/ir_lan.csv'))
    ir_ss <- ss_expander(ir$ss)
    ir <- expand.grid(lan = ir_lan, ss = ir_ss, type = ir$type, start_year = ir$start_year, end_year = ir$end_year, value = ir$value, unit = ir$unit)
    readr::write_csv(ir, here::here('data/debug/ir.csv'))

    new_data <- apply_intervention(new_data, ir)
  }
  return(new_data)
}


### === 3) GOOGLE DRIVE INTEGRATION === ###

## This code concerns the Google drive integration, using my own key - this should be updated to the details of whoever maintains
# the files in future.
options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = here::here(".secrets")
)

shared_drive <- 'ODA-REBF' # The name of the drive folder
shared_drive_loc <- googledrive::shared_drive_get(name = shared_drive) # Returns the path for the selected drive folder

drive_files <- drive_find(shared_drive = shared_drive) # List files in drive folder

downloadScenarios(drive_files) # Run the downloadScenarios function. This is done once per application session.

# = DATA = #
data_0 <- readr::read_csv(here::here('data/datasets/beis_min.csv')) # The default data set
subsector_choices <- readRDS(here::here('data/objects/subsector_choices')) # A list of all of the subsectors available

