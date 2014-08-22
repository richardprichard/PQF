
# version 3
library(plyr)
library(dplyr)
library(stringr)
library(lubridate)
library(reshape2)
library(openxlsx)

maxdays <- 1000 # applications will fail verification if they take longer than 3 years
start_date <- dmy('01/06/2012') # elapsed months etc work from this date
end_date <- dmy('31/03/2014') # applications after this date discarded. I've tried to make this a whole number of quarters. 
todays_date <- ymd(today())
missing <- "* missing *"

# remember to switch the working directory if you switch between windows and linux
setwd("~/My Dropbox/framework/") 
# setwd("~/Dropbox/framework/") 

check_date <- function(x) { # helper fn to ensure that dates were detected properly on import
  if (is.POSIXct(x)) return(x) # it's OK
  if (is.numeric(x)) return(as.POSIXct(convertToDate(x)))
  x <- as.numeric(x) # attempt to convert to number
  return(as.POSIXct(convertToDate(x))) # otherwise convert 
}


# excel code
lookups <- "system_inputs/master_lookups.xlsx"
places <- readWorkbook(lookups, 'places')
labels <- readWorkbook(lookups, 'labels')
labels_orig <- labels
development <- readWorkbook(lookups, 'development')


# trim the places (we don't need reporting groups as this stage) and factorise
places <- select(places, -(report_group_01:report_group_12), -email)
places$council <- as.factor(places$council)
places$code <- as.factor(places$code)
places$council_type <- as.factor(places$council_type)
# ditto labels
labels$composite_lookup <- paste(labels$list, labels$shortname, sep="_")
labels$list <- as.factor(labels$list)
labels$shortname <- as.factor(labels$shortname)
labels$group <- as.factor(labels$group)
labels$composite_lookup <- as.factor(labels$composite_lookup)
mappings <- unique(labels$list) # this creates the list of maps to cycle through
labels <- select(labels, composite_lookup, group, val) # ignore notes etc
# ditto for development
development <- select(development, -list)
development$shortname <- as.factor(development$shortname)
development$dev_group <- as.factor(development$dev_group)

# get council data
filenames <- list.files(path="council_inputs/", pattern = "*.xlsx", full.names=TRUE)

for (filename in filenames) {   #  filename <- filenames[2]

  lookups <- data.frame('council'=character(), 'list'=character(), 'local_value'=character(),
                        'local_notes'=character(),'system_value'=character(),
                        'system_count'=character(), 'system_notes'=character())
  
  apps <- readWorkbook(filename, 'applications')
  maps <- readWorkbook(filename, 'lookups')
  council <- as.character(apps$council[1])
  apps_orig <- apps
  maps_orig <- maps
  
  # turn the various lookups into factors (will make it all quicker)
  apps$council <- as.factor(apps$council)
  apps$application_type <- as.factor(apps$application_type)
  apps$development_category <- as.factor(apps$development_category)
  apps$application_status <- as.factor(apps$application_status)
  apps$decision_route <- as.factor(apps$decision_route)
  apps$decision_recommended <- as.factor(apps$decision_recommended)
  apps$decision_issued <- as.factor(apps$decision_issued)
  
  apps$date_valid <- check_date(apps$date_valid)
  apps$date_received <- check_date(apps$date_received)
  apps$date_decision_made <- check_date(apps$date_decision_made)
  apps$date_decision_issued <- check_date(apps$date_decision_issued)
  
  apps$date_error <- 0 # if it's still zero at the end it's passed all the tests  
  
  # calculate the fields derived directly from the data
  apps$days_makevalid <- as.numeric(difftime(apps$date_valid, apps$date_received, units=c("day")))
  apps$days_validtodecision <- as.numeric(difftime(apps$date_decision_issued, apps$date_valid, units=c("day")))
  apps$days_receipttodecision <- as.numeric(difftime(apps$date_decision_issued, apps$date_received, units=c("day")))
  
  # and check that dates make sense
  # do dates go backward ? or bigger than our limit ?
  # or in the future ?
  apps$date_error <- 0 # if it's still zero at the end it's passed all the tests
  apps$date_error <- 0+ ifelse(is.na(apps$date_received),1,0) + ifelse(is.na(apps$date_valid),1,0)
  apps$date_error <- apps$date_error + ifelse(is.na(apps$date_decision_made),1,0) + ifelse(is.na(apps$decision_issued),1,0)
  apps$date_error <- apps$date_error + ifelse(apps$date_received < start_date,1,0) + ifelse(apps$date_received > end_date,1,0)
  
  apps$date_error <- apps$date_error + ifelse(apps$days_makevalid<0,1,0) + ifelse(apps$days_makevalid>maxdays,1,0) +
    ifelse(apps$days_validtodecision<0,1,0) + ifelse(apps$days_validtodecision>maxdays,1,0) + 
    ifelse(apps$days_receipttodecision<0,1,0) + ifelse(apps$days_receipttodecision>maxdays,1,0)
  
  apps$date_error <- apps$date_error + ifelse(apps$date_valid > todays_date,1,0) +
    ifelse(apps$date_decision_made > todays_date,1,0) + ifelse(apps$date_decision_issued > todays_date,1,0)
  
  # -------------------------------------------------------------------------
  # list and dump records with malformed dates, as they just create annoying factors
  
  date_problems <- filter(apps, date_error>0) # this will be written into the xls file
  apps <-filter(apps, date_error == 0 | is.na(date_error)) # remove them from subsequent work
  
  # -----------------------------------------------------------------------------
  
  # to group applications around when they're received:
  apps$received_month <- as.integer(
    (year(apps$date_received) - year(start_date)) * 12 + 
      month(apps$date_received) - month(start_date))+1
  # apps$received_quarter <- str_join("Q", floor(apps$received_month/3))
  apps$received_quarter <- str_join("Q", round((apps$received_month+1)/3,0))
  
  # to group applications around when they're decided:
  apps$decision_month <- as.integer(
    (year(apps$date_decision_issued) - year(start_date)) * 12 + 
      month(apps$date_decision_issued) - month(start_date)) +1
  apps$decision_quarter <- str_join("Q", round((apps$decision_month+1)/3,0))
  apps$is_valid_day1 <-ifelse(apps$date_valid == apps$date_received,1,0)
  apps$is_8wk <- ifelse(apps$days_validtodecision <= 8*7,1,0)
  apps$is_13wk <- ifelse(apps$days_validtodecision <= 13*7,1,0)
  apps$is_26wk <- ifelse(apps$days_validtodecision <= 26*7,1,0)
  apps$is_zero_fee <- ifelse((apps$application_fee ==0 | is.na(apps$application_fee)),1,0)
  # portal applications are those that have a code (PP12345) or a 'Y'
  apps$is_portal <- ifelse(str_length(apps$portal)>1,1,0) + ifelse(str_detect(apps$portal, ignore.case('Y')),1,0)
  
  
  # Joining job 1------------------------------------------
  # join to the 'place' master reference tables
  apps <- left_join(apps, places, by='council', copy = "TRUE") # this brings in a field called "code" and an index relating to houseprice
  
  # Joining job 2 -----------------------------------------
  # join to the label lookups (of which there are 5)
  # for each of the five lookups, we do a double look-up to bring across the masters
  
  for (i in mappings) {  # i<- mappings[1]
    apps$composite_lookup <- as.factor(paste(i, apps[[i]], sep='_')) # make composite
    maps_subset <- filter(maps, list == i)
    if (length(maps_subset$system_value) == 0) { # nothing to lookup !
      apps[c(str_join(i,'_sys'), str_join(i,'_val'), str_join(i,'_group'))] <- NA # add empty columns
      
      temp <- data.frame('council' = council, 'list' = i, 'local_value' = apps[[i]],
                         'system_value' = missing)
      lookups <- rbind.fill(lookups,temp)
      next
    }
    maps_subset$composite_lookup <- as.factor(paste(i, maps_subset$system_value, sep="_")) # make composite that matches from the local to the system
    maps_subset <- left_join(maps_subset, labels, by=c('composite_lookup'), copy='TRUE')
    maps_subset$composite_lookup <- as.factor(paste(i, maps_subset$local_value, sep="_")) # make composite that matches from the local to the system)
    maps_subset = select(maps_subset, composite_lookup, system_value, group, val)
    
    apps <- left_join(apps, maps_subset, by=c('composite_lookup'), copy = "TRUE")
    temp <- data.frame('council' = council, 'list' = i, 'local_value' = apps[[i]],
                       'system_value' = apps$system_value, 'system_notes' = apps$group)
    lookups <- rbind.fill(lookups,temp)
    
    fail_join <- anti_join(maps_subset, apps, by=c('composite_lookup'))
    temp <- data.frame('council' = council, 'list' = i, 'local_value' = apps[[i]],
                       'system_value' = missing)
    lookups <- rbind.fill(lookups,temp)
        
    names(apps)[names(apps)=="system_value"] <- str_join(i, "_sys") # change the field name to map against the one just joined to the main table
    names(apps)[names(apps)=="val"] <- str_join(i, "_val") # change the field name to map against the one just joined to the main table
    names(apps)[names(apps)=="group"] <- str_join(i, "_group") # change the field name to map against the one just joined to the main table
  }
  apps <- select(apps, -composite_lookup)

  # joining job 3 -----------------------------
  
  # translate from the council's local reference to our development_categories
  listname <- 'development_code'
  temp <- filter(maps, list ==listname)
  
  if (length(temp$shortname) == 0) { # no lookups supplied !
    apps[c('development_category_sys', 'development_category_group', 
           'development_category_val','value')] <- NA # add empty columns
    
    apps_temp <- data.frame('council' = council, 'list' = listname, 
                           'local_value' = apps$development_category,
                           'system_value' = missing)
    lookups <- rbind.fill(lookups, apps_temp)  
  } else {
    temp <- rename(temp, replace =c(system_value='shortname'))
    temp$shortname <- as.factor(temp$shortname)
    temp <- left_join(temp, development, by = c('shortname'), copy='TRUE')
    temp <- select(temp, local_value, shortname, dev_group, hours, investment)
    temp <- rename(temp, replace =c(local_value='development_category'))
    temp$development_category <- as.factor(temp$development_category)
    apps <- left_join(apps, temp, by=c('development_category'), copy="TRUE")
     
    success <- data.frame('council' = council, 'list' = listname,
                          'local_value' = apps$development_category, 
                          'system_value' = apps$shortname, 
                          'system_notes' = apps$dev_group)
    lookups <- rbind.fill(lookups, success)
    
    fail_join <- anti_join(temp, apps, by=c('development_category'))
    fail  <- data.frame('council' = council, 'list' = listname,
                        'local_value' = apps$development_category, 
                        'system_value'= missing)
    lookups <- rbind.fill(lookups, fail)
    apps$value <- apps$investment * apps$price_weight    
  }

  
  # process all the lookups we've been hoovering up
  lookups_temp <- group_by(lookups, council, list, local_value, system_value, system_notes)
  lookups <- summarise(lookups_temp, count=n())
  lookups <- arrange(lookups, list, desc(local_value))
    
  # save the translated file in interim outputs  

  newname <- str_replace(filename, 'council_inputs', 'interim_outputs')
  
  wb <- createWorkbook()
  addWorksheet(wb, 'applications')
  addWorksheet(wb, 'lookups')
  addWorksheet(wb, 'date_errors')
  addWorksheet(wb, 'translated')
  addWorksheet(wb, 'sys_development')
  addWorksheet(wb, 'sys_names')
  writeData(wb, 'translated', x=apps, colNames=TRUE)
  writeData(wb, 'date_errors', x=date_problems, colNames=TRUE)
  writeData(wb, 'applications', x=apps_orig, colNames=TRUE)
  writeData(wb, 'lookups', x=lookups, colNames=TRUE)
  writeData(wb, 'sys_development', x=development, colNames=TRUE)
  writeData(wb, 'sys_names', x=labels_orig, colNames=TRUE)
  saveWorkbook(wb, newname, overwrite = TRUE)
  
  rm(wb)
}  

# ------------- fin

