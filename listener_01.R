
# code for receiving data
library(openxlsx)
library(rPython)
library(stringr)

setwd("~/Dropbox/framework/") 

lookups <- "system_inputs/master_lookups.xlsx"
places <- readWorkbook(lookups, 'places')


library(rPython)

# Load/run the main Python script
python.load("code and sql/get_inbox.py")

# Get the variable
stuff <- python.get("result")
data <- python.get("data")

# data is just a string. Convert it to a list of UIDs
uid_list <- strsplit(data, ' ')[[1]]

# we'll want to compare this list of UIDs to a file next


# for uid in uid_list etc

fetch_uid <- uid_list[1]  # resist the urge to convert to a numeric !

python.assign("fetch_uid", fetch_uid)
python.load("code and sql/get_message.py")

mail_from <- python.get("mail_from")
mail_subject <- python.get("mail_subject")
mail_time <- python.get("mail_time")
mail_attachment_name <- python.get("mail_attachment_name")
mail_attachment <- python.get("mail_attachment")

