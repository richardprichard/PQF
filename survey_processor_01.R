# code to hoover up files and reduce them to a single line
library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)

# hoover up all the datafiles in the survey directory

setwd("~/My Dropbox/F4Q 2014/surveys/") 
filenames <- list.files(pattern = "*R_data_file.csv", full.names=TRUE)
tables <- lapply(filenames, function(i) {
  read.csv(i, quote="\"", encoding="utf8", as.is=TRUE)
})
survey <- do.call(rbind, tables)
rm(tables)


# tidy the file before we chop it up
survey$submitdate <- ymd_hms(survey$submitdate) # timestamp it
survey <- survey[!is.na(survey$submitdate),] # drop rows without timestamp
survey <- select(survey, -(lastpage:refurl)) # drop the unused fields
survey <- select(survey, -(interviewtime:NEIGHimproveTime)) # drop the unused fields
survey <- rename(survey, replace=c(attribute_1='council', attribute_2='survey_type', attribute_3='reference')) 
survey$council <- as.factor(survey$council)
survey$survey_type <- as.factor(survey$survey_type)
survey$reference <- as.factor(survey$reference)


# make individual survey datasets

survey_app <- filter(survey, survey_type == 'applicant')
survey_app <- select(survey_app, -(HOP1help:NEIGHimprove))

survey_agent <- filter(survey, survey_type == 'agent')
survey_agent <- select(survey_agent, -(HOP1help:NEIGHimprove))

survey_review <- filter(survey, survey_type == 'review')
survey_review <- select(survey_review, (id:submitdate), starts_with('HOP'), firstname:reference )

# neighbours are slightly different, as there may be more than one per application, so we count and average them
survey_neigh <- filter(survey, survey_type == 'neighbour')
survey_neigh <- select(survey_neigh, (id:submitdate), (NEIGHfindout:reference))
survey_neigh <- select(survey_neigh, NEIGHsuccess, NEIGHoutof10.NEIGH10., council, reference)

survey_neigh_long <- melt(survey_neigh, id=c("council", "reference")) # melt into 'long' form
survey_means <- dcast(survey_neigh_long, council + reference ~ variable, mean)
survey_counts <- dcast(survey_neigh_long, council + reference ~ variable, length)
survey_counts <- rename(survey_counts, replace=c(NEIGHsuccess='response_count')) # we only want one count
survey_counts <- select(survey_counts, (council:response_count))

survey_neigh_summary <- merge(survey_means, survey_counts)


# remake the survey data by rejoining it to the original set of council / reference pairs

survey <- unique(select(survey, council, reference)) # unique pairs discard duplicate references

survey_all <- join_all(list(survey, survey_app, survey_agent, survey_review, survey_neigh_summary), type="left", match="first" )



