
library(ggplot2)
library(RColorBrewer)
library(scales)
library(plyr)
library(dplyr)
library(stringr)
library(lubridate)
library(reshape2)
library(knitr)
library(markdown)
library(R2HTML)
library(gridExtra)

# drawing functions - so that any tweaks etc are easily applied

plot_over_time <- function() {  # summarise stuff and plot it 
  # this is from the new dplyr package. See the vignette.
  # begin by summing the values
  arrange(i, code, lead_x) # arrange in order of the x axis
  x_points <- group_by(i, lead_x, code)
  if (plot_type == 'sum') i <- summarise(x_points, lead_val = sum(lead_val, na.rm=TRUE))
  if (plot_type == 'count') i <- summarise(x_points, lead_val = n(lead_val, na.rm=TRUE))
  if (plot_type == 'mean') i <- summarise(x_points, lead_val = mean(lead_val, na.rm=TRUE))
  if (plot_type == 'percent') i <- summarise(x_points, lead_val = mean(lead_val, na.rm=TRUE))
  arrange(j, code, group_x) # arrange in order of the x axis
  x_points <- group_by(j, group_x, code)
  if (plot_type == 'sum') j <- summarise(x_points, group_val = sum(group_val, na.rm=TRUE))
  if (plot_type == 'count') j <- summarise(x_points, group_val = n(group_val, na.rm=TRUE))
  if (plot_type == 'mean') j <- summarise(x_points, group_val = mean(group_val, na.rm=TRUE))
  if (plot_type == 'percent') j <- summarise(x_points, group_val = mean(group_val, na.rm=TRUE))
  p <- ggplot()
  p <- p + geom_line(data=j, aes(x=group_x, y=group_val, colour=code, group=code), size=1)
  p <- p + stat_smooth(data=j, aes(x=group_x, y=group_val, group=1), method="loess", colour="black")
  p <- p + geom_line(data=i, aes(x=lead_x, y=lead_val, colour=code, group=code), size=2)
  p <- p + stat_smooth(data=i, aes(x=lead_x, y=lead_val, group=1), method="lm", se=FALSE, colour="red", size=2)
  if (plot_type == 'percent') {
    p <- p + scale_y_continuous(labels = percent)
    p <- p + coord_cartesian(ylim = y_lims)
  }
  p <- p + labs(title = t, x=x_label, y=y_label)
  return(p)
}


# table helper
report_table <- function(data) {  # print table without dp
  kable(data, digits=0, format="html", row.names=FALSE)
#  HTML(data, file="",Border=1, align="right", digits=0,row.names=FALSE)
}


# ie this table helper works out how many of your table i the i$cat  is
make_prop_table <- function(i, t) {
# this works out what proportion of the whole each i$cat is for summarising tables
# by category
  a <- filter(i, code == report_lead)
  b <- filter(i, code != report_lead)
  a.summary <- a %.%
    group_by(cat) %.%
    summarise(n=n()) %.%
    mutate(perc = sprintf("%.0f%%", n/sum(n)*100))
  a.summary <- rename(a.summary, replace=c(cat=t, n='your count', perc='your perc'))
  
  b.summary <- b %.%
    group_by(cat) %.%
    summarise(n=n()) %.%
    mutate(perc = sprintf("%.0f%%", n/sum(n)*100))
  b.summary <- rename(b.summary, replace=c(cat=t, n='their count', perc='their perc'))
  
  c <- join(a.summary, b.summary, by=t, type="full")    
  return(c[order(c[,1]),]) # return c sorted by 1st col
}

make_perc_table <- function(i, t) { 
# this helper averages values passed to it in table 'i'
# it breaks the table into categories and then means the val 
  a <- filter(i, code == report_lead)
  b <- filter(i, code != report_lead)  
  
  a.summary <- a %.%
    group_by(cat) %.%
    summarise(count=n(),
              perc=sprintf("%.0f%%",mean(val, na.rm=TRUE)*100)) 
  a.summary <- rename(a.summary, replace=c(cat=t, count='your count', perc='your %age'))
  
  b.summary <- b %.%
    group_by(cat) %.%
    summarise(count =n(),
              perc=sprintf("%.0f%%",mean(val, na.rm=TRUE)*100)) 
  b.summary <- rename(b.summary, replace=c(cat=t, count='their count', perc='their %age'))
  c <- join(a.summary, b.summary, by=t, type="full")
  
  return(c[order(c[,1]),]) # return c sorted by 1st col
}



plot_table <- function(i, t) {
  p <- ggplot(data=i, aes(x=code, fill=cat))
  p <- p + geom_bar(position="fill")
  p <- p + scale_y_continuous(labels = percent)
  p <- p + labs(title = t)
 # p <- p + coord_flip() 
  return(p)
}


report_date <- as.character(today())
setwd("~/Dropbox/F4Q 2014/data/") 
# setwd("~/My Dropbox/F4Q 2014/data/") 
apps = tbl_df(read.csv("export_df.csv",header=TRUE, sep=",",quote="\"", encoding="windows-1252"))
places = tbl_df(read.csv("places.txt",header=TRUE, sep="\t",quote="\"", encoding="windows-1252"))


setwd("~/Dropbox/F4Q 2014/outputs/") 
# setwd("~/My Dropbox/F4Q 2014/outputs/") 

for (i in 1:length(places$code)) {
  report_lead <- as.character(places$code[i])
  output_file <- str_c(as.character(places$council[i]),'.html')
  attach(places)
  report_group <- c(as.character(report_group_01[i]), as.character(report_group_02[i]),
                    as.character(report_group_03[i]), as.character(report_group_04[i]),
                    as.character(report_group_05[i]), as.character(report_group_06[i]),
                    as.character(report_group_07[i]), as.character(report_group_08[i]),
                    as.character(report_group_09[i]), as.character(report_group_10[i]),
                    as.character(report_group_11[i]), as.character(report_group_12[i])
  )
  detach(places)
  report_group <- report_group[!is.na(report_group)]
  lead_council <- filter(apps, code==report_lead)
  
  comparator_group <- apps[which(apps$code %in% report_group),] # comparator_group <- filter(apps, code %in% group) this doesn't work
  whole_set <- rbind(lead_council, comparator_group)
  print(report_lead)
  
  # fire off each report
  knit("_report_markdown4.Rmd")
  markdownToHTML("_report_markdown4.md", output=output_file, stylesheet='pas-report-style.css', encoding='windows-1252')

  # or stop here so we can mess about making the report
  # break

}

# to dump a table out to double check in excel e.g.
# write.csv(whole_set, 'whole_set.csv', row.names = FALSE, na="")



