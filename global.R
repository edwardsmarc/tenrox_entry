library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(lubridate)
library(excelR)

lastmon <- function(x) 7 * floor(as.numeric(x-1+4)/7) + as.Date(1-4, origin="1970-01-01")
mdy <- function(x) paste(month(x, label = T), day(x), year(x), sep = "/")

timesheet_prep <- function(timesheet){
  
  timesheet %>% 
  mutate(duration = (timesheet$`End Time` - timesheet$`Start Time`)/60/60,
         date = as_date(`Start Date`)) %>%
  filter(!is.na(Categories)) %>%
  arrange(desc(date), `Start Time`)
}


full_table <- function(timesheet){
  
  prepped_timesheet <- timesheet_prep(timesheet)
  
  week <- lastmon(prepped_timesheet$date)
  
  tibble(Date = mdy(prepped_timesheet$date),
         Week = week,
         Hours = as.numeric(prepped_timesheet$duration),
         Project = prepped_timesheet$Categories,
         Details = prepped_timesheet$Subject) %>%
    filter(Week <= lastmon(Sys.Date())) %>%
    mutate(Week = mdy(Week))
}


weekly_sums <- function(timesheet){
  
  full_table(timesheet_prep(timesheet)) %>% 
    group_by(Week, Project) %>% 
    summarise(Hours = sum(Hours))
}

hours_this_week <- function(timesheet){
  full_table(timesheet_prep(timesheet)) %>%
    filter(Week == mdy(lastmon(Sys.Date()))) %>%
    summarise(sum = sum(Hours)) %>%
    pull(sum) %>%
    round(1)
}

full_columns = data.frame(title=c('Date', 'Week of', 'Hours', 'Project', 'Details'),
                     width= c(100, 100, 100, 200, 300))
weekly_columns = data.frame(title=c('Week of', 'Project', 'Hours'),
                     width= c(100, 300, 100))