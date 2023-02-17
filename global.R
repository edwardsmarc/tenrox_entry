library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(lubridate)
library(excelR)

lastmon <- function(x) 7 * floor(as.numeric(x-1+4)/7) + as.Date(1-4, origin="1970-01-01")


timesheet_prep <- function(timesheet){
  
  timesheet %>% 
  mutate(duration = (timesheet$`End Time` - timesheet$`Start Time`)/60/60,
         date = as_date(`Start Date`),
         mdy = paste(month(date, label = T), day(date), year(date), sep = "/")) %>%
  filter(!is.na(Categories)) %>%
  arrange(date, `Start Time`)
}


full_table <- function(prepped_timesheet){
  
  tibble(`Date (M/D/Y)` = prepped_timesheet$mdy,
         Week = as.character(lastmon(prepped_timesheet$date)),
         Hours = as.numeric(prepped_timesheet$duration),
         `Project/Activity` = prepped_timesheet$Categories,
         `Deliverable/Details` = prepped_timesheet$Subject) %>%
    filter(Week <= lastmon(Sys.Date()))
}


weekly_sums <- function(x){
  x %>% 
    group_by(`Project/Activity`, Week) %>% 
    summarise(Hours = sum(Hours)) %>%
    arrange(Week)
}

render_table_function <- function(df, csv_path){
  
  renderExcel({
    
    req(csv_path)
    
    excelTable(df)
    
  })
  
}