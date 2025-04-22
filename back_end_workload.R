
# The aim of this script is to have the largest workload detached from the Shiny App
# Launch this file before to start the app


#---
### Import Dataset ###


load('Enron.Rdata')



#---
### Import Librairies ###

library(rlist)
library(stringr)
library(tidyverse)
library(dplyr)
library(ggplot2) 
library(lubridate)
library(textclean)
library(bubbles)
library(hrbrthemes)
library(gridExtra)
library(reshape2) 
library(scales)
library(gghighlight)
library(wordcloud)
library(tm)
library(tidytext)
library(plyr) 



#---------------



### Import Functions ###

## Frequency Table
# We will use this function to create frequency table
# It takes as parameters:  
# - column = table$column or variable (e.g. year) format
# - order = TRUE for decreasing order and FALSE for increasing order
# The function returns an ordered table of frequencies in Dataframe format

frequency_table <- function(column, order){
  
  frequency_table <- table(column)
  frequency_table <- as.data.frame(frequency_table)
  frequency_table <- frequency_table[order(frequency_table$Freq, decreasing = order),]
  frequency_table
}



## Status Analysis
# We will use this function to analyze the mean and median for each Enron employee status
# It takes as parameters: 
# - DataFrame: the dataframe we need to calculate the mean and median
# - status : the name of the status
# This function return a DataFrame containing the status, the mean and the median

status_analysis <- function(dataframe, status){
  
  # Create an empty DataFrame
  status_analysis_df = data.frame()
  # Add the status
  status_analysis_df[1, 'Status'] <- status
  # Add the Median and the Mean
  status_analysis_df['Median'] <- ceiling(median(dataframe[dataframe$Status == status, 'Emails']))
  status_analysis_df['Mean'] <- ceiling(mean(dataframe[dataframe$Status == status, 'Emails']))
  status_analysis_df <- as.data.frame(status_analysis_df)
  
}



#---------------



### Import Data ###

employee_df <- employeelist
message_df <- message
recipient_df <- recipientinfo



#---------------



### Clean our 3 DataFrames ###



## Part 1 - Employee DataFrame ##

conditions <- c('d..baughman.com', 'matthew.smith?@enron.com', 'scott . hendrikcson@enron.com')
replace_values <- c('d.baughman@enron.com', 'matthew.smith@enron.com', 'scott.hendrikcson@enron.com')
# Replace missing value
employee_df$status[is.na(employee_df$status)] <- 'N/A'
# Remove the blank spaces
gsub(" ", "", employee_df$Email_id, , fixed=TRUE)
# Replace the emails using conditions
employee_df$Email2 <- replace(employee_df$Email2, employee_df$Email2 %in% conditions, replace_values)
# Replace the double dots with a dot
employee_df$Email_id <- gsub('..', '.', employee_df$Email_id, fixed=TRUE)
employee_df$Email2 <- gsub('..', '.', employee_df$Email2, fixed=TRUE)
employee_df$Email3 <- gsub('..', '.', employee_df$Email3, fixed=TRUE)
employee_df$EMail4 <- gsub('..', '.', employee_df$EMail4, fixed=TRUE)



## Part 2 - Message DataFrame ##

# Create 3 columns for the year, the month and the day
message_df <- message_df %>% 
  mutate(Year = year(message_df$date)) %>% 
  mutate(Month = month(message_df$date)) %>%
  mutate(Day = day(message_df$date))
# Transform 1 into 2021 and 2 into 2002
message_df$Year[message_df$Year == 1] <- 2001
message_df$Year[message_df$Year == 2] <- 2002
# Remove the first date column
message_df$date <- NULL
# Create a new date column and convert it in a date format
message_df$date <- paste(message_df$Year, message_df$Month, message_df$Day, sep = "-")
message_df$date <- as.Date(message_df$date , format = "%Y-%m-%d")
#Remove the row where the date is 1979, 2043 and 2044
message_df <- message_df[!(message_df$date > "2021-01-01" | message_df$date < "1980-01-01"), ]



## Part 3 - Recipient Info DataFrame ##
# Remove all spaces inside words
recipient_df$rvalue <- gsub(" ", "", recipient_df$rvalue, , fixed=TRUE)
# Replace all the double dots by a dot
recipient_df$rvalue <- gsub('..', '.', recipient_df$rvalue, fixed=TRUE)
# Replace special characters
recipient_df$rvalue <- mgsub(recipient_df$rvalue, c('_.', '._', '-.', '.-'), c('_', '_', '.', '-'),
                             fixed=TRUE)



#---------------



### Create a single Enron DataFrame ### 



## Part 1 - Prepare our 3 DataFrame ##

# Select Columns
list_variable_message <- c('mid', 'sender', 'subject', 'Year', 'Month', 'Day', 'date')
list_variable_employee <- c('eid', 'firstName', 'lastName', 'Email_id', 'Email2', 'Email3', 'EMail4', 'status')
message_df_2 <- message_df[ , list_variable_message]
employee_df_2 <- employee_df[ , list_variable_employee]
recipient_df_2 <- recipient_df

# Rename columns
message_df_2 <- message_df_2 %>% 
  dplyr::rename('MessageID' = 'mid',
         'SenderEmail' = 'sender' ,
         'Subject' = 'subject', 
         'DateYear' = 'Year', 
         'DateMonth'= 'Month', 
         'DateDay'= 'Day', 
         'Date' = 'date')
recipient_df_2 <- recipient_df_2 %>% 
  dplyr::rename('MessageID' = 'mid',
         'RecipientID' = 'rid', 
         'RecipientType' = 'rtype', 
         'RecipientEmail' = 'rvalue')

# Create new columns
employee_df_2$EmployeeName <- paste(employee_df_2$firstName, employee_df_2$lastName, sep = " ")
employee_df_2 <- employee_df_2[,!names(employee_df_2) %in% c('firstName', 'lastName', 'folder')]
employee_df_2 <- employee_df_2 %>% 
  dplyr::rename('EmployeeID' = 'eid',
         'Email1' = 'Email_id', 
         'Email4' = 'EMail4', 
         'Status' = 'status')



## Part 2 - Join the Message DataFrame and the Employee DataFrame ##

# Join for the Email1
df_join_1 <- inner_join(message_df_2, employee_df_2, by = c('SenderEmail' = 'Email1'))

# Retrieve all message ID that correspond to an employee's e-mail address
list_message_id_1 <- df_join_1[, 'MessageID']

# Create a new DataFrame with the remaining rows
remaining_rows <- message_df_2[!message_df_2$MessageID %in% list_message_id_1, ]

# Join a new times the new DataFrame with the Email2 of the Employee DataFrame
df_join_2 <- inner_join(remaining_rows, employee_df_2, by = c('SenderEmail' = 'Email2'))

# Repeat the process for the Email3 of the Employee DataFrame
list_recipient_id <- df_join_2[, 'MessageID']
list_message_id_2 <- c(list_message_id_1, list_recipient_id)
remaining_rows <- message_df_2[!message_df_2$MessageID %in% list_message_id_2, ]
df_join_3 <- inner_join(remaining_rows, employee_df_2, by = c('SenderEmail' = 'Email3'))

# Repeat the process for the Email4 of the Employee DataFrame
list_recipient_id <- df_join_3[, 'MessageID']
list_message_id_3 <- c(list_message_id_2, list_recipient_id)
remaining_rows <- message_df_2[!message_df_2$MessageID %in% list_message_id_3, ]
df_join_4 <- inner_join(remaining_rows, employee_df_2, by = c('SenderEmail' = 'Email4'))

# Repeat the same process to retrieve the remaining rows that are not joined during the process 
list_recipient_id <- df_join_4[, 'MessageID']
list_message_id_4 <- c(list_message_id_3, list_recipient_id)
remaining_df <- message_df_2[!message_df_2$MessageID %in% list_message_id_4, ]

# Group the 5 DataFrames together
enron_df <- df_join_1 %>%
  bind_rows(df_join_2) %>%
  bind_rows(df_join_3) %>%
  bind_rows(df_join_4) %>%
  bind_rows(remaining_df)

# Organize the Enron DataFrame
enron_df <- enron_df[,!names(enron_df) %in% c('Email1', 'Email2', 'Email3', 'Email4')]
enron_df <- enron_df %>% replace(is.na(.), 'N/A')
enron_df <- enron_df %>% 
  dplyr::rename('SenderEmployeeID' = 'EmployeeID',
         'SenderStatus' = 'Status', 
         'SenderName' = 'EmployeeName')



## Part - 3 - Join the DataFrames

# Join the new Eron DataFrame and the Recipient Table
enron_df <- enron_df %>% left_join(recipient_df_2, 
                                   by='MessageID')

# Join the new Eron DataFrame and the Employee DataFrame

# Join for the Email1
df_join_1 <- inner_join(enron_df, employee_df_2, by = c('RecipientEmail' = 'Email1'))

# Retrieve all recipient ID that are joined
list_recipient_id_1 <- df_join_1[, 'RecipientID']

# Create a new DataFrame with the remaining rows (not joined)
remaining_rows <- enron_df[!enron_df$RecipientID %in% list_recipient_id_1, ]

# Join a new times the remaining DataFrame with the Email2 of the Employee DataFrame
df_join_2 <- inner_join(remaining_rows, employee_df_2, by = c('RecipientEmail' = 'Email2'))

# Repeat the process for the Email3 of the Employee DataFrame
list_recipient_id <- df_join_2[, 'RecipientID']
list_recipient_id_2 <- c(list_recipient_id_1, list_recipient_id)
remaining_rows <- enron_df[!enron_df$RecipientID %in% list_recipient_id_2, ]
df_join_3 <- inner_join(remaining_rows, employee_df_2, by = c('RecipientEmail' = 'Email3'))

# Repeat the process for the Email4 of the Employee DataFrame
list_recipient_id <- df_join_3[, 'RecipientID']
list_recipient_id_3 <- c(list_recipient_id_2, list_recipient_id)
remaining_rows <- enron_df[!enron_df$RecipientID %in% list_recipient_id_3, ]
df_join_4 <- inner_join(remaining_rows, employee_df_2, by = c('RecipientEmail' = 'Email4'))

# Repeat the same process to retrieve the remaining rows that are not joined during the process
list_recipient_id <- df_join_4[, 'RecipientID']
list_message_id_4 <- c(list_recipient_id_3, list_recipient_id)
remaining_df <- enron_df[!enron_df$RecipientID %in% list_message_id_4, ]
# Group the 5 DataFrames together
enron_df <- bind_rows(df_join_1, df_join_2, df_join_3, df_join_4, remaining_df)



## part 4 - Clean the Enron DataFrame ##

# Remove the e-mails columns
enron_df <- enron_df[,!names(enron_df) %in% c('Email1', 'Email2', 'Email3', 'Email4')]

# Rename some specific columns for recipient employee information
enron_df <- enron_df %>% 
  dplyr::rename('RecipientName' = 'EmployeeName',
         'RecipientStatus' = 'Status', 
         'RecipientEmployeeID' = 'EmployeeID')
# Replace missing values in the columns Employee ID, Status and EmployeeName
enron_df <- enron_df %>% replace(is.na(.), 'N/A')
col_order <- c('MessageID', 'Date', 'DateYear', 'DateMonth', 'DateDay', 'Subject',
               'SenderEmail', 'SenderName', 'SenderStatus', 'SenderEmployeeID',
               'RecipientID', 'RecipientType', 'RecipientEmail', 'RecipientName',
               'RecipientStatus', 'RecipientEmployeeID')

enron_df <- enron_df[, col_order]



#---------------



### Analysis - 1 - Who is Enron's most active employee? ###



## Part 1 - Sender Most Active Employee DataFrame ##

# Create a DataFrame with only the Sender category
enron_df_sender <- enron_df[enron_df$SenderEmployeeID != 'N/A', ]

# Keep only the unique Message ID
enron_df_sender <- enron_df_sender %>% distinct(MessageID, .keep_all = TRUE)

# Create a frequency table for the Sender Category
frequency_table_sender = frequency_table(enron_df_sender$SenderName, 'TRUE')
frequency_table_sender <- frequency_table_sender %>%
  dplyr::rename('EmployeeName' = 'column',
         'Emails' = 'Freq')

# -> The frequency_table_sender will be imported into the server file



## Part 2 - Recipient Most Active Employee DataFrame ##

# Create a DataFrame with only the Recipient category
enron_df_recipient <- enron_df[enron_df$RecipientEmployeeID != 'N/A', ]

# Create a frequency table for the Recipient Category
frequency_table_recipient = frequency_table(enron_df_recipient$RecipientName, 'TRUE')
frequency_table_recipient <- frequency_table_recipient %>%
  dplyr::rename('EmployeeName' = 'column',
         'Emails' = 'Freq')

# -> The frequency_table_recipient will be imported into the server file



## Part 3 - Overall Most Active Employee DataFrame ##

# Create a DataFrame with only the Overall category
# Concatenate the DataFrames (Sender and Recipient)
most_active_employee_df <- bind_rows(frequency_table_sender, frequency_table_recipient)

# Merge every row with the same employee to have a total
most_active_employee_df <- most_active_employee_df %>%
  group_by(EmployeeName) %>%
  dplyr::summarise(Emails=sum(Emails))
most_active_employee_df <- most_active_employee_df %>% as.data.frame()
most_active_employee_df <- most_active_employee_df[order(most_active_employee_df$Emails, decreasing = TRUE),]

# -> The most_active_employee_df will be imported into the server file



#---------------



### Analysis - 2 - In which status are we most likely to be active, in terms of sending and receiving e-mail? ###



## Part 1 - Create a DataFrame for the Global Overall Activity (Sender + Recipient) ##

# Join the Most Active Employee DataFrame with the Employee Table
status_activity_df <- most_active_employee_df %>% left_join(employee_df_2, 
                                                            by='EmployeeName')
# Keep the 2 most important columns. We don't need the Employee Name anymore
columns <- c('Emails', 'Status')
status_activity_df <- status_activity_df[columns]
status_activity_df <- status_activity_df[order(status_activity_df$Emails, decreasing = TRUE), ]
# Group every status to have the total of the activity
status_activity_df <- status_activity_df %>%
  group_by(Status) %>%
  dplyr::summarise(Emails=sum(Emails))

#Create a DataFrame to have the number of people per status and the global activity
number_status_df <- employee_df_2[,'Status']
number_status_df <- frequency_table(number_status_df, 'TRUE')
number_status_df <- number_status_df %>%
  dplyr::rename('Status' = 'column',
         'People' = 'Freq')
status_people_activity_df <- number_status_df %>% left_join(status_activity_df, 
                                                            by='Status')

# Create 3 specific DataFrames per category with the Employee Name, his status and his number of e-mails
# For the Sender category 
columns_list <- c('EmployeeName', 'Emails', 'Status')
analysis_sender_df <- frequency_table_sender %>% left_join(employee_df_2, 
                                                           by='EmployeeName')
analysis_sender_df <- analysis_sender_df[,columns_list]

# For the Recipient category 
analysis_recipient_df <- frequency_table_recipient %>% left_join(employee_df_2, 
                                                                 by='EmployeeName')
analysis_recipient_df <- analysis_recipient_df[,columns_list]
# For the Overall category 
analysis_overall_df <- most_active_employee_df %>% left_join(employee_df_2, 
                                                             by='EmployeeName')
analysis_overall_df <- analysis_overall_df[,columns_list]



## Part 2 - Create 3 specific DataFrames per category to compare the global activity, the median and the mean ##

# For the Sender Category
# Create an Empty DataFrame 
status_list <- as.list(unique(employee_df_2['Status']))
status_sender_df <- data.frame()

# Iterate over the list of status calling ou function to create our Dataframe
for (status in status_list[[1]]) {
  # We call our Status Analysis function to have a DataFrame with the mean and the median
  status_analysis_sender_df <- status_analysis(analysis_sender_df, status)
  # We concatenate this 2 DataFrame
  status_sender_df <- bind_rows(status_sender_df, status_analysis_sender_df)  
}

# Add a column with the total of e-mail sent
analysis_sender_df_2 <- analysis_sender_df[,!names(analysis_sender_df) == 'EmployeeName']
analysis_sender_df_2 <- analysis_sender_df_2 %>%
  group_by(Status) %>%
  dplyr::summarise(Emails=sum(Emails))
status_sender_df <- analysis_sender_df_2 %>% left_join(status_sender_df, 
                                                       by='Status') 
status_sender_df <- status_sender_df %>% 
  dplyr::rename('TotalEmails' = 'Emails')

# Add the column of the number of people in the status
status_sender_df <- status_people_activity_df %>% left_join(status_sender_df, 
                                                            by='Status')
status_sender_df <- status_sender_df[,!names(status_sender_df) == 'Emails']
status_sender_df <- status_sender_df %>% 
  relocate(Median) %>%
  relocate(Status)
status_sender_df <- status_sender_df[order(status_sender_df$Median, decreasing = TRUE),]

# -> The status_sender_df will be imported into the server file



# For the Recipient Category
# Create an Empty DataFrame 
status_list <- as.list(unique(employee_df_2['Status']))
status_recipient_df <- data.frame()

# Iterate over ou list of status calling ou function to create our Dataframe
for (status in status_list[[1]]) {
  # We call our Status Analysis function to have a DataFrame with the mean and the median
  status_analysis_recipient_df <- status_analysis(analysis_recipient_df, status)
  # We concatenate this 2 DataFrame
  status_recipient_df <- bind_rows(status_recipient_df, status_analysis_recipient_df)  
}

# Add a column with the total of e-mail sent
analysis_recipient_df_2 <- analysis_recipient_df[,!names(analysis_recipient_df) == 'EmployeeName']
analysis_recipient_df_2 <- analysis_recipient_df_2 %>%
  group_by(Status) %>%
  dplyr::summarise(Emails=sum(Emails))
status_recipient_df <- analysis_recipient_df_2 %>% left_join(status_recipient_df, 
                                                             by='Status') 
status_recipient_df <- status_recipient_df %>% 
  dplyr::rename('TotalEmails' = 'Emails')

# Add the column of the number of people in the status
status_recipient_df <- status_people_activity_df %>% left_join(status_recipient_df, 
                                                               by='Status')
status_recipient_df <- status_recipient_df[,!names(status_recipient_df) == 'Emails']
status_recipient_df <- status_recipient_df %>% 
  relocate(Median) %>%
  relocate(Status)
status_recipient_df <- status_recipient_df[order(status_recipient_df$Median, decreasing = TRUE),]

# -> The status_recipient_df will be imported into the server file



# For the Overall Category
# Create an Empty DataFrame 
status_list <- as.list(unique(employee_df_2['Status']))
status_overall_df <- data.frame()

# Iterate over ou list of status calling ou function to create our Dataframe
for (status in status_list[[1]]) {
  # We call our Status Analysis function to have a DataFrame with the mean and the median
  status_analysis_overall_df <- status_analysis(analysis_overall_df, status)
  # We concatenate this 2 DataFrame
  status_overall_df <- bind_rows(status_overall_df, status_analysis_overall_df)  
}

# Add a column with the total of e-mail sent
analysis_overall_df_2 <- analysis_overall_df[,!names(analysis_overall_df) == 'EmployeeName']
analysis_overall_df_2 <- analysis_overall_df_2 %>%
  group_by(Status) %>%
  dplyr::summarise(Emails=sum(Emails))
status_overall_df <- analysis_overall_df_2 %>% left_join(status_overall_df, 
                                                         by='Status') 
status_overall_df <- status_overall_df %>% 
  dplyr::rename('TotalEmails' = 'Emails')

# And add the column of the number of people in the status
status_overall_df <- status_people_activity_df %>% left_join(status_overall_df, 
                                                             by='Status')
status_overall_df <- status_overall_df[,!names(status_overall_df) == 'Emails']
status_overall_df <- status_overall_df %>% 
  relocate(Median) %>%
  relocate(Status)
status_overall_df <- status_overall_df[order(status_overall_df$Median, decreasing = TRUE),]

# -> The status_overall_df will be imported into the server file



#---------------



### Analysis - 3 - When was e-mail activity highest? And why? ###



## Part 1 - Annual Part ##

# Create a DataFrame for each category with the number of e-mails according to the year and month
# For the Sender Category
enron_df_annual_sender_3 <- enron_df %>% distinct(MessageID, .keep_all = TRUE)
enron_df_annual_sender_3 <- enron_df_annual_sender_3 %>% select('Date')

# Extract Year and Month
enron_df_annual_sender_3$MonthYear <- format(as.Date(enron_df_annual_sender_3$Date), '%Y-%m')

# Group by Year and Month
enron_df_annual_sender_3 <- enron_df_annual_sender_3 %>% group_by(MonthYear) %>% 
  dplyr::summarise(EmailNumber=n(),.groups = 'drop') %>% 
  as.data.frame()

# Add the day 1 to have a date format
enron_df_annual_sender_3$Date <- as.Date(paste(enron_df_annual_sender_3$MonthYear, "-01", sep=""))
enron_df_annual_sender_3 <- enron_df_annual_sender_3[,!names(enron_df_annual_sender_3) == 'MonthYear']
# -> The enron_df_annual_sender_3 will be imported into the server file

# For the Overall Category
# Create a DataFrame with the column Date
enron_df_annual_overall_3 <- enron_df %>% select('Date')

# Extract Year and Month
enron_df_annual_overall_3$MonthYear <- format(as.Date(enron_df_annual_overall_3$Date), '%Y-%m')

# Group by Year and Month
enron_df_annual_overall_3 <- enron_df_annual_overall_3 %>% group_by(MonthYear) %>% 
  dplyr::summarise(EmailNumber=n(),.groups = 'drop') %>% 
  as.data.frame()

# Add the day 1 to have a date format
enron_df_annual_overall_3$Date <- as.Date(paste(enron_df_annual_overall_3$MonthYear, "-01", sep=""))
enron_df_annual_overall_3 <- enron_df_annual_overall_3[,!names(enron_df_annual_overall_3) == 'MonthYear']
# -> The enron_df_annual_overall_3 will be imported into the server file



## Part 2 - Monthly Part ##

# Create a DataFrame with the abbreviation of the month in letters, the month number, the year and the number of e-mails

# For the Sender Category 
enron_df_monthly_sender_3 <- enron_df_annual_sender_3
# Extract the Year
enron_df_monthly_sender_3$Years <- format(enron_df_monthly_sender_3$Date, format="%Y")
# Extract the month in a numerical format
enron_df_monthly_sender_3$MonthNumber <- as.numeric(format(as.Date(enron_df_monthly_sender_3$Date),"%m"))
# Create an abreviation of the Months
enron_df_monthly_sender_3$Date  <- months(as.Date(enron_df_monthly_sender_3$Date), abbreviate=TRUE) 
enron_df_monthly_sender_3 <- enron_df_monthly_sender_3[enron_df_monthly_sender_3$Years > 1998 & enron_df_monthly_sender_3$Years < 2003, ]
# -> The enron_df_monthly_sender_3 will be imported into the server file

# For the Overall Category
enron_df_monthly_overall_3 <-enron_df_annual_overall_3
# Extract the Year
enron_df_monthly_overall_3$Years <- format(enron_df_monthly_overall_3$Date, format="%Y")
# Extract the month in a numerical format
enron_df_monthly_overall_3$MonthNumber <- as.numeric(format(as.Date(enron_df_monthly_overall_3$Date),"%m"))
# Create an abreviation of the Months
enron_df_monthly_overall_3$Date  <- months(as.Date(enron_df_monthly_overall_3$Date), abbreviate=TRUE) 
enron_df_monthly_overall_3 <- enron_df_monthly_overall_3[enron_df_monthly_overall_3$Years > 1998 & enron_df_monthly_overall_3$Years < 2003, ]
# -> The enron_df_monthly_overall_3 will be imported into the server file



#---------------



### Analysis - 4 - What were the Enron employees talking about? ###



## Part 1 - General Overview Part ## 

# Corpus for the Wordcloud 

# List of words to delete for wordcloud
list_words_4_1 <- c('new', 'date', 'hourahead', 'update', 'daily', 'weekly', 'today', 'start', 'revised', 'fwd', 
                   'fw', 'master', 'inc', 'lay', 'list', 'call', 'news', 'access','change', 'informationsda', 
                   'please', 'company', 'demand', 'codesite', 'management', 'hour', 'may', 'schedule', 'capacity', 
                   'summary', 'week', 'notification', 'group', 'office', 'information', 'plan', 'final', 'corp', 
                   'weekend', 'email', 'price', 'data', 'services', 'review', 'infos','status', 'day', 'info', 
                   'confirmation', 'notice', 'free', 'response', 'proceeds', 'time', 'online', 'reminder', 'memo', 
                   'document', 'donate', 'contact', 'message', 'action', 'project', 'updated', 'options', 'staff', 
                   'release', 'global', 'comments', 'program', 'mentions', 'changes', 'natural', 'tw', 'mtg', 'business',
                   'pg', 'ofo', 'watch', 'center', 'lsda')

# Corpus for 1999
enron_df_year_1999_4_1 <- enron_df[enron_df$DateYear == 1999, ]
words_analysis_1999_4_1 <- enron_df_year_1999_4_1$Subject
# Transform the text into a “corpus” class, enabling us to apply certain functions
enron_wordcloud_corpus_1999_4_1 <- Corpus(VectorSource(words_analysis_1999_4_1))
# Clean the data: remove punctuation, lowercase, remove Numbers, remove "Stopwords"
enron_wordcloud_corpus_1999_4_1 <- enron_wordcloud_corpus_1999_4_1 %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
enron_wordcloud_corpus_1999_4_1 <- tm_map(enron_wordcloud_corpus_1999_4_1, content_transformer(tolower))
enron_wordcloud_corpus_1999_4_1 <- tm_map(enron_wordcloud_corpus_1999_4_1, removeWords, stopwords("english"))
enron_wordcloud_corpus_1999_4_1 <- tm_map(enron_wordcloud_corpus_1999_4_1, function(x)removeWords(x,list_words_4_1))

# -> The enron_wordcloud_corpus_1999_4_1 corpus will be imported into the server file

# Corpus for 2000
enron_df_year_2000_4_1 <- enron_df[enron_df$DateYear == 2000, ]
words_analysis_2000_4_1 <- enron_df_year_2000_4_1$Subject
enron_wordcloud_corpus_2000_4_1 <- Corpus(VectorSource(words_analysis_2000_4_1))
enron_wordcloud_corpus_2000_4_1 <- enron_wordcloud_corpus_2000_4_1 %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
enron_wordcloud_corpus_2000_4_1 <- tm_map(enron_wordcloud_corpus_2000_4_1, content_transformer(tolower))
enron_wordcloud_corpus_2000_4_1 <- tm_map(enron_wordcloud_corpus_2000_4_1, removeWords, stopwords("english"))
enron_wordcloud_corpus_2000_4_1 <- tm_map(enron_wordcloud_corpus_2000_4_1, function(x)removeWords(x,list_words_4_1))

# -> The enron_wordcloud_corpus_2000_4_1 corpus will be imported into the server file

# Corpus for 2001
enron_df_year_2001_4_1 <- enron_df[enron_df$DateYear == 2001, ]
words_analysis_2001_4_1 <- enron_df_year_2001_4_1$Subject
enron_wordcloud_corpus_2001_4_1 <- Corpus(VectorSource(words_analysis_2001_4_1))
enron_wordcloud_corpus_2001_4_1 <- enron_wordcloud_corpus_2001_4_1 %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
enron_wordcloud_corpus_2001_4_1 <- tm_map(enron_wordcloud_corpus_2001_4_1, content_transformer(tolower))
enron_wordcloud_corpus_2001_4_1 <- tm_map(enron_wordcloud_corpus_2001_4_1, removeWords, stopwords("english"))
enron_wordcloud_corpus_2001_4_1 <- tm_map(enron_wordcloud_corpus_2001_4_1, function(x)removeWords(x,list_words_4_1))

# -> The enron_wordcloud_corpus_2001_4_1 corpus will be imported into the server file


# Corpus for 2002
enron_df_year_2002_4_1 <- enron_df[enron_df$DateYear == 2002, ]
words_analysis_2002_4_1 <- enron_df_year_2002_4_1$Subject
enron_wordcloud_corpus_2002_4_1 <- Corpus(VectorSource(words_analysis_2002_4_1))
enron_wordcloud_corpus_2002_4_1 <- enron_wordcloud_corpus_2002_4_1 %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
enron_wordcloud_corpus_2002_4_1 <- tm_map(enron_wordcloud_corpus_2002_4_1, content_transformer(tolower))
enron_wordcloud_corpus_2002_4_1 <- tm_map(enron_wordcloud_corpus_2002_4_1, removeWords, stopwords("english"))
enron_wordcloud_corpus_2002_4_1 <- tm_map(enron_wordcloud_corpus_2002_4_1, function(x)removeWords(x,list_words_4_1))

# -> The enron_wordcloud_corpus_2002_4_1 corpus will be imported into the server file


# Corpus for the entire period
words_analysis_all_years_4_1 <- enron_df$Subject
enron_wordcloud_corpus_all_years_4_1 <- Corpus(VectorSource(words_analysis_all_years_4_1))
enron_wordcloud_corpus_all_years_4_1 <- enron_wordcloud_corpus_all_years_4_1 %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
enron_wordcloud_corpus_all_years_4_1 <- tm_map(enron_wordcloud_corpus_all_years_4_1, content_transformer(tolower))
enron_wordcloud_corpus_all_years_4_1 <- tm_map(enron_wordcloud_corpus_all_years_4_1, removeWords, stopwords("english"))
enron_wordcloud_corpus_all_years_4_1 <- tm_map(enron_wordcloud_corpus_all_years_4_1, function(x)removeWords(x,list_words_4_1))

# -> The enron_wordcloud_corpus_all_years_4_1 corpus will be imported into the server file


# DataFrames for the Bar Chart

# DataFrame for 1999
# Create a tibble that is near to a data frame
enron_df_barchart_1999_4_1 <- tibble(Text = words_analysis_1999_4_1)
# Create a tibble with one row by word
enron_df_barchart_1999_4_1 <- enron_df_barchart_1999_4_1 %>% 
  unnest_tokens(output = word, input = Text) 
# Remove stop words
enron_df_barchart_1999_4_1 <- enron_df_barchart_1999_4_1 %>%
  anti_join(stop_words) 
# Create a tibble with each word associated with the number of times it has been used
enron_df_barchart_1999_4_1 <- enron_df_barchart_1999_4_1 %>% dplyr::count(word, sort = TRUE)
# Rename columns 
enron_df_barchart_1999_4_1 <- enron_df_barchart_1999_4_1 %>% 
  dplyr::rename('Words' = 'word', 
                'Frequency' = 'n')
# Continue to clean the data by deleting the numeric values 
enron_df_barchart_1999_4_1$Words <- gsub('[[:digit:]]+', '', enron_df_barchart_1999_4_1$Words)
enron_df_barchart_1999_4_1$Words <- str_replace_all(enron_df_barchart_1999_4_1[['Words']], "[[:punct:]]", "")
enron_df_barchart_1999_4_1 <- enron_df_barchart_1999_4_1[enron_df_barchart_1999_4_1$Words != '',]
enron_df_barchart_1999_4_1 <- enron_df_barchart_1999_4_1[!(enron_df_barchart_1999_4_1$Words %in% list_words_4_1),]

# -> The enron_df_barchart_1999_4_1 DataFrame will be imported into the server file

# DataFrame for 2000
enron_df_barchart_2000_4_1 <- tibble(Text = words_analysis_2000_4_1)
enron_df_barchart_2000_4_1 <- enron_df_barchart_2000_4_1 %>% 
  unnest_tokens(output = word, input = Text) 
enron_df_barchart_2000_4_1 <- enron_df_barchart_2000_4_1 %>%
  anti_join(stop_words) 
enron_df_barchart_2000_4_1 <- enron_df_barchart_2000_4_1 %>% dplyr::count(word, sort = TRUE)
enron_df_barchart_2000_4_1 <- enron_df_barchart_2000_4_1 %>% 
  dplyr::rename('Words' = 'word', 
                'Frequency' = 'n')
enron_df_barchart_2000_4_1$Words <- gsub('[[:digit:]]+', '', enron_df_barchart_2000_4_1$Words)
enron_df_barchart_2000_4_1$Words <- str_replace_all(enron_df_barchart_2000_4_1[['Words']], "[[:punct:]]", "")
enron_df_barchart_2000_4_1 <- enron_df_barchart_2000_4_1[enron_df_barchart_2000_4_1$Words != '',]
enron_df_barchart_2000_4_1 <- enron_df_barchart_2000_4_1[!(enron_df_barchart_2000_4_1$Words %in% list_words_4_1),]

# -> The enron_df_barchart_2000_4_1 DataFrame will be imported into the server file

# DataFrame for 2001
enron_df_barchart_2001_4_1 <- tibble(Text = words_analysis_2001_4_1)
enron_df_barchart_2001_4_1 <- enron_df_barchart_2001_4_1 %>% 
  unnest_tokens(output = word, input = Text) 
enron_df_barchart_2001_4_1 <- enron_df_barchart_2001_4_1 %>%
  anti_join(stop_words) 
enron_df_barchart_2001_4_1 <- enron_df_barchart_2001_4_1 %>% dplyr::count(word, sort = TRUE)
enron_df_barchart_2001_4_1 <- enron_df_barchart_2001_4_1 %>% 
  dplyr::rename('Words' = 'word', 
                'Frequency' = 'n')
enron_df_barchart_2001_4_1$Words <- gsub('[[:digit:]]+', '', enron_df_barchart_2001_4_1$Words)
enron_df_barchart_2001_4_1$Words <- str_replace_all(enron_df_barchart_2001_4_1[['Words']], "[[:punct:]]", "")
enron_df_barchart_2001_4_1 <- enron_df_barchart_2001_4_1[enron_df_barchart_2001_4_1$Words != '',]
enron_df_barchart_2001_4_1 <- enron_df_barchart_2001_4_1[!(enron_df_barchart_2001_4_1$Words %in% list_words_4_1),]

# -> The enron_df_barchart_2001_4_1 DataFrame will be imported into the server file


# DataFrame for 2002
enron_df_barchart_2002_4_1 <- tibble(Text = words_analysis_2002_4_1)
enron_df_barchart_2002_4_1 <- enron_df_barchart_2002_4_1 %>% 
  unnest_tokens(output = word, input = Text) 
enron_df_barchart_2002_4_1 <- enron_df_barchart_2002_4_1 %>%
  anti_join(stop_words) 
enron_df_barchart_2002_4_1 <- enron_df_barchart_2002_4_1 %>% dplyr::count(word, sort = TRUE)
enron_df_barchart_2002_4_1 <- enron_df_barchart_2002_4_1 %>% 
  dplyr::rename('Words' = 'word', 
                'Frequency' = 'n')
enron_df_barchart_2002_4_1$Words <- gsub('[[:digit:]]+', '', enron_df_barchart_2002_4_1$Words)
enron_df_barchart_2002_4_1$Words <- str_replace_all(enron_df_barchart_2002_4_1[['Words']], "[[:punct:]]", "")
enron_df_barchart_2002_4_1 <- enron_df_barchart_2002_4_1[enron_df_barchart_2002_4_1$Words != '',]
enron_df_barchart_2002_4_1 <- enron_df_barchart_2002_4_1[!(enron_df_barchart_2002_4_1$Words %in% list_words_4_1),]

# -> The enron_df_barchart_2002_4_1 DataFrame will be imported into the server file


# DataFrame for the entire period
enron_df_barchart_all_years_4_1 <- tibble(Text = words_analysis_all_years_4_1)
enron_df_barchart_all_years_4_1 <- enron_df_barchart_all_years_4_1 %>% 
  unnest_tokens(output = word, input = Text) 
enron_df_barchart_all_years_4_1 <- enron_df_barchart_all_years_4_1 %>%
  anti_join(stop_words) 
enron_df_barchart_all_years_4_1 <- enron_df_barchart_all_years_4_1 %>% dplyr::count(word, sort = TRUE)
enron_df_barchart_all_years_4_1 <- enron_df_barchart_all_years_4_1 %>% 
  dplyr::rename('Words' = 'word', 
                'Frequency' = 'n')
enron_df_barchart_all_years_4_1$Words <- gsub('[[:digit:]]+', '', enron_df_barchart_all_years_4_1$Words)
enron_df_barchart_all_years_4_1$Words <- str_replace_all(enron_df_barchart_all_years_4_1[['Words']], "[[:punct:]]", "")
enron_df_barchart_all_years_4_1 <- enron_df_barchart_all_years_4_1[enron_df_barchart_all_years_4_1$Words != '',]
enron_df_barchart_all_years_4_1 <- enron_df_barchart_all_years_4_1[!(enron_df_barchart_all_years_4_1$Words %in% list_words_4_1),]

# -> The enron_df_barchart_all_years_4_1 DataFrame will be imported into the server file



## Part 2 - Comparison Part ## 



# DataFrame for the Bar Chart and Heat Map

# Preparation stage for our final DataFrames
# We create 3 Dataframe for each year
words_count_00 <- message_df_2[message_df_2$DateYear == 2000, ]
words_count_01 <- message_df_2[message_df_2$DateYear == 2001, ]
words_count_02 <- message_df_2[message_df_2$DateYear == 2002, ]

# Create a DataFrame with the words and the number of times they appear
# We follow the same process for each year

# Year 2000
words_count_00 <- words_count_00$Subject
words_count_00_df <- tibble(Text = words_count_00)
words_count_00_df <- words_count_00_df %>% 
  unnest_tokens(output = word, input = Text) 
words_count_00_df <- words_count_00_df %>%
  anti_join(stop_words) 
words_count_00_df <- words_count_00_df %>% dplyr::count(word, sort = TRUE)
words_count_00_df <- words_count_00_df %>% 
  dplyr::rename('Words' = 'word', 
                'Year2000' = 'n')
words_count_00_df$Words <- gsub('[[:digit:]]+', '', words_count_00_df$Words)
words_count_00_df <- words_count_00_df[words_count_00_df$Words != '',]
words_count_00_df <- words_count_00_df[!(words_count_00_df$Words %in% list_words_4_1),]
words_count_00_df <- as.data.frame(apply(words_count_00_df,2, function(x) gsub("\\s+", "", x)))

# Year 2001
words_count_01 <- words_count_01$Subject
words_count_01_df <- tibble(Text = words_count_01)
words_count_01_df <- words_count_01_df %>% 
  unnest_tokens(output = word, input = Text) 
words_count_01_df <- words_count_01_df %>%
  anti_join(stop_words) 
words_count_01_df <- words_count_01_df %>% dplyr::count(word, sort = TRUE)
words_count_01_df <- words_count_01_df %>% 
  dplyr::rename('Words' = 'word', 
                'Year2001' = 'n')
words_count_01_df$Words <- gsub('[[:digit:]]+', '', words_count_01_df$Words)
words_count_01_df <- words_count_01_df[words_count_01_df$Words != '',]
words_count_01_df <- words_count_01_df[!(words_count_01_df$Words %in% list_words_4_1),]
words_count_01_df <- as.data.frame(apply(words_count_01_df,2, function(x) gsub("\\s+", "", x)))

# Year 2002
words_count_02 <- words_count_02$Subject
words_count_02_df <- tibble(Text = words_count_02)
words_count_02_df <- words_count_02_df %>% 
  unnest_tokens(output = word, input = Text) 
words_count_02_df <- words_count_02_df %>%
  anti_join(stop_words) 
words_count_02_df <- words_count_02_df %>% dplyr::count(word, sort = TRUE)
words_count_02_df <- words_count_02_df %>% 
  dplyr::rename('Words' = 'word', 
                'Year2002' = 'n')
words_count_02_df$Words <- gsub('[[:digit:]]+', '', words_count_02_df$Words)
words_count_02_df <- words_count_02_df[words_count_02_df$Words != '',]
words_count_02_df <- words_count_02_df[!(words_count_02_df$Words %in% list_words_4_1),]
words_count_02_df <- as.data.frame(apply(words_count_02_df,2, function(x) gsub("\\s+", "", x)))

# Join the three DafaFrames in one
enron_df_words_comparison_4_2 <- words_count_01_df %>% left_join(words_count_00_df, 
                                                        by='Words')
enron_df_words_comparison_4_2 <- enron_df_words_comparison_4_2 %>% left_join(words_count_02_df, 
                                                           by='Words') %>%
  distinct(Words, Year2000, Year2001, Year2001, .keep_all =TRUE)
# Reorder the final DataFrame
enron_df_words_comparison_4_2 <- enron_df_words_comparison_4_2[, c('Words', 'Year2000', 'Year2001', 'Year2002')]
# Remove the duplicates
enron_df_words_comparison_4_2 = enron_df_words_comparison_4_2[!duplicated(enron_df_words_comparison_4_2$Words),]

# Convert each year column into numeric format
enron_df_words_comparison_4_2$Year2001 <- as.numeric(as.character(enron_df_words_comparison_4_2$Year2001))
enron_df_words_comparison_4_2$Year2000 <- as.numeric(as.character(enron_df_words_comparison_4_2$Year2000))
enron_df_words_comparison_4_2$Year2002 <- as.numeric(as.character(enron_df_words_comparison_4_2$Year2002))
# Order each column starting with 2001, as this is the most important year.
enron_df_words_comparison_4_2 <- enron_df_words_comparison_4_2[order(enron_df_words_comparison_4_2$Year2001,
                                                                     enron_df_words_comparison_4_2$Year2000,
                                                                     enron_df_words_comparison_4_2$Year2002, decreasing=TRUE), ]
# Rename columns + Order DataFrame
enron_df_words_comparison_4_2 <- enron_df_words_comparison_4_2 %>% 
  dplyr::rename('2000' = 'Year2000', 
                '2001' = 'Year2001',
                '2002' = 'Year2002')

# Create 3 Dataframe for the top 5, top 10 and top 15

# For the top 5
# Select the top 5 rows
enron_df_words_comparison_top5_4_2 <- enron_df_words_comparison_4_2[1:5, ]
# Transform the Dataframe into Melt Format
enron_words_comparison_top5_4_2 <- reshape2::melt(enron_df_words_comparison_top5_4_2, id = c('Words'))
enron_words_comparison_top5_4_2$value <- as.numeric(enron_words_comparison_top5_4_2$value)
enron_words_comparison_top5_4_2 <- enron_words_comparison_top5_4_2 %>% 
  dplyr::rename('Years' = 'variable', 
                'Count' = 'value')

# -> The enron_words_comparison_top5_4_2 DataFrame will be imported into the server file


# For the top 10
# Select the top 10 rows
enron_df_words_comparison_top10_4_2 <- enron_df_words_comparison_4_2[1:10, ]
# Transform the Dataframe into Melt Format
enron_words_comparison_top10_4_2 <- reshape2::melt(enron_df_words_comparison_top10_4_2, id = c('Words'))
enron_words_comparison_top10_4_2$value <- as.numeric(enron_words_comparison_top10_4_2$value)
enron_words_comparison_top10_4_2 <- enron_words_comparison_top10_4_2 %>% 
  dplyr::rename('Years' = 'variable', 
                'Count' = 'value')

# -> The enron_words_comparison_top10_4_2 DataFrame will be imported into the server file

# For the top 15
# Select the top 15 rows
enron_df_words_comparison_top15_4_2 <- enron_df_words_comparison_4_2[1:15, ]
# Transform the Dataframe into Melt Format
enron_words_comparison_top15_4_2 <- reshape2::melt(enron_df_words_comparison_top15_4_2, id = c('Words'))
enron_words_comparison_top15_4_2$value <- as.numeric(enron_words_comparison_top15_4_2$value)
enron_words_comparison_top15_4_2 <- enron_words_comparison_top15_4_2 %>% 
  dplyr::rename('Years' = 'variable', 
                'Count' = 'value')

# -> The enron_words_comparison_top15_4_2 DataFrame will be imported into the server file


