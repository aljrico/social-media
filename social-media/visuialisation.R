library(tidyverse)
library(data.table)


list_files = list.files(pattern="*.csv")
df <- tibble()

for(i in 1: length(list_files)){
	tmp <- fread(list_files[[i]])
	platform <- substr(list_files[[i]], 1, nchar(list_files[[i]])-4)
	tmp$platform <- platform
	colnames(tmp) <- c("month", "value", "platform")
	df <- df %>% rbind(tmp)
}

df <- df %>%
	mutate(value = ifelse(value == "<1", 0, (value))) %>%
	mutate(value = as.numeric(value)) %>%
	mutate(month = as.Date(paste(month,"-01",sep="")))



