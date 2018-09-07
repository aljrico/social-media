library(tidyverse)
library(data.table)
library(zoo)
library(scales)

list_files = list.files(pattern="*.csv")
df <- tibble()

for(i in 1: length(list_files)){
	tmp <- fread(list_files[[i]])
	platform <- substr(list_files[[i]], 1, nchar(list_files[[i]])-4)
	tmp$platform <- platform
	colnames(tmp) <- c("month", "value", "platform")
	df <- df %>% rbind(tmp)
}

rm(tmp, i, platform, list_files)

df <- df %>%
	mutate(value = ifelse(value == "<1", 0, (value))) %>%
	mutate(value = as.numeric(value)) %>%
	mutate(month = as.Date(paste(month,"-01",sep=""))) %>%
	group_by(platform) %>%
	mutate(value = rollmean(value, 6, align = "center", fill = NA))


# Plot --------------------------------------------------------------------

colours <- c(
	facebook = "#3B5998",
	instagram = "#8a3ab9",
	myspace = "#313131",
	reddit = "#ff5700",
	snapchat = "#FFFB00",
	twitter = "#55acee"
)

df %>%
	filter(platform != 'tuenti') %>%
	filter(platform != 'fotolog') %>%
	filter(platform != 'whatsapp') %>%
	ggplot(aes(x = month, y = value, colour = platform)) +
	geom_line(size = 1.1) +
	geom_area(aes(fill = platform), alpha = 0.3, position = "identity") +
	theme_test() +
	scale_colour_manual(values = colours, name = "Platform", guide = FALSE) +
	scale_fill_manual(values = colours, name = "Platform") +
	scale_x_date(breaks = date_breaks("years"), labels = date_format("%Y")) +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	ggtitle("Rise and Fall of Social Networks") +
	ylab("Relative Search Interest") +
	xlab("Date")



