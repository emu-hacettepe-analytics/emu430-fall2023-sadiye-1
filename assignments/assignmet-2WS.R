
library(tidyverse) # for everything :)
library(rvest) # for HTML scraping
library(stringr) # for string processing

url_1 <- "https://m.imdb.com/search/title/?title_type=feature&release_date=2010-01-01,2024-01-01&sort=release_date,desc&num_votes=2498,&country_of_origin=TR&count=250"
url_2 <- "https://m.imdb.com/search/title/?title_type=feature&release_date=,2010-01-01&sort=release_date,desc&num_votes=2498,&country_of_origin=TR&count=250"

data_html_1 <- read_html(url_1)
data_html_2 <- read_html(url_2)

title_names_1 <-html_nodes(data_html_1,'.ipc-title__text')
title_names_1 <- html_text(title_names_1)
title_names_1 <- tail(head(title_names_1,-1),-1)
title_names_1 <- str_split(title_names_1, " ", n=2)
title_names_1 <- unlist(lapply(title_names_1, function(x) {x[2]}))

title_names_2 <-html_nodes(data_html_2,'.ipc-title__text')
title_names_2 <- html_text(title_names_2)
title_names_2 <- tail(head(title_names_2,-1),-1)
title_names_2 <- str_split(title_names_2, " ", n=2)
title_names_2 <- unlist(lapply(title_names_2, function(x) {x[2]}))

titles<-c(title_names_1,title_names_2)


year_1 <- html_elements(data_html_1,'.dli-title-metadata > span:nth-child(1)')
year_1 <- html_text(year_1)

year_2 <- html_elements(data_html_2,'.dli-title-metadata > span:nth-child(1)')
year_2 <- html_text(year_2)

year <- as.numeric(c(year_1,year_2))


duration_1 <- html_elements(data_html_1,'.dli-title-metadata > span:nth-child(2)')
duration_1 <- html_text(duration_1)

duration_2 <- html_elements(data_html_2,'.dli-title-metadata > span:nth-child(2)')
duration_2 <- html_text(duration_2)

durations <- c(duration_1,duration_2)

rating_1 <- html_elements(data_html_1,'.ipc-rating-star--imdb')
rating_1 <- html_text(rating_1)
cleaned_rating_1 <- gsub("[^0-9]", "", rating_1)
first_two_digits_1 <- substr(cleaned_rating_1, 1, 2)
numeric_rating_1 <-as.numeric(first_two_digits_1)/10

rating_2 <- html_elements(data_html_2,'.ipc-rating-star--imdb')
rating_2 <- html_text(rating_2)
cleaned_rating_2 <- gsub("[^0-9]", "", rating_2)
first_two_digits_2 <- substr(cleaned_rating_2, 1, 2)
numeric_rating_2 <-as.numeric(first_two_digits_2)/10


rating <- c(numeric_rating_1, numeric_rating_2)

vote_1 <- html_elements(data_html_1,'.kRnqtn')
vote_1 <- html_text(vote_1)
cleaned_vote_1 <- gsub("[^0-9]", "", vote_1)
numeric_vote_1 <- as.numeric(gsub(",", "", cleaned_vote_1))


vote_2 <- html_elements(data_html_2,'.kRnqtn')
vote_2 <- html_text(vote_2)
cleaned_vote_2 <- gsub("[^0-9]", "", vote_2)
numeric_vote_2 <- as.numeric(gsub(",", "", cleaned_vote_2))

votes <- c(numeric_vote_1,numeric_vote_2)

hours <- ifelse(is.na(as.numeric(sub("h.*", "",durations))),0,as.numeric(sub("h.*", "",durations)))



minutes <- sub(".*\\s(\\d+)m", "\\1", durations)
minutes<-ifelse(grepl("h", minutes),0,minutes) 
minutes<-ifelse(grepl("m",minutes),gsub("m", "", minutes),minutes)
minutes <- as.numeric(minutes)

duration_min <-60*hours+minutes

imdb_data_frame <- data.frame(Title=titles,Year=year,Duration=durations, Rating=rating, Votes=votes, DurationMin = duration_min)

imdb_data_frame

index <- order(imdb_data_frame$Rating,decreasing = TRUE )
top_5 <- head(index,5)
imdb_data_frame[top_5[1:5],]

index_1 <- order(imdb_data_frame$Rating)
bottom_5 <- head(index_1,5)
imdb_data_frame[bottom_5[1:5],]     

my_fav_movies <- imdb_data_frame[grepl("Dedemin Insanlari", ignore.case = TRUE, imdb_data_frame$Title) |
                                                     grepl("Ayla", ignore.case = TRUE, imdb_data_frame$Title), ]


yearly_avg_ratings <- imdb_data_frame %>%
  group_by(Year) %>%
  summarize(avg_rating = mean(Rating))

ggplot(yearly_avg_ratings, aes(x = avg_rating, y = Year)) +
  geom_point() +
  labs(title = "Yearly Average Ratings of Turkish Movies", x ="Average Rating" , y = "Year")

movie_counts <- imdb_data_frame %>%
  group_by(Year) %>%
  summarise(count = n())


ggplot(movie_counts, aes(x = count, y = Year)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of Movies Over the Years", x = "Number of Movies", y = "Year" )

correlation <- cor(imdb_data_frame$Votes, imdb_data_frame$Rating)

                            


url_3 <- "https://m.imdb.com/search/title/?title_type=feature&groups=top_1000&country_of_origin=TR"

data_html_3 <- read_html(url_3)

title_names_3 <-html_nodes(data_html_3,'.ipc-title__text')
title_names_3 <- html_text(title_names_3)
title_names_3 <- tail(head(title_names_3,-1),-1)
title_names_3 <- str_split(title_names_3, " ", n=2)
title_names_3 <- unlist(lapply(title_names_3, function(x) {x[2]}))

year_3 <- html_elements(data_html_3,'.dli-title-metadata > span:nth-child(1)')
year_3 <- html_text(year_3)
year_3 <- as.numeric(year_3)


imdb_data_frame_top1000 <- data.frame(Title=title_names_3,Year=year_3)
imdb_data_frame_top1000


index_2 <- order(imdb_data_frame$Rating,decreasing = TRUE )
top_11 <- head(index_2,11)
top_11 <- imdb_data_frame[top_11[1:11],]

merged_data <- left_join(imdb_data_frame_top1000,imdb_data_frame, by = c("Title", "Year"))

order(imdb_data_frame_top1000)
