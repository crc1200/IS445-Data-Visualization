
#Libraries
install.packages("ggradar")
library(Hmisc)
library(psych)
library(GGally)
library(ggplot2)
library(vioplot)
library(corrplot)
library(dplyr)
library(readr)
library(superheat)
library(GGally)
library(stringr)

install.packages("scales")
library(scales)


install.packages("devtools")
devtools::install_github("ricardo-bion/ggradar", 
                         dependencies = TRUE)
library(ggradar)

install.packages("waterfalls")
library(waterfalls)

########################################################################################################################

####################### Read in Data Set #######################

Book_Data <- read_csv("Assignment 2 Books.csv")
dim(Book_Data)

total_percentage_by_genre <- Book_Data %>%
  mutate(first_genre = str_extract(genre_and_votes, "^[^0-9]+") %>% str_trim()) %>%
  group_by(first_genre) %>%
  summarise(
    total_ratings = sum(rating_count, na.rm = TRUE),
    avg_rating = (mean(average_rating, na.rm = TRUE)),
  )

####################### Scatterplot Matrix #######################

set.seed(43)
sampled_data <- Book_Data %>%
  sample_n(7500)

selected_categories <- c("Classics")

classic_books <- sampled_data %>%
  mutate(first_genre = str_extract(genre_and_votes, "^[^0-9]+") %>% str_trim()) %>%
  filter(first_genre %in% selected_categories)

selected_columns <- classic_books %>%
  mutate(log_avg = log(average_rating + 1),
         log_rating_count = log(rating_count + 1),
         log_review_count = log(review_count + 1),
         log_star_rating = log(five_star_ratings + 1), 
         log_pages = log(number_of_pages + 1)) %>%
  select(
    `Log Average\nRating` = log_avg,
    `Log Number of\nRatings` = log_rating_count,
    `Log Number of\nReviews` = log_review_count,
    `Log Number of\nFive-Star\nRatings` = log_star_rating,
    `Log Page \nCount` = log_pages
  ) 

ggpairs(selected_columns, 
        title = "Scatterplot of 'Classic' Genre Book Attributes") + labs(subtitle='Goodreads \'Books2\' Dataset') + theme_light()

####################### Bubble Chart #######################

set.seed(123)
sampled_data <- Book_Data %>%
  sample_n(10000)

selected_categories <- c("Self Help")

fantasy_books <- sampled_data %>%
  mutate(first_genre = str_extract(genre_and_votes, "^[^0-9]+") %>% str_trim()) %>%
  filter(first_genre %in% selected_categories)

bubble_data <- fantasy_books %>%
  filter(!is.na(number_of_pages) & number_of_pages > 0) %>%
  mutate(
    rating_bucket = cut(
      rating_count, 
      breaks = c(0, 500, 1000, 5000, Inf), 
      labels = c("0-500", "501-1000", "1001-5000", "5000+"),
      include.lowest = TRUE
    )
  )


ggplot(bubble_data, aes(x = number_of_pages, 
                        y = average_rating, 
                        size = as.numeric(rating_bucket))) + 
  geom_point(alpha = 0.5, fill = "cornflowerblue", color = "black", shape = 21) + 
  geom_smooth(method = "lm", color = "red", linetype = 1, se = FALSE, show.legend = FALSE) +
  scale_size_continuous(
    name = "Number of Ratings", 
    breaks = 1:4, 
    labels = c("0-500", "501-1000", "1001-5000", "5000+")) + 
  labs(
    title = "Self Help Book Ratings by Page Count and Number of Ratings",
    subtitle="Goodreads 'Books2' Dataset",
    x = "Page Count",
    y = "Average Rating"
  ) +
  theme_minimal() +
  theme(legend.position = "right")


####################### Heat Map #######################

set.seed(123)
sampled_data <- Book_Data %>%
  sample_n(10000)

bubble_data_genre <- sampled_data %>%
  mutate(first_genre = str_extract(genre_and_votes, "^[^0-9]+") %>% 
           str_trim())

genre_heatmap_data <- bubble_data_genre %>%
  group_by(first_genre) %>%
  summarise(
    avg_rating = log(mean(average_rating, na.rm = TRUE)),
    avg_ratings = log(mean(rating_count, na.rm = TRUE)),
    avg_pages = log(mean(number_of_pages, na.rm = TRUE))
  ) %>%
  filter(!is.na(first_genre))

genre_heatmap_data_clean <- genre_heatmap_data %>%
  filter(
    is.finite(avg_rating), 
    is.finite(avg_ratings),
    is.finite(avg_pages)  
  )

genre_heatmap_data <- as.data.frame(genre_heatmap_data_clean)
rownames(genre_heatmap_data) <- genre_heatmap_data$first_genre
genre_heatmap_data$first_genre <- NULL

sampled_genre_data <- genre_heatmap_data %>%
  sample_n(40)

colnames(sampled_genre_data) <- c("Average Rating", "Number of \n Ratings", "Page Count")

superheat(sampled_genre_data, 
          scale = TRUE, 
          left.label.text.size = 2,
          bottom.label.text.size = 2,
          bottom.label.size = 0.1,
          row.dendrogram = TRUE,
          title = "Book Metrics by Genre") 

####################### Radar Chart #######################

selected_categories <- c("Childrens-Picture Books", "Horror", "Football")

rating_percentage_by_genre <- sampled_data %>%
  mutate(first_genre = str_extract(genre_and_votes, "^[^0-9]+") %>% str_trim()) %>%
  filter(first_genre %in% selected_categories) %>%
  group_by(first_genre) %>%
  summarise(
    total_ratings = sum(five_star_ratings + four_star_ratings + three_star_ratings + two_star_ratings + one_star_ratings, na.rm = TRUE),
    five_star_pct = (sum(five_star_ratings, na.rm = TRUE) / total_ratings * 100),
    one_star_pct = (sum(one_star_ratings, na.rm = TRUE) / total_ratings * 100),
    avg_rating = (mean(average_rating, na.rm = TRUE)),
    avg_reviews = (mean(review_count, na.rm = TRUE)),
    avg_ratings = mean(rating_count, na.rm = TRUE),
    avg_pages = (mean(number_of_pages, na.rm = TRUE))
    
  )

radar_data <- rating_percentage_by_genre %>%
  select(first_genre, avg_pages, avg_rating, avg_reviews) %>%
  mutate_at(vars(-first_genre), ~ . / max(., na.rm = TRUE))

ggradar(radar_data,
        grid.label.size = 3,
        axis.label.size = 3,
        group.point.size = 5,
        group.line.width = 1.5,
        legend.text.size = 10,
        group.colours = c("red", "blue", "orange"),
        axis.labels = c("Page Count", "Average Rating", "Number of \n Reviews")
) + labs(title = "Genres, Page Count, Average Rating, Amount of Reviews")

####################### Waterfall Chart #######################

# waterfall_categories <- c("Humor-Jokes")

# waterfall_categories <- c("European Literature-Bulgarian Literature")

# European Literature-Bulgarian Literature

waterfall_categories <- c("European Literature-Finnish Literature")

waterfall_data <- Book_Data %>%
  mutate(first_genre = str_extract(genre_and_votes, "^[^0-9]+") %>% str_trim()) %>%
  group_by(first_genre) %>%
  filter(first_genre%in%waterfall_categories) %>%
  summarise(
    total_ratings = sum(rating_count, na.rm = TRUE),
    avg_rating = (mean(average_rating, na.rm = TRUE)),
    total_five_star = round((sum(five_star_ratings, na.rm = TRUE) / total_ratings * 100), 2),
    total_four_star = round((sum(four_star_ratings, na.rm = TRUE) / total_ratings * 100), 2),
    total_three_star = round((sum(three_star_ratings, na.rm = TRUE) / total_ratings * 100), 2),
    total_two_star = round((sum(two_star_ratings, na.rm = TRUE) / total_ratings * 100), 2),
    total_one_star = round((sum(one_star_ratings, na.rm = TRUE) / total_ratings * 100), 2),
  )

reviews <- data.frame(
  category = c(
    "Five Stars",
    "Four Stars",
    "Three Stars",
    "Two Stars",
    "One Star"
  ),
  amount = c(
    waterfall_data$total_five_star,
    waterfall_data$total_four_star,
    waterfall_data$total_three_star,
    -waterfall_data$total_two_star,
    -waterfall_data$total_one_star
  )
)

waterfall(reviews, 
          calc_total = TRUE, 
          total_axis_text = "More Positive \nReviews (%)", 
          total_rect_text_color = "black", 
          total_rect_color = "goldenrod1") + 
  theme_minimal() +
  labs(
    title = "Humor Genre Sentiment Analysis",
    subtitle = "Positive VS. Negative Ratings",
    x = "Stars Received",
    y = "Proportion of Total Ratings (%)"
  )
