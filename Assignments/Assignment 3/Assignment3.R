
#Libraries

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
library(scales)
library(ggradar)
library(waterfalls)
library(lubridate)
library(ggcorrplot)
library(CGPfunctions)
library(tidyr)
library(gridExtra)
library(grid)

########################################################################################################################


# Read in Data Set

book_data <- read_csv("Assignment 2 Books.csv")

# Time-Dependent Graph

book_data_time <- book_data %>% select(title, rating_count, review_count, average_rating, date_published)

extract_year <- function(date) {
  if (grepl("^[A-Za-z]+ \\d{1,2}[a-z]{2} \\d{4}$", date)) {
    return(date)
  } else {
    return(NA)
  }
}

book_data_time <- book_data %>%
  mutate(year_published = sapply(date_published, extract_year))


dates_filtered <- book_data_time %>% select(title, rating_count, review_count, average_rating, date_published, year_published) %>% filter(!is.na(year_published))


dates_filtered <- dates_filtered %>% mutate(
  date_clean = str_replace_all(date_published, "(\\d+)(st|nd|rd|th)", "\\1"),
  date_published = as.Date(date_clean, format = "%B %d %Y")
)

average_ratings <- dates_filtered %>%
  filter(year(date_published) > 2000 & year(date_published) < 2020) %>%  
  mutate(year_month = as.Date(format(date_published, "%Y-%m-01"))) %>%  
  group_by(year_month) %>%
  summarize(
    avg_rating = mean(average_rating, na.rm = TRUE),
    avg_rating_count = mean(rating_count, na.rm = TRUE),
    avg_review_count = mean(review_count, na.rm = TRUE)
  )


ggplot(average_ratings, aes(x = year_month, y = avg_rating)) +
  geom_line(color = "indianred3", size = 1) +
  geom_smooth() + 
  labs(
    title = "Average Rating of Books vs. Date Published",
    subtitle = "Published Between 2000 to 2020",
    x = "Date Published (Month-Year)",
    y = "Average Rating (1-5)"
  ) +
  scale_x_date(date_breaks='2 years',
               labels=date_format("%b-%Y")) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 28, face = "bold", color = "black"),
    plot.subtitle = element_text(size = 22, color = "black"),
    axis.title.x = element_text(size = 22, face = "bold", color = "black", margin = margin(t = 10)), 
    axis.title.y = element_text(size = 22, face = "bold", color = "black", margin = margin(r = 10)), 
    axis.text.x = element_text(size = 20, face = "bold", color = "black"),  
    axis.text.y = element_text(size = 20, color = "black"),
    panel.grid.minor = element_blank()
  )

########################################################################################################################

# Correlation Plot


selected_categories <- c("Fantasy")

fantasy_books <- book_data %>%
  mutate(first_genre = str_extract(genre_and_votes, "^[^0-9]+") %>% str_trim()) %>%
  filter(first_genre %in% selected_categories)


correlation_plot_data <- fantasy_books %>% select(rating_count, review_count, average_rating, five_star_ratings, four_star_ratings, three_star_ratings, two_star_ratings, one_star_ratings, number_of_pages)

correlation_plot_data_clean <- na.omit(correlation_plot_data)

correlation_plot_data_normal <- correlation_plot_data_clean %>%
  mutate(
    rating_count = (rating_count), 
    review_count = (review_count),
    average_rating = (average_rating), 
    five_star_ratings = (five_star_ratings) / rating_count,
    four_star_ratings = (four_star_ratings) / rating_count,
    three_star_ratings = (three_star_ratings) / rating_count,
    two_star_ratings = (two_star_ratings) / rating_count,
    one_star_ratings = (one_star_ratings) / rating_count,
    number_of_pages = (number_of_pages)
  )

View(correlation_plot_data_normal)

data <- correlation_plot_data_normal %>% select(-rating_count, -review_count)

r <- cor(data,method='pearson')

custom_labels <- c(
  "five_star_ratings" = "% Rated Five Stars",
  "four_star_ratings" = "% Rated Four Stars",
  "three_star_ratings" = "% Rated Three Stars",
  "two_star_ratings" = "% Rated Two Stars",
  "one_star_ratings" = "% Rated One Star",
  "number_of_pages" = "Number of Pages",
  "average_rating" = "Average Rating"
)

ggcorrplot(r, 
           hc.order = TRUE, 
           type = "lower", 
           lab = TRUE,
           lab_size = 7,  
           title = "Correlation Matrix for Fantasy Books")  + 
  scale_x_discrete(labels = custom_labels) +  
  scale_y_discrete(labels = custom_labels) + theme_minimal() +  theme(
    plot.title = element_text(size = 20, face = "bold", color = "black", hjust = 0.5),
    plot.subtitle = element_text(size = 22, color = "black", hjust = 0.5),  
    axis.title.x = element_text(size = 18, face = "bold", color = "black"), 
    axis.title.y = element_text(size = 18, face = "bold", color = "black"), 
    axis.text.x = element_text(size = 16, face = "bold", color = "black", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 16, face = "bold", color = "black"),  
    legend.text = element_text(size = 14, face = "bold", color = "black"),  
    legend.title = element_text(size = 16, face = "bold", color = "black"),  
    legend.key.size = unit(1.2, "cm"),
    plot.margin = margin(15, 15, 15, 15)  
  ) + labs(x = "Book Attribute", y = "Book Attribute")

########################################################################################################################

# Slope Graph

selected_categories <- c("Romance", "Young Adult", "Classics")

genre_books <- book_data %>%
  mutate(first_genre = str_extract(genre_and_votes, "^[^0-9]+") %>% str_trim()) %>%
  filter(first_genre %in% selected_categories) %>%
  select(title, rating_count, review_count, average_rating, date_published, first_genre)

genre_books_data_time <- genre_books %>%
  mutate(year_published = sapply(date_published, extract_year)) %>% filter(!is.na(year_published))

genres_filtered <- genre_books_data_time %>% mutate(
  date_clean = str_replace_all(date_published, "(\\d+)(st|nd|rd|th)", "\\1"),
  date_published = as.Date(date_clean, format = "%B %d %Y")
)

selected_years <- c(2000, 2005, 2010, 2015)

average_ratings_by_genre <- genres_filtered %>%
  filter(year(date_published) %in% selected_years) %>%  
  mutate(year_month = as.Date(format(date_published, "%Y-%m-01"))) %>%  
  group_by(year = year(date_published), first_genre) %>% 
  summarize(
    avg_rating = mean(average_rating, na.rm = TRUE)
  )


df <- average_ratings_by_genre %>% mutate(year = factor(year), rating = round(avg_rating, 2), genre=first_genre) %>% select(genre, year, rating)

df_wide <- df %>%
  pivot_wider(names_from = year, values_from = rating)

df_wide_long <- df_wide %>%
  pivot_longer(cols = starts_with("20"), names_to = "year", values_to = "rating") %>%  # Pivot longer
  mutate(year = as.factor(year))  # Ensure 'year' is a factor

newggslopegraph(df_wide_long, year, rating, genre, Caption = NULL, YTextSize = 6, XTextSize = 18, DataTextSize = 6.5) +  
  labs(
    title = "Average Rating by Genre and Publishing Year", 
    subtitle = "Published From 2000 to 2015"
  ) +
  theme(
    plot.title = element_text(size = 26, face = "bold", color = "black", hjust = 0.5, margin = margin(b = 10)),  # Add margin below title
    plot.subtitle = element_text(size = 19, face = "bold", color = "black", hjust = 0.5, margin = margin(b = 15)),  # Add margin below subtitle
    axis.text = element_text(size = 14, face = "bold", color = "black"),  # Axis text
    axis.title = element_text(size = 16, face = "bold", color = "black"),  # Axis titles
    legend.title = element_text(size = 14, face = "bold", color = "black"),  # Legend title
    legend.text = element_text(size = 12, face = "bold", color = "black"),  # Legend text
    plot.margin = margin(20, 20, 20, 20)  # Add margin around the entire plot
  ) 

########################################################################################################################

# Three Panel Graph (For books published since 1950 years)

selected_publishers <- c("Ballantine Books", "Penguin Books")

publisher_counts <- book_data %>% filter(publisher %in% selected_publishers)

publisher_counts <- publisher_counts %>% mutate(year_published = sapply(date_published, extract_year)) %>% filter(!is.na(year_published))

publisher_counts_dates <- publisher_counts %>% mutate(
  date_clean = str_replace_all(date_published, "(\\d+)(st|nd|rd|th)", "\\1"),
  date_published = as.Date(date_clean, format = "%B %d %Y")
) %>% filter(year(date_published) > 1950)

publisher_counts_dates$color <- ifelse(publisher_counts_dates$publisher == "Penguin Books", "orange", "cornflowerblue")

p1 <- ggplot(publisher_counts_dates, aes(x = publisher, fill = color)) +
  theme_classic() + 
  geom_bar(stat="count", color="black") + 
  labs(x = "Publisher", y = "Books Published", title="Books Published") +
  scale_fill_identity() + theme_minimal() +   theme(
    plot.title = element_text(size = 20, face = "bold", color = "black", margin = margin(t = 10)),  
    axis.text.x = element_text(angle = 0, size = 14, face = "bold", color = "black", margin = margin(t = 10)),  
    axis.text.y = element_text(size = 14, color = "black", margin = margin(r = 10)),  
    axis.title.x = element_text(size = 16, face = "bold", color = "black", margin = margin(t = 20)),  
    axis.title.y = element_text(size = 16, face = "bold", color = "black", margin = margin(r = 20)),  
    legend.title = element_text(size = 14, face = "bold", color = "black", margin = margin(b = 10)),  
    legend.text = element_text(size = 12, color = "black")  
  )

log_publisher_counts <- publisher_counts_dates %>% 
  mutate(
    log_pages = log(number_of_pages + 1),
    log_review_count = log(review_count + 1),
    log_rating_count = log(rating_count + 1),
    year_published = year(date_published)
  ) %>% select(title, publisher, date_published, year_published, log_pages, log_review_count, log_rating_count, rating_count, review_count, average_rating, five_star_ratings, four_star_ratings, three_star_ratings, two_star_ratings, one_star_ratings, number_of_pages)

books_per_year_by_publisher <- log_publisher_counts %>%
  group_by(year_published, publisher) %>%
  summarize(book_count = n(), .groups = "drop")

View(books_per_year_by_publisher)

p2 <- ggplot(books_per_year_by_publisher, aes(x = year_published, y = book_count, color = publisher, group = publisher)) +
  geom_line(size = 1) + 
  theme_minimal() +
  labs(
    title = "Number of Books Published Per Year",
    x = "Year Published",
    y = "Number of Books",
    color = "Publisher"
  ) +
  scale_color_manual(
    values = c("Penguin Books" = "orange", "Ballantine Books" = "cornflowerblue")
  ) +
  scale_x_continuous(
    breaks = seq(min(books_per_year$year_published), max(books_per_year$year_published), by = 15)
  ) +
  scale_y_continuous(
    breaks = seq(0, max(books_per_year_by_publisher$book_count), by = 2)  
  ) +
  theme(
    plot.title = element_text(size = 20, face = "bold", color = "black", hjust = 0.5, margin = margin(t = 10)),  
    axis.text.x = element_text(angle = 0, size = 14, face = "bold", color = "black", margin = margin(t = 10)),  
    axis.text.y = element_text(size = 14, color = "black", margin = margin(r = 10)),  
    axis.title.x = element_text(size = 16, face = "bold", color = "black", margin = margin(t = 20)), 
    axis.title.y = element_text(size = 16, face = "bold", color = "black", margin = margin(r = 20)),  
    legend.title = element_text(size = 14, face = "bold", color = "black", margin = margin(b = 10)),  
    legend.text = element_text(size = 12, color = "black")  
  )

selected_genres <- c("Fiction", "Historical-Historical Fiction", "Nonfiction", "Fantasy")
publisher_genre_books <- publisher_counts_dates %>%
  mutate(first_genre = str_extract(genre_and_votes, "^[^0-9]+") %>% str_trim()) %>%
  filter(first_genre %in% selected_genres) %>% 
  select(title, publisher, first_genre, rating_count, review_count, average_rating, five_star_ratings, four_star_ratings, three_star_ratings, two_star_ratings, one_star_ratings, number_of_pages)
  
publisher_genre_books <- publisher_genre_books %>% mutate(log_page_count = log(number_of_pages + 1))

p3 <- ggplot(publisher_genre_books, aes(x = first_genre, y = number_of_pages, fill = publisher)) +
  geom_boxplot(
    color = "black",
    outlier.shape = 16,
    outlier.size = 2,
    height=1.2,
    linewidth = .25,
    width = .8  
  ) +
  scale_fill_manual(values = c("Penguin Books" = "orange", "Ballantine Books" = "cornflowerblue")) +  
  scale_color_manual(values = c("Penguin Books" = "orange", "Ballantine Books" = "cornflowerblue")) +  
  labs(
    title = "Comparison of Genre Page Count",
    x = "Genre",
    y = "Number of Pages",  
    fill = "Publisher",
    color = "Publisher"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", color = "black", hjust = 0.5, margin = margin(t = 10)),  
    axis.text.x = element_text(angle = 0, size = 14, face = "bold", color = "black", margin = margin(t = 10)), 
    axis.text.y = element_text(size = 14, color = "black", margin = margin(r = 10)), 
    axis.title.x = element_text(size = 16, face = "bold", color = "black", margin = margin(t = 20)), 
    axis.title.y = element_text(size = 16, face = "bold", color = "black", margin = margin(r = 20)),  
    legend.title = element_text(size = 14, face = "bold", color = "black", margin = margin(b = 10)),  
    legend.text = element_text(size = 12, color = "black") 
  )


layout_matrix <- matrix(c(1, 2, 3, 3), ncol = 2, byrow = TRUE)


grid.arrange(
  p1, p2, p3, 
  layout_matrix = layout_matrix,
  top = textGrob(
    "Ballantine Books vs. Penguin Books Publishing Trends 1950 to 2020",
    gp = gpar(fontsize = 26, fontface = "bold"),
  )
)

