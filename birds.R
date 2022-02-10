# Load packages
library(dplyr)
library(readr)
library(ggwordcloud)
library(stringr)

# Read in bird checklist
birds <- read_csv("https://raw.githubusercontent.com/KateMMiller/IMD_R_Training_Advanced/main/data/birds.csv")

# Preview dataset
birds

# Verify that all species names are unique
paste("There are", nrow(birds), "rows in this dataset.")
paste("There are", nrow(distinct(birds)), "unique rows in this dataset.")

# How many species groups are there?
paste("There are", n_distinct(birds$species_group), "species groups.")

# Any missing data?
any(is.na(birds$species_group))
any(is.na(birds$primary_com_name))

# Count species in each species group
group_size <- birds %>%
  group_by(species_group) %>%
  summarize(species_count = n()) %>%
  arrange(-species_count) %>%
  ungroup()

# What are the largest species groups?
group_size

# What are the smallest species groups?
tail(group_size)

## Species description wordcloud

# Remove the type of bird (last word in the species name)
bird_words <- birds %>%
  mutate(primary_com_name = str_replace(primary_com_name, "\\b(\\w+)$", ""),
         primary_com_name = str_replace_all(primary_com_name, "-", " "),
         primary_com_name = trimws(primary_com_name),
         primary_com_name = tolower(primary_com_name)) %>%
  filter(primary_com_name != "")

# Split species descriptions into single words
bird_words <- paste(bird_words$primary_com_name, collapse = " ") %>%
  str_split(boundary("word"), simplify = TRUE) %>%
  as.vector()

# Get frequency of each word
bird_words_df <- tibble(word = bird_words) %>%
  group_by(word) %>%
  summarise(freq = n()) %>%
  arrange(-freq) %>%
  filter(freq > 10) %>%
  mutate(angle = 90 * sample(c(-1:1), n(), replace = TRUE, prob = c(1, 3, 1)))

# Make the word cloud
n_words <- 100  # Number of words to include
ggplot(bird_words_df[1:n_words,],
       aes(label = word, size = freq, 
           color = sample.int(10, n_words, replace = TRUE),
           angle = angle)) +
  geom_text_wordcloud_area() + 
  scale_size_area(max_size = 20) +
  theme_minimal() +
  scale_color_continuous(type = "viridis")

ggsave("birds_wordcloud.png", bg = "white")