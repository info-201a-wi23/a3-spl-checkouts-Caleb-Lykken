library(tidyverse)
cb_data <- read.csv("Cooking_Books.csv")

#Average checkouts for particular type of book

Material_type_average <-cb_data %>%
  group_by(MaterialType) %>% 
  summarize(mean_books_checked_out = mean(Checkouts))

# Number of print books over time

physical_cooking_books_over_timer <- cb_data %>%
  group_by(CheckoutYear) %>% 
  filter(MaterialType == "BOOK") %>% 
  summarize(books_checked_out = sum(Checkouts))

# Highest and lowest cooking Ebooks checked out

year_highest_cooking_books <- cb_data %>%
  group_by(CheckoutYear) %>% 
  filter(MaterialType == "BOOK") %>% 
  summarize(books_checked_out = sum(Checkouts)) %>% 
  filter(books_checked_out == max(books_checked_out)) %>% pull(CheckoutYear)

year_lowest_cooking_books <- cb_data %>%
  group_by(CheckoutYear) %>% 
  filter(MaterialType == "BOOK") %>% 
  summarize(books_checked_out = sum(Checkouts)) %>% 
  filter(books_checked_out == min(books_checked_out)) %>% pull(CheckoutYear)

# Highest and lowest cooking Ebooks checked out

year_highest_cooking_Ebooks <- cb_data %>%
  group_by(CheckoutYear) %>% 
  filter(MaterialType == "EBOOK") %>% 
  summarize(books_checked_out = sum(Checkouts)) %>% 
  filter(books_checked_out == max(books_checked_out)) %>% pull(CheckoutYear)

year_lowest_cooking_books <- cb_data %>%
  group_by(CheckoutYear) %>% 
  filter(MaterialType == "EBOOK") %>% 
  summarize(books_checked_out = sum(Checkouts)) %>% 
  filter(books_checked_out == min(books_checked_out)) %>% pull(CheckoutYear)

  
