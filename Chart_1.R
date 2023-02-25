library(dplyr)
library(ggplot2)
library(tidyr)
cb_data <- read.csv("Cooking_Books.csv")
checkout_years_BOOK <- cb_data %>% 
  group_by(CheckoutYear) %>% 
  filter(MaterialType == "BOOK") %>% 
  summarize(BOOKS = sum(Checkouts))
  
checkout_years_EBOOK <- cb_data %>% 
  group_by(CheckoutYear) %>% 
  filter(MaterialType == "EBOOK") %>% 
  summarize(EBOOKS = sum(Checkouts))

checkout_years <- left_join(checkout_years_BOOK, checkout_years_EBOOK, by = "CheckoutYear")
checkout_years <- checkout_years %>% filter(CheckoutYear != 2023)

df <- checkout_years %>%
  select(CheckoutYear, EBOOKS, BOOKS) %>%
  gather(key = "Type_of_book", value = "Number_of_checkouts", -CheckoutYear)

# Visualization
ggplot(df, aes(x = CheckoutYear, y = Number_of_checkouts)) + 
  geom_line(aes(color = Type_of_book, linetype = Type_of_book)) + 
  scale_color_manual(values = c("darkred", "steelblue")) +
  ggtitle("Ebooks VS books checked out from 2005 to 2022")
