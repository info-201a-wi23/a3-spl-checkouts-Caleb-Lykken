library(tidyverse)
library(ggplot2)
lb_data <- read.csv("2013-2023-5-Checkouts-SPL.csv")
ld_data <-read.csv("Checkouts_by_Title.csv")
cb_data <- read.csv("Cooking_Books.csv")


cooking_checkout_years <- cb_data %>% 
  group_by(CheckoutYear) %>% 
  summarize(cooking_checkouts = sum(Checkouts))

five_checkout_years <- ld_data %>% 
  group_by(CheckoutYear) %>% 
  summarize(total_checkouts = sum(Checkouts))
five_checkout_years <- five_checkout_years %>% filter(CheckoutYear != 2013)

five_total_checkout_years <- lb_data %>% 
  group_by(CheckoutYear) %>% 
  summarize(total_checkouts = sum(Checkouts))

total_checkout_years <- rbind(five_checkout_years, five_total_checkout_years)

Combined_data <-left_join(total_checkout_years, cooking_checkout_years)
round
Combined_data <- Combined_data %>% mutate(proportion_of_cooking = (round((cooking_checkouts/total_checkouts)*100,2)))
combined_data <- Combined_data %>% filter(CheckoutYear > 2012 & CheckoutYear != 2023)

ggplot(data=combined_data, aes(x= CheckoutYear, y = proportion_of_cooking)) +
  geom_bar(stat="identity" , fill = "steelblue") +  
  geom_text(aes(label=proportion_of_cooking), vjust=1.6, color="white", size=3.5) +
  ggtitle("Percentage of cooking items checked out between 2013 to 2022")

