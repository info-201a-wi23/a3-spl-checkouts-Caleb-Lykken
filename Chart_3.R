library(dplyr)
library(ggplot2)
seasons <- c("Winter", "Winter", "Spring", "Spring", "Spring", "Summer", "Summer", "Summer","Fall", "Fall", "Fall" ,"Winter")
cb_data <- read.csv("Cooking_Books.csv")
cb_data <- cb_data %>% mutate(Season = seasons[CheckoutMonth])
season_df <- cb_data %>% group_by(Season) %>% summarize(total_checkouts = sum(Checkouts))
season_df <- season_df %>% mutate(percentage = round(total_checkouts/sum(total_checkouts)*100))

ggplot(season_df, aes(x = "", y= total_checkouts, fill=Season)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)  +  
  geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust=0.5)) +
  ggtitle("Seasons that books are checked out between 2005 and 2023")


  
