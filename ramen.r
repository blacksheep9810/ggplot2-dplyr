library(dplyr)
library(ggplot2)
ramen <- read.csv("ramen-ratings.csv")
ramen <- tbl_df(ramen)
data.frame(head(ramen))

Stars <- as.numeric(levels(ramen$Stars))[as.integer(ramen$Stars)]

#counts of ramen per country to find out which country is the most popular 
#for ramen
country_count <- ramen %>% 
  group_by(Country) %>%
  summarise(count = n(), avg = mean(Stars), na.rm = TRUE)

sorted_country <- arrange(country_count, desc(count))

print.data.frame(sorted_country)

#simpler version 
country_count <- flights %>%
  group_by(Country) %>%
  tally(sort=TRUE)

print.data.frame(country_count)

#data visualization for country_counts
pdf("country_counts.pdf")
country_counts <- ggplot(ramen, aes(factor(Country))) + 
  theme_light() + 
  theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0)) +
  geom_bar(width = 2) + 
  labs (y = "Passenger Count", 
        title = "Titanic Survival Rates")
print(country_counts)
dev.off()

#number of distinct brands and countries
ramen %>% 
  summarise(distinct = n_distinct(Brand))

ramen %>% 
  summarise(distinct = n_distinct(Country))

#top 10 brands 
brand_count <- ramen %>% 
  group_by(Brand) %>%
  summarise(count = n()) %>%
  
sorted_brand <- arrange(brand_count)

top_n(sorted_brand, 10)

#top 10 of average stars per country
## **summarise(as.numeric(ramen$Stars))
average_rating <- aggregate( as.numeric(ramen$Stars) ~ Country, ramen, mean)
top_n(average_rating, 10)

#double check 
ramen %>%
  filter(Country == "Brazil")

