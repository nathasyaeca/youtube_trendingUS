# load the packages
library(tidyverse) # packages all in one
library(extrafont) # to edit font in graph
library(skimr) # data exploration
install.packages("patchwork")
library(patchwork) # to combine graphs into one frame
# import the df
youtube <- read_csv("~/Datasets/Trending YT Video Statistic/USvideos.csv") %>% 
  mutate(trending_date = ydm(trending_date),
         category_id = as.factor(category_id))
# find the best catagories that always trending
youtube %>% 
  group_by(category_id) %>% 
  summarize(sum = sum(views)) %>% 
  mutate(category_id = fct_reorder(category_id, sum),
         sum = sum/1000000) %>% 
  ggplot(aes(x = category_id, y = sum, fill = category_id)) +
  geom_col(alpha = .7, width = .5, show.legend = F, fill = "purple") +
  coord_flip() +
  labs(x = "Category ID",
       y = "Viewers in Million",
       title = "Categories of Youtube Videos",
       subtitle = "That have the high probabily of trending") +
  theme_classic() +
  theme(text = element_text(family = "Goudy Old Style"))
# The most like and dislike categories
#likes
likes <- youtube %>%
  group_by(category_id) %>% 
  summarize(ratio = sum(likes)/sum(views)) %>% 
  mutate(category_id = fct_reorder(category_id, ratio, .desc = T)) %>% 
  ggplot(aes(category_id, ratio)) +
  geom_segment(aes(x = category_id, xend = category_id,
                   y = 0, yend = ratio), color = "skyblue") +
  geom_point(color = "blue", size = 4, alpha = .7) +
  labs(x = "Category ID",
       y = "Ratio") +
  theme_classic() + 
  theme(text = element_text(family = "Goudy Old Style")) 
#dislike
dislikes <- youtube %>% 
  group_by(category_id) %>% 
  summarize(ratio = sum(likes)/sum(views)) %>% 
  mutate(category_id = fct_reorder(category_id, ratio, .desc = T)) %>% 
  ggplot(aes(category_id, ratio)) +
  geom_segment(aes(x = category_id, xend = category_id,
                   y = 0, yend = ratio), color = "pink") +
  geom_point(color = "red", size = 4, alpha = .7) +
  labs(x = "Category ID",
       y = "Ratio") +
  theme_classic() +
  theme(text = element_text(family = "Goudy Old Style"))
# combining likew+dislikes
(likes|dislikes) +
  plot_annotation(title = "Categories of Video Trending",
                  subtitle = "That Generated Most Likes and Dislikes")
# see how much comment that get disable
youtube %>% 
  mutate(comment_count = comment_count/1000000) %>% 
  ggplot(aes(comments_disabled, comment_count, fill = comments_disabled)) +
  geom_col(width = .5, alpha = .6, show.legend = F) +
  labs(x = "",
       y = "Comment in Million") +
  theme_classic() +
  theme(text = element_text(family = "Goudy Old Style"))
# Video that've the most viewers in 2017-2018
youtube %>% 
  group_by(channel_title) %>% 
  summarize(total_views = sum(views)) %>% 
  mutate(total_views = total_views/1000000,
         channel_title = fct_reorder(channel_title, desc(total_views), .desc = T)) %>% 
  select(channel_title, total_views) %>% 
  arrange(desc(total_views)) %>% 
  head(10) %>% 
  ggplot(aes(channel_title, total_views, fill = channel_title)) +
  geom_col(width = .7, alpha = .5, show.legend = F) +
  coord_flip() +
  labs(x = "",
       y = "Total Views in Million",
       title = "The Most Watched Youtube Channel from 2017-2018") +
  theme_classic() +
  theme(text = element_text(family = "Goudy Old Style"))