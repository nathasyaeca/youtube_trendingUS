Youtube Trending
================
Nathasya Pramudita
2023-11-15

\# Youtube Trending Video from 2017-2018

``` r
# import dataset
youtube <- read_csv("~/Datasets/Trending YT Video Statistic/USvideos.csv") %>% 
  mutate(trending_date = ydm(trending_date),
         category_id = as.factor(category_id))
```

    ## Rows: 40949 Columns: 16
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): video_id, trending_date, title, channel_title, tags, thumbnail_lin...
    ## dbl  (5): category_id, views, likes, dislikes, comment_count
    ## lgl  (3): comments_disabled, ratings_disabled, video_error_or_removed
    ## dttm (1): publish_time
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# find the categories that get the most views
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
```

![](youtube_trendingUS_script_files/figure-gfm/find%20the%20categories%20that%20get%20the%20most%20views-1.png)<!-- -->

``` r
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
```

``` r
# combining likes+dislikes
(likes|dislikes) +
  plot_annotation(title = "Categories of Video Trending",
                  subtitle = "That Generated Most Likes and Dislikes")
```

![](youtube_trendingUS_script_files/figure-gfm/combining%20likes%20and%20dislikes-1.png)<!-- -->

``` r
# see how much comment that get disable based on categories
youtube %>% 
  filter(comments_disabled == TRUE) %>% 
  group_by(category_id) %>% 
  summarize(sum = sum(comments_disabled)) %>% 
  ggplot(aes(category_id, sum)) +
  geom_segment(aes(x = category_id, xend = category_id,
                   y = 0, yend = sum), color = "black") +
  geom_point(color = "purple",size = 4, alpha = .7) +
  labs(x = "Category ID",
       y = "Disabled Comments",
       title = "Total Comment that get disable based on categories") +
  theme_classic()+
  theme(text = element_text(family = "Goudy Old Style"))
```

![](youtube_trendingUS_script_files/figure-gfm/see%20how%20much%20comment%20that%20get%20disable%20based%20on%20categories-1.png)<!-- -->

``` r
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
```

![](youtube_trendingUS_script_files/figure-gfm/Video%20that%20have%20the%20most%20viewers%20in%202017-2018-1.png)<!-- -->

``` r
# categories video that get the most disable comment and ratting
youtube %>% 
  filter(comments_disabled == T) %>% 
  slice_sample(n = 1000) %>% 
  ggplot(aes(category_id, dislikes, color = comments_disabled)) +
  geom_point(show.legend = F)
```

![](youtube_trendingUS_script_files/figure-gfm/categories%20video%20that%20get%20the%20most%20disable%20comment%20and%20ratting-1.png)<!-- -->
