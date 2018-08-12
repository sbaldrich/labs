library(jsonlite)
library(tidyverse)
library(lubridate)

files <- list.files(pattern = 'followers_.*json')
frames <- vector('list', length(files))
for(i in seq_along(files)){
  frames[[i]] <- fromJSON(read_file(files[[i]]))$users
}
followers <- bind_rows(frames) %>% as_tibble()
followers_trimmed <- 
  select(followers, id_str, screen_name, created_at, utc_offset, location, lang,
         statuses_count, default_profile, default_profile_image, followers_count, 
         friends_count) %>% 
  mutate(created_at = parse_datetime(str_replace_all(str_sub(created_at, 5), '\\+0000 ', ''), format="%b %d %H:%M:%S %Y")) %>% 
  arrange(desc(created_at))

now <- now()
followers_trimmed <- followers_trimmed %>% 
  rowwise() %>% 
  mutate(bot_points = 
           sum(map_dbl(c(default_profile_image,
               followers_count < 5,
               day(as.period(now - created_at)) < 60,
               str_detect(screen_name, "\\d{3,}"),
               str_count(screen_name, '\\d') > 3,
               str_detect(screen_name, '^\\d'),
               statuses_count < 20
             ), as.numeric)
           ))

counts <- followers_trimmed %>% count(bot_points)

ggplot(followers_trimmed) + 
  geom_jitter(aes(x = 1, y = bot_points)) + 
  labs(x = 'Followers', y = "Bot Score", title = 'Bot score w ingroup count') + 
  scale_y_continuous(breaks = seq(0,8), labels = as.character(seq(0,8))) + 
  scale_x_continuous(breaks = FALSE) + 
  geom_label(data = counts, stat = 'identity', aes(x = 1, y = bot_points, label = n), color = 'red') + 
  theme(plot.title = element_text(hjust = 0.5, face='italic'))
  
followers_trimmed %>% 
  arrange(desc(bot_points)) %>% 
  write_csv('followers_with_bot_score.csv')
  
