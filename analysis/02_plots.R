library(tidyverse)

places = read_csv('places.csv')

places

popularity = read_csv('analysis/popularity-expected.csv') %>%
  mutate(datadatetime = lubridate::as_datetime(datadatetime, tz = 'America/Los_Angeles')) %>% 
  left_join(
    places %>% select(venuename, id = gmapsid)
  ) %>% 
  select(datadatetime, venuename, current = current_popularity, expected = exppopularity) %>% 
  pivot_longer(-datadatetime:-venuename, names_to = 'popularity.type', values_to = 'popularity')

popularity

comparison.chart = popularity %>%
  ggplot(aes(x = datadatetime, y = popularity, color = popularity.type)) +
  geom_line() +
  facet_wrap(. ~ venuename) +
  theme(axis.text.x = element_text(angle = 90))
  theme_minimal()

comparison.chart

ggsave('analysis/plot.png', comparison.chart, units = 'in', width = 11, height = 7)
