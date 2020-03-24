library(tidyverse)
library(lubridate)

places = read_csv('places.csv')

places

popularity = read_csv('analysis/popularity-expected.csv') %>%
  mutate(datadatetime = lubridate::as_datetime(datadatetime, tz = 'America/Los_Angeles')) %>% 
  left_join(
    places %>% select(venuename, typeofspace, id = gmapsid)
  ) %>% 
  select(datadatetime, venuename, typeofspace, current = current_popularity, expected = exppopularity) %>% 
  pivot_longer(cols = c(current, expected), names_to = 'popularity.type', values_to = 'popularity') %>% 
  arrange(venuename, datadatetime)

popularity

makeplot = function(data, title = '') {
  ggplot(data, aes(x = datadatetime, y = popularity, color = popularity.type)) +
    geom_line() +
    facet_wrap(. ~ venuename) +
    xlab('') +
    ggtitle(title) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))
}

comparison.chart = popularity %>% makeplot()

comparison.chart

ggsave('analysis/plot.png', comparison.chart, units = 'in', width = 11, height = 7)

popularity %>% 
  group_by(typeofspace) %>% 
  nest() %>% 
  mutate(
    plot = map2(data, typeofspace, makeplot),
    save = walk2(plot, typeofspace, ~ggsave(str_c('analysis/', .y, '.png'), .x, units = 'in', width = 11, height = 7))
  )

writetime = stamp('2020-03-20 23:30:47')

popularity %>%
  mutate(
    datadatetime = writetime(datadatetime)
  ) %>% 
  write_csv('analysis/foottraffic-clean.csv')

popularity %>%
  mutate(
    datadatetime = writetime(datadatetime),
    popularity.type = str_c(popularity.type, '.popularity')
  ) %>% 
  pivot_wider(names_from = popularity.type, values_from = popularity) %>% 
  write_csv('analysis/foottraffic-clean-wide.csv')
