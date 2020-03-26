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


popularity %>% 
  filter(venuename %in% c('L.A. Union Station','Temescal Canyon Park','MedMen WeHo','Santa Monica Pier', 'LACMA')) %>% 
  group_by(venuename) %>% 
  nest() %>% 
  mutate(
    plot = map2(
      data,
      venuename,
      ~.x %>% 
        ggplot(aes(datadatetime, popularity, fill = popularity.type)) +
        geom_bar(stat = "identity", position = 'dodge') +
        ggtitle(.y) +
        scale_fill_brewer(palette = 'Accent') +
        theme_minimal()
    ),
    save = walk2(plot, venuename, ~ggsave(str_c('analysis/', .y, '.png'), .x, units = 'in', width = 11, height = 7))
  )

popularity %>% 
  mutate(datadatetime = floor_date(datadatetime, unit = 'hour')) %>% 
  filter(venuename %in% c('L.A. Union Station','Temescal Canyon Park','MedMen WeHo','Santa Monica Pier', 'LACMA', 'Dockweiler Beach', 'Grand Central Market')) %>%
  distinct(datadatetime, venuename, typeofspace, popularity.type, .keep_all = TRUE) %>% 
  pivot_wider(names_from = popularity.type, values_from = popularity) %>% 
  group_by(venuename) %>% 
  nest() %>% 
  mutate(
    plot = map2(
      data,
      venuename,
      ~.x %>% 
        ggplot(aes(x = datadatetime)) +
        geom_bar(aes(y = expected), stat = "identity", fill = '#beaed4') +
        geom_line(aes(y = current), color = '#7fc97f', size = 1) +
        # ggtitle(.y) +
        scale_y_continuous(limits = c(0,150)) +
        theme_minimal() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank())
    ),
    # save = walk2(plot, venuename, ~ggsave(str_c('analysis/', .y, '-combo.png'), .x, units = 'in', width = 11, height = 7)),
    save2 = walk2(plot, venuename, ~ggsave(str_c('analysis/svg/', .y, '-combo.svg'), .x, units = 'in', width = 11, height = 7))
  ) %>% 
  pull(plot)
