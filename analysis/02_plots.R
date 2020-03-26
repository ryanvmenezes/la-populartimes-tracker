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

ind.plots = popularity %>% 
  mutate(datadatetime = floor_date(datadatetime, unit = 'hour')) %>% 
  distinct(datadatetime, venuename, typeofspace, popularity.type, .keep_all = TRUE) %>% 
  pivot_wider(names_from = popularity.type, values_from = popularity) %>% 
  group_by(venuename) %>% 
  nest() %>% 
  mutate(
    plotobj = map2(
      data,
      venuename,
      ~.x %>% 
        ggplot(aes(x = datadatetime)) +
        geom_bar(aes(y = expected), stat = "identity", fill = '#beaed4') +
        geom_line(aes(y = current), color = '#7fc97f', size = 1) +
        scale_y_continuous(limits = c(0,150)) +
        ggtitle(.y) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90))
        #+
        # theme(axis.title.x = element_blank(),
        #       axis.title.y = element_blank(),
        #       panel.grid.major.x = element_blank(),
        #       panel.grid.minor.x = element_blank(),
        #       panel.grid.minor.y = element_blank())
    ),
    # save2 = walk2(plot, venuename, ~ggsave(str_c('analysis/svg/', .y, '-combo.svg'), .x, units = 'in', width = 11, height = 7))
  )

ind.plots

ind.plots %>% 
  mutate(
    save = walk2(
      plot, venuename, ~ggsave(str_c('analysis/png/', .y, '.png'), .x, units = 'in', width = 11, height = 7)
    )
  )
    
ind.plots.svg = ind.plots %>% 
  filter(
    venuename %in% c(
      'L.A. Union Station',
      'Temescal Canyon Park',
      'MedMen WeHo',
      'Santa Monica Pier',
      'LACMA',
      'Dockweiler Beach',
      'Grand Central Market'
    )
  ) %>%
  mutate(
    svgplot = map(
      plotobj,
      ~.x + 
        theme(
          title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank()
        )
    )
  )

ind.plots.svg %>% 
  head() %>% 
  pull(svgplot)
