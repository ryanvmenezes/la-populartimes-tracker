library(tidyverse)
library(lubridate)

popularity = read_csv('clean/popularity-expected.csv') %>%
  mutate(datadatetime = floor_date(datadatetime, unit = 'hour')) %>% 
  distinct(datadatetime, venuename, typeofspace, .keep_all = TRUE)

popularity

comparison.chart = popularity %>%
  pivot_longer(current:expected, names_to = 'popularity.type', values_to = 'popularity') %>% 
  ggplot(aes(x = datadatetime, y = popularity, color = popularity.type)) +
  geom_line() +
  facet_wrap(. ~ venuename) +
  xlab('') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

comparison.chart

ggsave('analysis/plot.png', comparison.chart, units = 'in', width = 11, height = 7)

ind.plots = popularity %>% 
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
    )
  )

ind.plots

ind.plots %>% 
  mutate(
    save = walk2(
      plotobj, venuename, ~ggsave(str_c('analysis/png/', .y, '.png'), .x, units = 'in', width = 11, height = 7)
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
      'Dockweiler Beach'
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
  mutate(save = walk2(svgplot, venuename, ~ggsave(str_c('analysis/svg/', .y, '.svg'), .x, units = 'in', width = 11, height = 7)))
