library(jsonlite)
library(lubridate)
library(tidyverse)

# raw data

data = tibble(file = str_c('output/', list.files('output/'))) %>% 
  arrange(file) %>% 
  mutate(
    datadatetime = str_replace(file, 'output/', ''),
    datadatetime = str_replace(datadatetime, '.json', ''),
    datadatetime = as_datetime(datadatetime, tz = 'America/Los_Angeles'),
    jsondata = map(file, fromJSON),
  ) %>% 
  filter(date(datadatetime) > make_date(2020, 3, 20)) # start of observations

data

# current popularity by location at each time

currentpop = data %>% 
  transmute(
    datadatetime,
    currentpop = map(
      jsondata,
      ~.x %>% 
        as_tibble() %>% 
        select(id, name, current_popularity)
    )
  ) %>% 
  unnest(c(currentpop)) %>% 
  mutate(
    current_popularity = replace_na(current_popularity, 0),
    dayofweek = lubridate::wday(datadatetime, label = TRUE),
    hourofday = lubridate::hour(datadatetime)
  )

currentpop

# list of places being tracked

places = currentpop %>% 
  select(id, name) %>% 
  distinct()

places

# popular times

# cleaning up the data

poptimesclean = data %>%
  transmute(
    datadatetime,
    poptimes = map(
      jsondata,
      ~.x %>% 
        as_tibble() %>% 
        select(id, name, populartimes)
    )
  ) %>% 
  unnest(c(poptimes)) %>% 
  mutate(
    query = map(
      populartimes,
      safely(
        ~.x %>% 
          as_tibble() %>% 
          mutate(data = map_chr(data, str_c, collapse = '|')) %>% 
          separate(data, into = str_c('h', seq(0,23)), sep = '\\|') %>% 
          pivot_longer(-name, names_to = 'hour', values_to = 'popularity') %>% 
          mutate(name = str_sub(name, end = 3)) %>% 
          unite(dayhour, name:hour) %>% 
          pivot_wider(names_from = dayhour, values_from = popularity)
      )
    ),
    error = map_chr(query, ~typeof(pluck(., 'error'))),
    ptimebydayhour = map(query, ~pluck(., 'result'))
  )

poptimesclean

# locations that don't consistently have the populartimes

poptimesclean %>%
  filter(error != 'NULL') %>% 
  count(name)

# cleaned popular times

poptimes = poptimesclean %>% 
  anti_join(
    poptimesclean %>%
      filter(error != 'NULL') %>%
      select(id, name) %>%
      distinct()
  ) %>% 
  # filter(error == 'NULL') %>% 
  select(datadatetime, id, name, ptimebydayhour) %>% 
  unnest(c(ptimebydayhour)) %>% 
  arrange(id, name, datadatetime)

poptimes

# how often are the popular times changing?
# if any of these are n > 1, there's been a change in the popular times data

change.in.pop = poptimes %>% 
  distinct_at(vars(-datadatetime)) %>% 
  count(id, name) %>% 
  filter(n > 1)

change.in.pop %>% 
  write_csv('analysis/change-in-pop.csv')

# joining currentpopularity and exppopularity for that hour

popularity.expected = currentpop %>% 
  right_join(
    poptimes %>% 
      group_by(datadatetime, id, name) %>% 
      nest() %>% 
      ungroup()
  ) %>%
  mutate(
    exppopularity = map2_chr(
      str_c(dayofweek, hourofday, sep = '_h'),
      data,
      ~.y %>% 
        pull(.x)
    )
  ) %>% 
  select(datadatetime, id, name, current_popularity, dayofweek, hourofday, exppopularity) %>% 
  mutate(exppopularity = as.integer(exppopularity))

popularity.expected

popularity.expected %>% 
  write_csv('analysis/popularity-expected.csv')