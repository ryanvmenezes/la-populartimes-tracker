library(furrr)
library(jsonlite)
library(lubridate)
library(tidyverse)

plan(multisession)

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

gmapsplaces = data %>% 
  transmute(
    placeinfo = map(
      jsondata,
      ~.x %>% 
        as_tibble() %>% 
        select(id, name, address, types, coordinates) %>% 
        flatten()
    )
  ) %>% 
  unnest(c(placeinfo)) %>% 
  mutate(types = map_chr(types, str_c, collapse = '|')) %>% 
  distinct()

gmapsplaces  

gmapsplaces %>% write_csv('clean/google-maps-place-info.csv')

# popular times

## cleaning up the data

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
    query = future_map(
      populartimes,
      safely(
        ~.x %>% 
          as_tibble() %>% 
          mutate(data = map_chr(data, str_c, collapse = '|')) %>% 
          separate(data, into = str_c('h', seq(0,23)), sep = '\\|') %>% 
          pivot_longer(-name, names_to = 'hour', values_to = 'popularity') %>% 
          mutate(name = str_sub(name, end = 3)) %>% 
          unite(dayhour, name:hour) %>% 
          pivot_wider(names_from = dayhour, values_from = popularity),
      ),
      .progress = TRUE
    ),
    error = map_chr(query, ~typeof(pluck(., 'error'))),
    ptimebydayhour = map(query, ~pluck(., 'result'))
  )

poptimesclean

## locations that don't consistently have the populartimes

poptimesclean %>%
  filter(error != 'NULL') %>% 
  count(name)

## cleaned popular times

poptimes = poptimesclean %>% 
  anti_join(
    poptimesclean %>%
      filter(error != 'NULL') %>%
      select(id, name) %>%
      distinct()
  ) %>% 
  select(datadatetime, id, name, ptimebydayhour) %>% 
  unnest(c(ptimebydayhour)) %>% 
  arrange(id, name, datadatetime)

poptimes

# how often are the popular times changing?
# if any of these are n > 1, there's been a change in the popular times data

expected.by.location = poptimes %>% 
  arrange(id, datadatetime) %>% 
  distinct_at(vars(-datadatetime), .keep_all = TRUE)

expected.by.location

expected.by.location %>% 
  write_csv('clean/expected-time-by-location.csv')

change.in.pop = expected.by.location %>% 
  count(id, name) %>% 
  filter(n > 1)

change.in.pop %>% 
  write_csv('clean/change-in-pop.csv')

# joining currentpopularity and exppopularity for that hour

places = read_csv('places.csv')

popularity.expected = currentpop %>% 
  right_join(
    poptimes %>% 
      group_by(datadatetime, id, name) %>% 
      nest() %>% 
      ungroup()
  ) %>%
  left_join(
    places,
    by = c('id' = 'gmapsid')
  ) %>% 
  mutate(
    exppopularity = map2_chr(
      str_c(dayofweek, hourofday, sep = '_h'),
      data,
      ~.y %>% pull(.x)
    ),
    exppopularity = as.integer(exppopularity)
  ) %>% 
  select(datadatetime, venuename, typeofspace, current = current_popularity, expected = exppopularity)

popularity.expected

writetime = stamp('2020-10-20 23:30:47')

popularity.expected %>% 
  mutate(datadatetime = writetime(datadatetime)) %>% 
  write_csv('clean/popularity-expected.csv')
