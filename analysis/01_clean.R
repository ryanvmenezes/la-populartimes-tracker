library(furrr)
library(jsonlite)
library(lubridate)
library(tidyverse)

plan(multisession)
availableCores()

# import ------------------------------------------------------------------

# bring in jsons one by one

data = tibble(file = str_c('output/', list.files('output/'))) %>% 
  # in alpha/time order
  arrange(file) %>% 
  mutate(
    # convert filename to proper datetime
    filedatetime = str_replace(file, 'output/', ''),
    filedatetime = str_replace(filedatetime, '.json', ''),
    filedatetime = as_datetime(filedatetime, tz = 'America/Los_Angeles'),
    # round down to top of hour
    datadatetime = floor_date(filedatetime, unit = 'hours'),
    # bring in json
    jsondata = map(file, fromJSON)
  ) %>%
  # keep only one observation per hour
  distinct(datadatetime, .keep_all = TRUE) %>% 
  # start of observations
  filter(date(datadatetime) >= make_date(2020, 3, 21))

data

# is there exactly a one hour difference between each observation?

sum(as.integer(data$datadatetime - lag(data$datadatetime)) > 1, na.rm = TRUE) == 0

# current popularity ------------------------------------------------------

currentpop = data %>% 
  transmute(
    datadatetime,
    currentpop = map(
      jsondata,
      ~.x %>% 
        # flatten json into tibble
        as_tibble() %>% 
        # get these vars
        select(id, name, current_popularity)
    )
  ) %>% 
  unnest(c(currentpop)) %>% 
  mutate(
    current_popularity = replace_na(current_popularity, 0),
    dayofweek = wday(datadatetime, label = TRUE),
    hourofday = hour(datadatetime)
  )

currentpop

# place info from google maps ---------------------------------------------

gmapsplaces = data %>% 
  transmute(
    placeinfo = map(
      jsondata,
      ~.x %>% 
        as_tibble() %>% 
        select(id, name, address, types, coordinates) %>% 
        # need to separate out coordinates
        jsonlite::flatten()
    )
  ) %>% 
  unnest(c(placeinfo)) %>% 
  mutate(types = map_chr(types, str_c, collapse = '|')) %>% 
  distinct()

gmapsplaces  

gmapsplaces %>% write_csv('clean/google-maps-place-info.csv')

# popular times -----------------------------------------------------------

# cleanup

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
    # safely transform the table
    query = future_map(
      populartimes,
      safely(
        # convert 24x7 table into one WIDE table
        # column names: Mon_h0, Mon_h1, ... Sun_h23
        # this will help to track changes later with a distinct query
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

# filter out places that errored and unnest
# this is all popular times ever recorded

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
# if any location has > 1 distinct row, there's been a change in the popular times data

expected.by.location = poptimes %>% 
  arrange(id, datadatetime) %>% 
  # keep only the unique sequences of 24x7 hours
  distinct_at(vars(-datadatetime), .keep_all = TRUE)

expected.by.location

expected.by.location %>% write_csv('clean/expected-time-by-location.csv')

change.in.pop = expected.by.location %>% 
  count(id, name) %>% 
  filter(n > 1)

change.in.pop

change.in.pop %>% write_csv('clean/change-in-pop.csv')

# join it all together ----------------------------------------------------

# my places list

places = read_csv('places.csv')

places

popularity.expected = currentpop %>% 
  # right join to popularity matrix to filter
  # this has popular times recorded every hour
  right_join(
    poptimes %>% 
      group_by(datadatetime, id, name) %>% 
      nest() %>% 
      ungroup()
  ) %>%
  # bring in clean place names
  left_join(
    places,
    by = c('id' = 'gmapsid')
  ) %>% 
  mutate(
    # pull expected popular time from the matrix
    expected.popularity = map2_chr(
      str_c(dayofweek, hourofday, sep = '_h'),
      data,
      ~.y %>% pull(.x)
    ),
    expected.popularity = as.integer(expected.popularity)
  ) %>% 
  select(datadatetime, venuename, typeofspace, current = current_popularity, expected = expected.popularity)

popularity.expected

# write out cleaned file --------------------------------------------------

# date formatter to prevent dates being written in UTC time
# need to convert them to characters first

writetime = stamp('2020-10-20 23:30:47')

popularity.expected %>% 
  mutate(datadatetime = writetime(datadatetime)) %>% 
  write_csv('clean/popularity-expected.csv')
