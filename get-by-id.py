#!/usr/bin/env python

import csv
import json
import pytz
import datetime
import populartimes as pt

api_key = 'AIzaSyA92zfr0K_Fya4-UTaiVuqlLMcjpMPXh2M'

places = []
with open('places.csv') as infile:
    dr = csv.DictReader(infile)
    for d in dr:
        places.append(d)

placeinfo = [pt.get_id(api_key, p['gmapsid']) for p in places]

timestamp = datetime.datetime.now(tz=pytz.timezone('US/Pacific')).isoformat()

with open(f'output/{timestamp}.json', 'w') as outfile:
    outfile.write(json.dumps(placeinfo))
