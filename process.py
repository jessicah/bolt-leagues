#!/usr/bin/python3

import csv

with open('temp/results.csv') as csvfile:
	reader = csv.DictReader(csvfile)
	last_zid=0
	categories = ['A', 'B', 'C', 'D', 'E']
	num_entrants = {}
	events = {}
	racers = {}
	for row in reader:
		if row['zid'] != last_zid:
			if last_zid != 0:
				# print results
				for cat in categories:
					print('Event', last_zid, 'category', cat, ': total participants: ', num_entrants[cat])
			
			print('new event')
			last_zid = row['zid']
			for cat in categories:
				num_entrants[cat] = 0
		
		num_entrants[row['category']] += 1
		eventID = row['zid']
		if not events[eventID]:
			events[eventID] = {}
		
		events[row[eventID]].total_entrants += 1
		#print(row['zid'], row['position_in_cat'], row['category'], row['name'])
