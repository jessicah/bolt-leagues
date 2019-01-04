#!/bin/bash

#IRON_GOAT_IDS="101154 99243 97449 95704 94214 92665"
#AGES_RACING_IDS="100512 98943 96985 95205 93675 92203"
#CRIT_RACING_IDS="100398 98736 96800 95159 93646 91983"
#TIME_TRIAL_IDS="100016 98369 96388 94909 93208 91671"

# New Leagues starting 1st January
IRON_GOAT_IDS=""
AGES_RACING_IDS=""
CRIT_RACING_IDS=""
TIME_TRIAL_IDS="124741"

# ZwiftPower sprints URL
SPRINTS_URI='https://www.zwiftpower.com/api3.php?do=event_sprints&zid=@@EVENT_ID@@'

# ZwiftPower results URL
RESULTS_URI='https://www.zwiftpower.com/api3.php?do=event_results&zid=@@EVENT_ID@@'

rm -rf temp
mkdir -p temp/irongoat
mkdir -p temp/ages
mkdir -p temp/crit
mkdir -p temp/tt

for zid in $IRON_GOAT_IDS; do
	echo "Fetching data for $zid..."
	sprint_uri=$(echo "$SPRINTS_URI" | sed -s "s/@@EVENT_ID@@/$zid/")
	curl -sS $sprint_uri -o "temp/irongoat/$zid.sprints.json"
	results_uri=$(echo "$RESULTS_URI" | sed -s "s/@@EVENT_ID@@/$zid/")
	curl -sS $results_uri -o "temp/irongoat/$zid.results.json"
#	q -H -d , "select r.name, r.category, r.position_in_cat, s.position_in_cat from temp/$zid.results.csv r join temp/$zid.sprints.csv s on r.zwid = s.zwid"
done
for zid in $AGES_RACING_IDS; do
	echo "Fetching data for $zid..."
	sprint_uri=$(echo "$SPRINTS_URI" | sed -s "s/@@EVENT_ID@@/$zid/")
	curl -sS $sprint_uri -o "temp/ages/$zid.sprints.json"
	results_uri=$(echo "$RESULTS_URI" | sed -s "s/@@EVENT_ID@@/$zid/")
	curl -sS $results_uri -o "temp/ages/$zid.results.json"
#	q -H -d , "select r.name, r.category, r.position_in_cat, s.position_in_cat from temp/$zid.results.csv r join temp/$zid.sprints.csv s on r.zwid = s.zwid"
done
for zid in $CRIT_RACING_IDS; do
	echo "Fetching data for $zid..."
	sprint_uri=$(echo "$SPRINTS_URI" | sed -s "s/@@EVENT_ID@@/$zid/")
	curl -sS $sprint_uri -o "temp/crit/$zid.sprints.json"
	results_uri=$(echo "$RESULTS_URI" | sed -s "s/@@EVENT_ID@@/$zid/")
	curl -sS $results_uri -o "temp/crit/$zid.results.json"
#	q -H -d , "select r.name, r.category, r.position_in_cat, s.position_in_cat from temp/$zid.results.csv r join temp/$zid.sprints.csv s on r.zwid = s.zwid"
done
for zid in $TIME_TRIAL_IDS; do
	echo "Fetching data for $zid..."
	sprint_uri=$(echo "$SPRINTS_URI" | sed -s "s/@@EVENT_ID@@/$zid/")
	curl -sS $sprint_uri -o "temp/tt/$zid.sprints.json"
	results_uri=$(echo "$RESULTS_URI" | sed -s "s/@@EVENT_ID@@/$zid/")
	curl -sS $results_uri -o "temp/tt/$zid.results.json"
#	q -H -d , "select r.name, r.category, r.position_in_cat, s.position_in_cat from temp/$zid.results.csv r join temp/$zid.sprints.csv s on r.zwid = s.zwid"
done

# generate our results file
#cat temp/*.results.csv | awk -F , 'NR==1 { print $0 } $1=="DT_RowId" { next } { print $0 }' > temp/results.csv
#cat temp/*.sprints.csv | awk -F , 'NR==1 { print $0 } $1=="DT_RowId" { next } { print $0 }' > temp/sprints.csv
