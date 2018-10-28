#!/bin/bash

# Criterium Racing League
EVENT_IDS="96800 95159 93646 91983 90324 88428 86504 84629"

# ZwiftPower sprints URL
SPRINTS_URI='https://www.zwiftpower.com/api3.php?do=event_sprints&zid=@@EVENT_ID@@&csv=1'

# ZwiftPower results URL
RESULTS_URI='https://www.zwiftpower.com/api3.php?do=event_results&zid=@@EVENT_ID@@&csv=1'

rm -rf temp
mkdir -p temp

for zid in $EVENT_IDS; do
	echo "Fetching data for $zid..."
	sprint_uri=$(echo "$SPRINTS_URI" | sed -s "s/@@EVENT_ID@@/$zid/")
	curl -sS $sprint_uri -o "temp/$zid.sprints.csv"
	results_uri=$(echo "$RESULTS_URI" | sed -s "s/@@EVENT_ID@@/$zid/")
	curl -sS $results_uri -o "temp/$zid.results.csv"
#	q -H -d , "select r.name, r.category, r.position_in_cat, s.position_in_cat from temp/$zid.results.csv r join temp/$zid.sprints.csv s on r.zwid = s.zwid"
done

# generate our results file
cat temp/*.results.csv | awk -F , 'NR==1 { print $0 } $1=="DT_RowId" { next } { print $0 }' > temp/results.csv
cat temp/*.sprints.csv | awk -F , 'NR==1 { print $0 } $1=="DT_RowId" { next } { print $0 }' > temp/sprints.csv