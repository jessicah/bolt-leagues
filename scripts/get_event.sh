#!/bin/bash

if [ "$#" -ne 1 ]; then
	echo "Please specifiy ZwiftPower event ID"
	exit 1
fi

# ZwiftPower sprints URL
SPRINTS_URI='https://www.zwiftpower.com/api3.php?do=event_sprints&zid=@@EVENT_ID@@'

# ZwiftPower results URL
RESULTS_URI='https://www.zwiftpower.com/api3.php?do=event_results&zid=@@EVENT_ID@@'

mkdir -p output

curl -s "https://www.zwiftpower.com/ucp.php?mode=login" -d 'username=jessica.l.hamilton@gmail.com&password=&autologin=1&redirect=./index.php?&login=' -c "zwiftpower.cookies"

echo "Fetching data for $1..."
sprint_uri=$(echo "$SPRINTS_URI" | sed -s "s/@@EVENT_ID@@/$1/")
curl -sS -c "zwiftpower.cookies" $sprint_uri -o "output/$1.sprints.json"
results_uri=$(echo "$RESULTS_URI" | sed -s "s/@@EVENT_ID@@/$1/")
curl -sS -c "zwiftpower.cookies" $results_uri -o "output/$1.results.json"
