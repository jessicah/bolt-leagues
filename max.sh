#!/bin/bash

max=$(curl -s "https://www.zwiftpower.com/api3.php?do=profile_results&z=$1&type=all" | jq '[.data[] | {wkg_ftp: .wkg_ftp[0] |tonumber, date: .event_date}] | sort_by(.date) | reverse | .[0:30] | [.[].wkg_ftp] | max')

numCompare() {
	awk -v n1=$1 -v n2=$2 'BEGIN { printf (n1 >= n2 ? "yes" : "no") }'
}

echo "max = \"$max\""

echo $(numCompare $max 2.0)
echo $(numCompare $max 3.0)
echo $(numCompare $max 4.0)

if [[ "$(numCompare $max 4.0)" == "yes" ]]; then
	echo "Category A";
elif [[ "$(numCompare $max 3.2)" == "yes" ]]; then
	echo "Category B";
elif [[ "$(numCompare $max 2.5)" == "yes" ]]; then
	echo "Category C";
else
	echo "Category D";
fi
