#!/bin/sh

printf "Content-type: text/plain\n"
printf "Access-control-allow-origin: *\n\n"

zwiftid=$(echo $QUERY_STRING | cut -d'=' -f2)

if [[ x"$zwiftid" == x"" ]]; then
        printf "Unable to locate Zwift ID"
        exit 1
fi

numCompare() {
        awk -v n1=$1 -v n2=$2 'BEGIN { printf (n1 >= n2 ? "yes" : "no") }'
}

#max=$(wget -q -O - "https://www.zwiftpower.com/api3.php?do=profile_results&z=$zwiftid&type=all" | jq '[.data[] | {wkg: .wkg_ftp[0] | tonumber, date: .event_date}] | sort_by(.date) | reverse | .[0:30] | [.[].wkg] | max')

# Fetch events in the last 30 days...
thirtydays=$(echo "$(date -u +'%s') - 2592000" | bc)
maxel=$(wget -q -O - "https://www.zwiftpower.com/api3.php?do=profile_results&z=$zwiftid&type=all" | jq "[.data[] | {wkg: .wkg_ftp[0] | tonumber, date: .event_date, flag: .flag, male: .male, title: .event_title, name: .name, avg_power: .avg_power[0], avg_wkg: .avg_wkg[0],  wftp: .wftp[0], zid: .zid | tonumber}] | sort_by(.date) | reverse | map(select(.date > $thirtydays)) | max_by(.wkg)")

if [[ "$(echo $maxel | jq length)" == "0" ]]; then
        # unable to find any events in the last 30 days, take the last 10 races instead
        maxel=$(wget -q -O - "https://www.zwiftpower.com/api3.php?do=profile_results&z=$zwiftid&type=all" | jq '[.data[] | {wkg: .wkg_ftp[0] | tonumber, date: .event_date, flag: .flag, male: .male, title: .event_title, name: .name, avg_power: .avg_power[0], avg_wkg: .avg_wkg[0],  wftp: .wftp[0], zid: .zid | tonumber}] | sort_by(.date) | reverse | .[0:10] | max_by(.wkg)')
fi

printf "Name: $(echo $maxel | jq .name)\n"

max=$(echo $maxel | jq .wkg)
catlabel="Category"
if [[ "$(echo $maxel | jq .male)" == "0" ]]; then
        catlabel="Mixed Category"
        if [[ "$(numCompare $max 3.7)" == "yes" ]]; then
                printf "Women's Category: A\n"
        elif [[ "$(numCompare $max 3.2)" == "yes" ]]; then
                printf "Women's Category: B\n"
        elif [[ "$(numCompare $max 2.5)" == "yes" ]]; then
                printf "Women's Category: C\n"
        else
                printf "Women's Category: D\n"
        fi
fi

if [[ "$(numCompare $max 4.0)" == "yes" ]]; then
        printf "$catlabel: A\n"
elif [[ "$(numCompare $max 3.2)" == "yes" ]]; then
        printf "$catlabel: B\n"
elif [[ "$(numCompare $max 2.5)" == "yes" ]]; then
        printf "$catlabel: C\n"
else
        printf "$catlabel: D\n"
fi

printf "W/kg: $max\n"
printf "Watts: $(echo $maxel | jq .wftp)\n"
printf "Country: $(echo $maxel | jq .flag | tr '[:lower:]' '[:upper:]')\n";
printf "Gender: $(echo $maxel | jq '.male | if . == 0 then "Female" else "Male" end')\n";

exit 0

