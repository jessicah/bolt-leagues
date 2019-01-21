IND_PREV_ROUND=$1
IND_CUR_ROUND=$2
TEAM_PREV_ROUND=$3
TEAM_CUR_ROUND=$4

# individual results
echo "category,events,points,tname,tc,tbc,tbd,name,flag,zwid" > individuals_temp.csv
# combined points
q -H -d, "select cur.category, (prev.events + cur.events), (prev.points + cur.points), cur.tname, cur.tc, cur.tbc, cur.tbd, cur.name, cur.flag, cur.zwid from $IND_CUR_ROUND cur join $IND_PREV_ROUND prev on cur.zwid = prev.zwid" >> individuals_temp.csv
# racers only from previous round
q -H -d, "select prev.* from $IND_PREV_ROUND prev where prev.zwid not in (select cur.zwid from $IND_CUR_ROUND cur)" >> individuals_temp.csv
# racers only from current round
q -H -d, "select cur.* from $IND_CUR_ROUND cur where cur.zwid not in (select prev.zwid from $IND_PREV_ROUND prev)" >> individuals_temp.csv
# and sort results
q -O -H -d, "select temp.* FROM individuals_temp.csv temp ORDER BY category, points DESC" > individuals.csv
rm individuals_temp.csv

# team results
echo "category,events,points,tname,tc,tbc,tbd" > teams_temp.csv
# combined points
q -H -d, "SELECT cur.category, (cur.events + prev.events), (cur.points + prev.points), cur.tname, cur.tc, cur.tbc, cur.tbd FROM $TEAM_CUR_ROUND cur JOIN $TEAM_PREV_ROUND prev ON cur.tname = prev.tname AND cur.category = prev.category" >> teams_temp.csv
# teams only from previous round
q -H -d, "SELECT prev.* FROM $TEAM_PREV_ROUND prev WHERE NOT EXISTS (SELECT cur.* FROM $TEAM_CUR_ROUND cur WHERE prev.category = cur.category AND prev.tname = cur.tname)" >> teams_temp.csv
# teams only from current round
q -H -d, "SELECT cur.* FROM $TEAM_CUR_ROUND cur WHERE NOT EXISTS (SELECT prev.* FROM $TEAM_PREV_ROUND prev WHERE prev.category = cur.category AND prev.tname = cur.tname)" >> teams_temp.csv
# and sort results
q -O -H -d, "SELECT * FROM teams_temp.csv ORDER BY category, points DESC" > teams.csv
rm teams_temp.csv
