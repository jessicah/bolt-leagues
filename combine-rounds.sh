IND_PREV_ROUND=$1
IND_CUR_ROUND=$2
TEAM_PREV_ROUND=$3
TEAM_CUR_ROUND=$4

# individual results
echo "category,events,points,tname,tid,tc,tbc,tbd,name,flag,zwid,podiums" > individuals_temp.csv
# combined points
q -H -d, "select cur.category, (prev.events + cur.events), (prev.points + cur.points), cur.tname, cur.tid, cur.tc, cur.tbc, cur.tbd, cur.name, cur.flag, cur.zwid, prev.podiums from $IND_CUR_ROUND cur join $IND_PREV_ROUND prev on cur.zwid = prev.zwid" >> individuals_temp.csv
# racers only from previous round
q -H -d, "select prev.* from $IND_PREV_ROUND prev where prev.zwid not in (select cur.zwid from $IND_CUR_ROUND cur)" >> individuals_temp.csv
# racers only from current round
q -H -d, "select cur.* from $IND_CUR_ROUND cur where cur.zwid not in (select prev.zwid from $IND_PREV_ROUND prev)" >> individuals_temp.csv
# and sort results
q -O -H -d, "select temp.* FROM individuals_temp.csv temp ORDER BY category, points DESC" > individuals.csv
rm individuals_temp.csv

# test combining podiums
q -O -H -d, "select i.category, i.events, i.points, i.tname, i.tid, i.tc, i.tbc, i.tbd, i.name, i.flag, i.zwid, trim(ifnull(i.podiums, '') || ' ' || ifnull(p.podiums,'')) as podiums from individuals.csv i left join current_podiums.csv p on i.zwid = p.zwid" > individuals_with_podiums.csv

# team results
echo "category,events,points,tid,tname,tc,tbc,tbd" > teams_temp.csv
# combined points
q -H -d, "SELECT cur.category, (cur.events + prev.events), (cur.points + prev.points), cur.tid, cur.tname, cur.tc, cur.tbc, cur.tbd FROM $TEAM_CUR_ROUND cur JOIN $TEAM_PREV_ROUND prev ON cur.tid = prev.tid AND cur.category = prev.category" >> teams_temp.csv
# teams only from previous round
q -H -d, "SELECT prev.* FROM $TEAM_PREV_ROUND prev WHERE NOT EXISTS (SELECT cur.* FROM $TEAM_CUR_ROUND cur WHERE prev.category = cur.category AND prev.tid = cur.tid)" >> teams_temp.csv
# teams only from current round
q -H -d, "SELECT cur.* FROM $TEAM_CUR_ROUND cur WHERE NOT EXISTS (SELECT prev.* FROM $TEAM_PREV_ROUND prev WHERE prev.category = cur.category AND prev.tid = cur.tid)" >> teams_temp.csv
# and sort results
q -O -H -d, "SELECT * FROM teams_temp.csv ORDER BY category, points DESC" > teams.csv
rm teams_temp.csv
