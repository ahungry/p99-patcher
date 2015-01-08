#!/bin/sh

# Parse out some data from an EQ log
log=$1

# Things related to damage taken
taken=$(grep 'YOU' $log)
total=$(echo "$taken"|wc -l)
misses=$(echo "$taken"|grep 'but misses'|wc -l)
dodges=$(echo "$taken"|grep 'but YOU dodge'|wc -l)
parry=$(echo "$taken"|grep 'but YOU parry'|wc -l)
riposte=$(echo "$taken"|grep 'but YOU riposte'|wc -l)
damage_log=$(echo "$taken"|grep -E 'YOU for [0-9]+')
damage=$(echo "$taken"|grep -E 'YOU for'|wc -l)
damage_spread=$(echo "$damage_log"|awk '{print $11}'|sort|uniq -c|sort -nr|awk '{print $2 " (" $1/'$damage'*100 "%)"}'|column)
damage_average=$(echo "$damage_log"|awk '{total+=1; sum+=$11} END {print sum/total}')
damage_average_swing=$(echo "$damage_log"|awk '{total+=1; sum+=$11} END {print sum/'$total'}')

# Things related to damage dealt
dealt=$(grep -E '(You try|You.*for [0-9]+ points)' $log)
dealt_total=$(echo "$dealt"|wc -l)
dealt_misses=$(echo "$dealt"|grep 'but miss!'|wc -l)
dealt_dodges=$(echo "$dealt"|grep 'dodges'|wc -l)
dealt_parry=$(echo "$dealt"|grep 'parries'|wc -l)
dealt_riposte=$(echo "$dealt"|grep 'ripos'|wc -l)
dealt_log=$(grep -E 'You.*for [0-9]+ points' $log)
dealt_log_count=$(grep -E 'You.*for [0-9]+ points' $log|wc -l)
dealt_spread=$(echo "$dealt_log"|awk '{print $11}'|sort|uniq -c|sort -nr|awk '{print $2 " (" $1/'$dealt_log_count'*100 "%)"}'|column)
dealt_average=$(echo "$dealt_log"|awk '{total+=1; sum+=$11} END {print sum/total}')
dealt_average_swing=$(echo "$dealt_log"|awk '{total+=1; sum+=$11} END {print sum/'$dealt_total'}')

cat << OUT
--------------------------------------------------------
DAMAGE TAKEN REPORT (AC)
--------------------------------------------------------

Data for ${total} hits:

Breakdown (attacks done to you):
--------------------------------------------------------
Misses:   $(echo "$misses/$total*100"|bc -l)
Dodges:   $(echo "$dodges/$total*100"|bc -l)
Parry:    $(echo "$parry/$total*100"|bc -l)
Riposte:  $(echo "$riposte/$total*100"|bc -l)
Hit For:  $(echo "$damage/$total*100"|bc -l)

Damage Spread (damage taken and how often it was taken):
--------------------------------------------------------
$damage_spread

Averages:
--------------------------------------------------------
$damage_average damage taken per successful hit
$damage_average_swing damage taken per swing (adding avoidance)


OUT

cat << OUT
--------------------------------------------------------
DAMAGE DEALT REPORT (DPS)
--------------------------------------------------------

Data for ${dealt_total} hits:

Breakdown (attacks you dealt):
--------------------------------------------------------
Misses:   $(echo "$dealt_misses/$dealt_total*100"|bc -l)
Dodges:   $(echo "$dealt_dodges/$dealt_total*100"|bc -l)
Parry:    $(echo "$dealt_parry/$dealt_total*100"|bc -l)
Riposte:  $(echo "$dealt_riposte/$dealt_total*100"|bc -l)
Hit For:  $(echo "$dealt_log_count/$dealt_total*100"|bc -l)

Damage Spread (damage dealt and how often it was delivered):
--------------------------------------------------------
$dealt_spread

Averages:
--------------------------------------------------------
$dealt_average damage dealt per successful hit
$dealt_average_swing damage dealt per swing (adding avoidance)

OUT


