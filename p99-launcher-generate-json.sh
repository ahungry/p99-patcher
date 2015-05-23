#!/bin/sh

# Easily create the required .json file for package maintenance

# Run this in the top level of an unzipped p99 zip set of files

IFS=$'\n' 

# The filesize for the download zip file
FILESIZE=$(ls -l *zip | awk '{print $5}')

JSON=$(echo -e "[\n")

for f in $(find -type f); do 
	if [ -z "$(echo $f | grep -E '\.zip$')" ]; then
		JSON=$(echo -e "$JSON"; echo -e "    {")
		JSON=$(echo -e "$JSON"; echo -e "        'filename':'$(echo $f|sed -e 's/\.\///g')',")
		JSON=$(echo -e "$JSON"; echo -e "        'checksum':'$(md5sum $f|awk '{print $1}')',")
		JSON=$(echo -e "$JSON"; echo -e "        'download':'http://www.project1999.com/files/P99Files36.zip',")
		JSON=$(echo -e "$JSON"; echo -e "        'filesize':'${FILESIZE}'")
		JSON=$(echo -e "$JSON"; echo -e "    },")
	fi
done

JSON=$(echo -e "${JSON}_" | sed -e 's/,_//g' | sed -e 's/\x27/"/g'; echo -e "]")

echo -e "$JSON"
