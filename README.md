p99-patcher
===============

A full featured patcher for the http://project1999.org Everquest
emulator/server.

## Extra Features

Realtime map application (run locally) to keep track of yourself
and where you're located on a given map!

![Sample Mapper](https://github.com/ahungry/p99-patcher/raw/master/p99-mapper.png)

## Installing

You can get the binary download all ready (the exe) for Windows at:

http://ahungry.com/p99-patcher-min.7z

Then after downloading it, just extract via 7-zip (make sure your EQ install
path has a Logs/eqlog_YourName_project1999.txt and a maps/ directory for the
mapper to work.

### In game steps to take

Make sure that /log is turned on (this won't work without it!)

Make a macro that combines Sense Heading and /loc (or click/type manually, just
remember the program relies on seeing this updated in somewhat real time if
you want your position updated in somewhat real time).

### TODO

Many of the noobie towns/initial areas have been set up in the application.

I have to make the hash table for the rest of the zone names to zone files,
then eventually update the map files with extras, such as mob spawn locations
and other information from the EQAtlas mirrors.

## Used ports
```
4444 - hunchentoot port
```
