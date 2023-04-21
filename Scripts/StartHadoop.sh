#!/bin/bash

jps | awk '{print $1}' | xargs kill

start-dfs.sh

hdfs dfs -rm -r /CrimeAnalysis/

hdfs dfs -mkdir /CrimeAnalysis
hdfs dfs -mkdir /CrimeAnalysis/Input
hdfs dfs -put Major_Crime_Indicators.csv /CrimeAnalysis/Input/in.csv

start-yarn.sh
