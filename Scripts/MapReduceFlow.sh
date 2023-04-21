#!/bin/bash

export HADOOP_CLASSPATH="$(hadoop classpath)"

MONTH_YEAR=$1
MONTH_YEAR_NUMBER=$2
CRIME_NUMBER=$3

HADOOP_HOST="http://localhost:9870/explorer.html#"

hadoop fs -rm -r "/CrimeAnalysis/Output"

javac -classpath $HADOOP_CLASSPATH -d MapReduceFiles/classobjects MapReduceFiles/CrimeMapReduce.java

jar -cvf MapReduceFiles/CrimeMapReduce.jar -C MapReduceFiles/classobjects .

hadoop jar MapReduceFiles/CrimeMapReduce.jar CrimeMapReduce "/CrimeAnalysis/Input/in.csv" "/CrimeAnalysis/Output" $MONTH_YEAR $MONTH_YEAR_NUMBER $CRIME_NUMBER

# hadoop fs -get hdfs://localhost:9870/CriminalAnalysis/Output/part-r-00000 ~/Desktop/project/Output