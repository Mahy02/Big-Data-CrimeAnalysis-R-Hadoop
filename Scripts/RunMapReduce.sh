#!/bin/bash

YEAR="Y"
MONTH="M"

read -p "Month (M) or Year (Y)? " YEAR_MONTH

if [ "$YEAR_MONTH" != "$YEAR" -a "$YEAR_MONTH" != "$MONTH" ] ; then
    echo "Invalid input!"
    echo "Getting Top 3 Crimes for Year 2022"
    Scripts/MapReduceFlow.sh Y 2022 3
    exit 0
fi

if [ "$YEAR_MONTH" == "$YEAR" ] ; then
    
    read -p "Enter Year: " YEAR
    if [ "$YEAR" -ge 2023 -o "$YEAR" -le 1999 ] ; then
        echo "Invalid Year!"
        echo "Getting Top 3 Crimes for Year 2022"
        Scripts/MapReduceFlow.sh Y 2022 3
        exit 0
    fi

    read -p "Enter the number of Top Crimes: " CRIME_NUMBER
    if [ "$CRIME_NUMBER" -ge 10 -o "$CRIME_NUMBER" -le 0 ] ; then
        echo "Invalid Top Crimes Number!"
        echo "Getting Top 3 Crimes for Year $YEAR"
        Scripts/MapReduceFlow.sh Y $YEAR 3
        exit 0
    fi

    echo "Getting Top $CRIME_NUMBER Crime(s) for Year $YEAR"
    Scripts/MapReduceFlow.sh Y $YEAR $CRIME_NUMBER
    exit 0
fi

if [ "$YEAR_MONTH" == "$MONTH" ] ; then

    read -p "Enter Month number (1-12): " MONTH
    if [ "$MONTH" -le 0 -o "$MONTH" -ge 13 ] ; then
        echo "Invalid Month number!"
        echo "Getting Top 3 Crimes for Month 1 (January)"
        Scripts/MapReduceFlow.sh M 1 3
        exit 0
    fi

    read -p "Enter the number of Top Crimes: " CRIME_NUMBER
    if [ "$CRIME_NUMBER" -ge 10 -o "$CRIME_NUMBER" -le 0 ] ; then
        echo "Invalid Top Crimes Number!"
        echo "Getting Top 3 Crimes for Month $MONTH"
        Scripts/MapReduceFlow.sh M $MONTH 3
        exit 0
    fi

    echo "Getting Top $CRIME_NUMBER Crime(s) for Month $MONTH"
    Scripts/MapReduceFlow.sh M $MONTH $CRIME_NUMBER
    exit 0
fi