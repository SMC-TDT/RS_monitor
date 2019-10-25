#!/bin/sh

STARTdate="2018-11-23"
ENDdate="2019-01-31"

# Set paths
SCRIPTpath="./prog"

# Path to Rscript command in operator@smcprotdt01
RS="Rscript"

#########################################################################################


ini=$(date -d $STARTdate +%s)
fin=$(date -d $ENDdate +%s)

while [ "$ini" -le "$fin" ]
    do

    DATE=$(date -d "@$ini" +%F)
    YY=$(date -d "@$ini" +%Y)
    MM=$(date -d "@$ini" +%m)
        
    echo $DATE

    $RS $SCRIPTpath/RS_readLOG_main.R -d "$DATE"
    
    ini=$(($ini+86400))

    done

