#make 

#cat main.list | grep -oP '\t[0-9a-f]{8}' | awk -v awkvar="$LONG" '{print awkvar}'


declare -a tests=('eq' 'ge' 'gt' 'hi' 'hs' 'le' 'lo' 'ls' 'lt' 'mi' 'ne' 'pl')

run () {
    for i in "${tests[@]}"
    do 
        make $i
        LONG=$(cat $i.list | grep -oP '\t[0-9a-f]{8}' | wc -l)
        cat $i.list | grep -oP '\t[0-9a-f]{8}' | awk -v awkvar="$LONG" '{if (NR==1) {print "ROM [0:" awkvar-1 "] =\x27{32\x27h" $1 ","} else { if (NR==awkvar){print "32\x27h" $1 "};"} else {print "32\x27h" $1 ","}}}' > dumps/$i.dump
    done
    make clean
}

run 0
rm *.o


