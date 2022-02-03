punto1() {
    echo "PUNTO 1:" &&
    cat /proc/cpuinfo | grep "model name"
}

punto2() {
    echo "PUNTO 2:" &&
    cat /proc/cpuinfo | grep "model name" | wc -l
}

punto3() {
    echo "PUNTO 3: " &&
    curl https://www.gutenberg.org/files/11/11-0.txt > JULIAN_in_wonderland.txt &&
    sed -i 's/Alice/Julian/g' JULIAN_in_wonderland.txt
}

punto4() {
    echo "PUNTO 4:" &&
    sort -nrk 5 datos/weather_cordoba.in |
    echo "MAXIMO: "$(head -n 1) &&
    sort -nrk 5 datos/weather_cordoba.in |
    echo "MINIMO: "$(tail -n 1)
}

punto5() {
    sort -nk 3 datos/atpplayers.in
}

punto6() {
    awk '$0=$0"" $7-$8' datos/superliga.in | sort -rn -k2 -k9
}

punto7() {
    echo "PUNTO 7:" &&
    ifconfig | grep ether
}

punto8() {
    echo "PUNTO 8:" &&
    mkdir Friends &&
    cd Friends/ &&
    for i in {0..9}; do touch "fma_S01E0""$i""_es.srt"; done &&
    for file in *_es.srt*; do mv -- "$file" "${file//_es.srt/''}"; done
}

punto1 0
punto2 0
punto3 0
punto4 0
punto5 0
punto6 0
punto7 0
punto8 0
