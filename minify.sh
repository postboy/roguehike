#!/bin/sh
# stage 0: hand optimizations in source code
# stage 1: change identifiers
# sed -e 's/[^[:alpha:]-]/ /g' src/r/c.clj | tr '\n' " " |  tr -s " " | tr " " '\n' | sort | uniq -c | sort -nr | head -n 25
target=stage1.clj
cp stage0.clj $target
sed -i 's/\(.*\);.*/\1/g' $target
sed -i 's/ref-set/a/g' $target
sed -i 's/(def a a)/(def a ref-set)/g' $target
sed -i 's/(str/(o/g' $target
sed -i 's/ str / o /g' $target
sed -i 's/@screen/@b/g' $target
sed -i 's/ screen / b /g' $target
sed -i 's/@render-delta-x/@d/g' $target
sed -i 's/ render-delta-x / d /g' $target
sed -i 's/@render-delta-y/@e/g' $target
sed -i 's/ render-delta-y / e /g' $target
sed -i 's/@cur-altitude/@f/g' $target
sed -i 's/ cur-altitude / f /g' $target
sed -i 's/@status-message/@g/g' $target
sed -i 's/ status-message/ g /g' $target
sed -i 's/@canvas-cols/@h/g' $target
sed -i 's/ canvas-cols / h /g' $target
sed -i 's/@cur-energy/@j/g' $target
sed -i 's/ cur-energy / j /g' $target
sed -i 's/@render-center-x/@k/g' $target
sed -i 's/ render-center-x / k /g' $target
sed -i 's/@render-center-y/@l/g' $target
sed -i 's/ render-center-y / l /g' $target
sed -i 's/@player-x/@m/g' $target
sed -i 's/ player-x / m /g' $target
sed -i 's/@player-y/@n/g' $target
sed -i 's/ player-y / n /g' $target
sed -i 's/shift-x/x/g' $target
sed -i 's/shift-y/y/g' $target
sed -i 's/status-bar-row/z/g' $target
sed -i 's/(math/(p/g' $target
sed -i 's/ math/ p/g' $target
sed -i 's/move /c /g' $target
sed -i 's/ world-size/ i/g' $target
# stage 2: remove excess spaces
target=stage2.clj
cp stage1.clj $target
sed -i 's/ \+/ /g' $target
sed -i 's/ \\/\\/g' $target
sed -i 's/ (/(/g' $target
sed -i 's/) /)/g' $target
sed -i 's/ @/@/g' $target
sed -i 's/ {/{/g' $target
sed -i 's/} /}/g' $target
sed -i 's/ \[/\[/g' $target
sed -i 's/\] /\]/g' $target
sed -i 's/^ //g' $target
sed -i 's/(\([[:alpha:]]*\) "/(\1"/g' $target
sed -i 's/(ref "/(ref"/g' $target
sed -i 's/:else " "/:else" "/g' $target
# stage 3: remove newlines (and bit of excess spaces again)
target=src/r/c.clj
tr -d '\n' < stage2.clj > $target
sed -i 's/\(.*\);.*/\1/g' $target
sed -i 's/ \+/ /g' $target
sed -i 's/ \\/\\/g' $target
sed -i 's/ (/(/g' $target
sed -i 's/) /)/g' $target
sed -i 's/ @/@/g' $target
sed -i 's/ {/{/g' $target
sed -i 's/} /}/g' $target
sed -i 's/ \[/\[/g' $target
sed -i 's/\] /\]/g' $target
sed -i 's/^ //g' $target
sed -i 's/(str "/(str"/g' $target
sed -i 's/(ref "/(ref"/g' $target
sed -i 's/:else " "/:else" "/g' $target
