#!/bin/sh
# stage 1: change identifiers
target=stage1.clj
cp original.clj $target
# stage 2: remove excess spaces
target=stage2.clj
cp stage1.clj $target
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
# stage 3: remove newlines (and bit of excess spaces again)
target=src/roguehike/core.clj
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
