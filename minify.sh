#!/bin/sh
# stage 1: change identifiers
cp original.clj stage1.clj
# stage 2: remove excess spaces
cp stage1.clj stage2.clj
sed -i 's/\(.*\);.*/\1/' stage2.clj
sed -i 's/ \+/ /g' stage2.clj
sed -i 's/ \\/\\/g' stage2.clj
sed -i 's/ (/(/g' stage2.clj
sed -i 's/) /)/g' stage2.clj
sed -i 's/ @/@/g' stage2.clj
sed -i 's/} /}/g' stage2.clj
sed -i 's/ \[/\[/g' stage2.clj
sed -i 's/\] /\]/g' stage2.clj
# stage 3: remove newlines (and bit of excess spaces again)
tr -d '\n' < stage2.clj > src/roguehike/core.clj
sed -i 's/\(.*\);.*/\1/' src/roguehike/core.clj
sed -i 's/ \+/ /g' src/roguehike/core.clj
sed -i 's/ \\/\\/g' src/roguehike/core.clj
sed -i 's/ (/(/g' src/roguehike/core.clj
sed -i 's/) /)/g' src/roguehike/core.clj
sed -i 's/ @/@/g' src/roguehike/core.clj
sed -i 's/} /}/g' src/roguehike/core.clj
sed -i 's/ \[/\[/g' src/roguehike/core.clj
sed -i 's/\] /\]/g' src/roguehike/core.clj
