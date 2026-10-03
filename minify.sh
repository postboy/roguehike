#!/bin/sh
cp original.clj stage1.clj
cp stage1.clj src/roguehike/core.clj
sed -i 's/\(.*\);.*/\1/' src/roguehike/core.clj
tr -d '\n' > src/roguehike/tmp.clj < src/roguehike/core.clj
mv src/roguehike/tmp.clj src/roguehike/core.clj
sed -i 's/ \+/ /g' src/roguehike/core.clj
sed -i 's/ (/(/g' src/roguehike/core.clj
