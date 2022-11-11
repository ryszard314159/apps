
# customize your pass.js and pass.html

for x in js html; do
sed -e 's/DIGITS/<your-digit-set>/' \
    -e 's/LOWER_CHARS/<your-lower-chars-set>/' \
    -e 's/DEFAULT_LENGTH/<your-default-length>/' \
    -e 's/DEFAULT_PREFIX/<your-default-prefix>/' \
    pass.$x.template > pass.$x
done
