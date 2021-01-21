set -e

OUTPUT_DIR="$1"
INPUT_DIR="$2"
EXTRA_DATA="$3"
BINARY="$4" #BINARY will be set to gzip or sqlite3

find "${OUTPUT_DIR}" -maxdepth 1 -name "*.csv.expected.sorted" | wc -l > "${OUTPUT_DIR}/num.generated"

diff "${OUTPUT_DIR}/num.generated" "${OUTPUT_DIR}/num.expected"

for file in $(cd "${OUTPUT_DIR}" && find . -maxdepth 1 -name "*.csv"); do
    sort "${OUTPUT_DIR}/${file}" > "${OUTPUT_DIR}/${file}.generated.sorted"
    diff "${OUTPUT_DIR}/${file}.generated.sorted" "${OUTPUT_DIR}/${file}.expected.sorted"
done

if [ ! -z ${EXTRA_DATA} ]; then
    for file in $(cd "${OUTPUT_DIR}" && find . -maxdepth 1 -name "*.output"); do
        if [ ${EXTRA_DATA} = "gzip" ]; then
            # Strip the .gz.output suffix.  There should be a corresponding .csv file
            # in the input directory
            EXTRA_FILE="${OUTPUT_DIR}/${file%%.gz.output}"
            "${BINARY}" -d -c "${OUTPUT_DIR}/$file" > "${EXTRA_FILE}.generated.unsorted"
        elif [ ${EXTRA_DATA} = "sqlite3" ]; then
            # Strip the .sqlite.output suffix.  There should be a corresponding .csv file
            # in the input directory
            EXTRA_FILE="${OUTPUT_DIR}/${file%%.sqlite.output}.csv"
            "${BINARY}" -batch "${OUTPUT_DIR}/$file" -init "${INPUT_DIR}/$file.script" "" > "${EXTRA_FILE}.generated.unsorted"
        else
            echo "Unknown processing type"
            exit 1
        fi

        echo PROCESSING ${EXTRA_FILE}
        sort "${EXTRA_FILE}.generated.unsorted" > "${EXTRA_FILE}.generated.sorted"
        diff "${EXTRA_FILE}.generated.sorted" "${EXTRA_FILE}.expected.sorted"
    done
fi
