set -e

OUTPUT_DIR="$1"

ls "${OUTPUT_DIR}"/*.csv | wc -l > "${OUTPUT_DIR}/num.generated"
ls "${OUTPUT_DIR}"/*.csv.expected.sorted | wc -l > "${OUTPUT_DIR}/num.expected"

diff "${OUTPUT_DIR}/num.generated" "${OUTPUT_DIR}/num.expected"

for file in $(cd "${OUTPUT_DIR}" && ls *.csv); do
    sort "${OUTPUT_DIR}/${file}" > "${OUTPUT_DIR}/${file}.generated.sorted"
    diff "${OUTPUT_DIR}/${file}.generated.sorted" "${OUTPUT_DIR}/${file}.expected.sorted"
done
