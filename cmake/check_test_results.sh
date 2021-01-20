set -e

OUTPUT_DIR="$1"

find "${OUTPUT_DIR}" -maxdepth 1 -name "*.csv.expected.sorted" | wc -l > "${OUTPUT_DIR}/num.generated"

diff "${OUTPUT_DIR}/num.generated" "${OUTPUT_DIR}/num.expected"

for file in $(cd "${OUTPUT_DIR}" && find . -maxdepth 1 -name "*.csv"); do
    sort "${OUTPUT_DIR}/${file}" > "${OUTPUT_DIR}/${file}.generated.sorted"
    diff "${OUTPUT_DIR}/${file}.generated.sorted" "${OUTPUT_DIR}/${file}.expected.sorted"
done
