set -e

INPUT_DIR="$1"
OUTPUT_DIR="$2"
TEST_NAME="$3"

mkdir -p "${OUTPUT_DIR}"
rm -f "${OUTPUT_DIR}"/*

cp "${INPUT_DIR}/${TEST_NAME}".out "${OUTPUT_DIR}/${TEST_NAME}".out.expected
sort "${INPUT_DIR}/${TEST_NAME}".out > "${OUTPUT_DIR}/${TEST_NAME}".out.expected.sorted

cp "${INPUT_DIR}/${TEST_NAME}".err "${OUTPUT_DIR}/${TEST_NAME}".err.expected

for file in $(cd "${INPUT_DIR}" && find . -maxdepth 1 -name "*.csv"); do
    cp "${INPUT_DIR}/$file" "${OUTPUT_DIR}/${file}.expected"
    sort "${OUTPUT_DIR}/${file}.expected" > "${OUTPUT_DIR}/${file}.expected.sorted"
done

find "${INPUT_DIR}" -maxdepth 1 -name "*.csv" | wc -l > "${OUTPUT_DIR}/num.expected"
