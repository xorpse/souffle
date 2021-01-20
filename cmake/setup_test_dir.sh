set -e

INPUT_DIR="$1"
OUTPUT_DIR="$2"
TEST_NAME="$3"

mkdir -p "${OUTPUT_DIR}"
rm -f "${OUTPUT_DIR}"/*

for file in $(cd "${INPUT_DIR}" && ls *.csv "${TEST_NAME}.out"); do
    cp "${INPUT_DIR}/$file" "${OUTPUT_DIR}/${file}.expected"
    sort "${OUTPUT_DIR}/${file}.expected" > "${OUTPUT_DIR}/${file}.expected.sorted"
done

cp "${INPUT_DIR}/${TEST_NAME}".err "${OUTPUT_DIR}/${TEST_NAME}".err.expected
