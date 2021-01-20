set -e

OUTPUT_DIR="$1"
TEST_NAME="$2"

sort "${OUTPUT_DIR}/${TEST_NAME}.out" > "${OUTPUT_DIR}/${TEST_NAME}.out.sorted"
diff "${OUTPUT_DIR}/${TEST_NAME}.out.sorted" "${OUTPUT_DIR}/${TEST_NAME}.out.expected.sorted"

diff "${OUTPUT_DIR}/${TEST_NAME}.err" "${OUTPUT_DIR}/${TEST_NAME}.err.expected"
