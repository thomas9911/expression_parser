COMMAND="cargo run -q --example file"
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

function run_single_test() {
  file=$1
  expected=$2

  output=$($COMMAND $file)
  [ "$output" = "$expected" ]
  print_success "$?" "$file" "$output" "$expected"
}

function print_success() {
  if [[ "$1" != 0 ]]; then
    echo -e "${RED}Error:${NC} '$2' ${RED}failed${NC} with '$3'; expected '$4'"
  else
    echo -n -e "${GREEN}.${NC}"
  fi
}

function run_test() {
  case $1 in

  ./examples/file/script.txt)
    run_single_test $1 "123"
    ;;

  ./examples/file/recursion.txt)
    run_single_test $1 "STACKOVERFLOW"
    ;;

  ./examples/file/sort.txt)
    run_single_test $1 "[ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14 ]"
    ;;

  ./examples/file/import.txt)
    run_single_test $1 "\"testing 123\""
    ;;

  ./examples/file/import_from.txt)
    run_single_test $1 "null"
    ;;

  ./examples/file/secant_method.txt)
    run_single_test $1 "true"
    ;;

  *)
    echo "$1 does not have an output"
    ;;
  esac
}

if [ "skip" != "$1" ]; then
  cargo test -q
  cargo test -q --all-features
fi

echo "run file examples"
for FILE in ./examples/file/*; do
  run_test "$FILE"
done
echo
echo "done"
