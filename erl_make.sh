echo "BUILD COMMON"
./common/erl_make.sh
echo "BUILD INTERACTIVE"
./interactive/erl_make.sh
echo "BUILD NOTIFIER"
./notifier/erl_make.sh
