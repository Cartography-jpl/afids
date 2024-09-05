#! /bin/bash
# 
# This compare the results of a afids unit test run with
# expected results. Note that testing in vicar is really limited,
# we can only check the text results of a run. There can be 
# differences just because of different runs, roundoff differences
# between machines, etc. This script tries to filter this out.
#
# Note that in some cases there may be more than one acceptable output.
# For example, a build on the Mac or a different version of the compilers
# might give slightly different results. So we check against files like
# tst.out, tst.out.2, tst.out.3 etc. If *any* if these files match, then
# we consider the test a success.
#
# So if you have unit test failures on a new system and you have verified
# that the actual results are ok, then you can add a new file to the 
# expected_test_results directory.

# Where we are running from
scriptpath="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

for i in "$2"*; do
    ${scriptpath}/tst_compare_diff_command.sh $1 $i > /dev/null 2>&1
    res=$?
    if [ $res -eq 0 ]; then
	exit 0
    fi
done
# Run diff a second time, this time printing out the results. Arbitrary to pick the first one
# to compare to, but we need to choose something. This will give an analyst something to look
# at.
${scriptpath}/tst_compare_diff_command.sh $1 $2
exit 1
