#! /bin/bash
#
# Diff command to use with tst_compare.sh. This ignores lines with text we
# expect to change from one run to the next, or harmlessly vary when run
# on different machines. We put this in its own script just so we don't need
# to repeat the rather long -I list.

diff -I 'UNIQUE:' -I 'USING SCRATCH FILE:' -I 'F2 version' -I 'Task:' -I 'R2LIB set to' -I 'convergence =' -I '----TASK:' -I 'disable-log' -I 'X86-MACOSX' -I 'X86-LINUX' -I 'tst$*' -I 'ush ln -s' -I 'exit' -I 'slogoff' -I 'etop02nobath.hlf' -I 'LBLSIZE=' -I 'BUFSIZ=' -I 'HOST=' -I 'plotxxx' $1 $2
