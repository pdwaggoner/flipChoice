#!/bin/bash

if [ -d tests/testthat ]; then
    echo "tfile <- tempfile()" > test.R
    echo "capture.output(res<-devtools::test(), file = tfile)" >> test.R
    echo "out <- readLines(tfile)" >> test.R
    echo "cat(out, sep = '\n')" >> test.R
    echo "n.fail <- as.numeric(sub('Failed:[[:space:]]', '', out[grep('Failed:[[:space:]]', out)]))" >> test.R
    echo "write.csv(as.data.frame(res), file='test_results.csv')" >> test.R
    echo "quit(status = !identical(n.fail, 0), save='no')" >> test.R
    Rscript --default-packages="datasets,utils,grDevices,graphics,stats,methods" test.R
    exit $?
fi
exit 0
