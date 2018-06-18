#!/bin/bash

echo "tfile <- 'testres.out'" > test.R
echo "capture.output(res<-devtools::test(), file = tfile)" >> test.R
echo "out <- readLines(tfile); cat(out, sep = '\n')" >> test.R
echo "n.fail <- as.numeric(sub('Failed:[[:space:]]', '', out[grep('Failed:[[:space:]]', out)]))" >> test.R
echo "write.csv(as.data.frame(res), file='test_results.csv')" >> test.R
echo "quit(status=n.fail, save='no')" >> test.R
Rscript --default-packages='datasets,utils,grDevices,graphics,stats,methods' test.R
