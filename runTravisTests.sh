#!/bin/bash

rcode="tfile <- '~/testres.out'; capture.output(res<-devtools::test(), file = tfile); out <- readLines(tfile); cat(out, sep = '\n'); "
rcode+="n.fail <- as.numeric(sub('Failed:[[:space:]]', '', out[grep('Failed:[[:space:]]', out)])); "
rcode+="write.csv(as.data.frame(res), file='test_results.csv'); "
rcode+="quit(status=n.fail, save='no');"
Rscript --default-packages='datasets,utils,grDevices,graphics,stats,methods' -e "$rcode";
