
library(ssrch)
# extended field cleaning
buildIndices = function(csvs, cleanFields, verbose=TRUE) {
 sind = csv2envs(csvs[1], cleanFields=cleanFields)
 for (i in csvs[-1]) {
   csv2envs(i, sind, cleanFields=cleanFields)
   if (verbose) cat(".")
   }
 sind
}

cf = list("*.id", "filename", "checksum", "isolate", "ID", 
   "batch", "barcode",
  "sample_name", "Sample.Name", "sample.pool.name")
allc = dir(patt="csv$")
bb = buildIndices(allc, cleanFields=cf)

