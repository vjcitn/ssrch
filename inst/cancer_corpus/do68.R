library(ssrch)
myc = dir(patt="csv$")
d1 = parseDoc(myc[1])
for (d in myc[-1]) parseDoc(d, d1)
docset_cancer68 = d1
load("../../data/titles68.rda")
docset_cancer68@titles = titles68
can68retriever = function (docid) 
{
    docname = paste0(docid, ".csv")
    zpath = system.file("cancer_corpus/canc68.zip", package = "ssrch")
    con = unz(zpath, docname)
    read.csv(con, stringsAsFactors = FALSE)
}
docset_cancer68@doc_retriever = can68retriever
save(docset_cancer68, file="docset_cancer68.rda")
