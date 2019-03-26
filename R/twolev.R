#' Container for simple documents with arbitrary numbers/shapes of records
#' @rdname dsutils
#' @export
setClass("DocSet", representation(
  kw2docs = "environment",
  docs2recs = "environment",
  docs2kw = "environment",
  titles = "character",
  doc_retriever = "function"))

#' constructor for DocSet
#' @param kw2docs an environment mapping keywords to documents
#' @param docs2recs an environment mapping document identifiers to records
#' @param docs2kw an environment mapping documents to keywords
#' @param doc_retriever a function that, given a document identifier, will produce the document
#' @export
DocSet = function(kw2docs, docs2recs, docs2kw, titles, doc_retriever) {
 new("DocSet", 
  kw2docs = kw2docs,
  docs2recs = docs2recs,
  docs2kw = docs2kw,
  titles = titles,
  doc_retriever = doc_retriever)
}

setMethod("show", "DocSet", function(object) {
 cat("ssrch DocSet resource:\n")
 cat(sprintf(" %d documents, %d records\n", length(unique(ls(
  envir=object@docs2kw))), length(unique(
    unlist(mget(ls(envir=object@docs2recs), envir=object@docs2recs))))))
 cat("some titles:\n  ")
 nt = names(object@titles)
 sh = paste0(substr(object@titles, 1, 55), "... (", nt, ")")
 cat(head(sh), sep="\n  ")
})

#' utilities for ssrch 
#' @rdname dsutils
#' @param sdata instance of srchData class
#' @export
kw2docs = function(sdata) slot(sdata, "kw2docs")
#' @rdname dsutils
#' @export
docs2kw = function(sdata) slot(sdata, "docs2kw")
#' @rdname dsutils
#' @export
docs2recs = function(sdata) slot(sdata, "docs2recs")
#' @rdname dsutils
#' @param string character(1) query string
#' @param obj instance of DocSet class
#' @export
searchDocs = function(string, obj, ...)
 {
 hits = grep(string, ls(envir=kw2docs(obj)), value=TRUE, ...)
 z = mget(hits, kw2docs(obj))
 l = vapply(z, length, integer(1))
 hits = rep(hits, l)
 data.frame(hits=hits, studies=unlist(z), stringsAsFactors=FALSE, row.names=NULL)
 }
#' @rdname dsutils
#' @export
retrieve_doc = function(x, obj, ...) {
 slot(obj, "doc_retriever")(x, ...)
}

can68retriever = function (docid) 
{
    docname = paste0(docid, ".csv")
    zpath = system.file("cancer_corpus/canc68.zip", package = "ssrch")
    con = unz(zpath, docname)
    read.csv(con, stringsAsFactors = FALSE)
}

