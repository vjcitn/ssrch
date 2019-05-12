#' Container for simple documents with arbitrary numbers/shapes of records
#' @import methods
#' @rdname dsutils
#' @examples
#' getClass("DocSet")
#' @export
setClass("DocSet", representation(
  kw2docs = "environment",
  docs2recs = "environment",
  docs2kw = "environment",
  titles = "character",
  urls = "character",
  doc_retriever = "function"))

#' constructor for DocSet
#' @param kw2docs an environment mapping keywords to documents
#' @param docs2recs an environment mapping document identifiers to records
#' @param docs2kw an environment mapping documents to keywords
#' @param titles a named character vector with titles; names are document identifiers
#' @param urls a named character vector with document-associated URLs; names are document identifiers
#' @param doc_retriever a function that, given a document identifier, will produce the document
#' @return instance of DocSet
#' @note Titles must be bound in post-hoc.  parseDoc produces
#' data including parsed titles but does not bind the title into
#' the resulting object.  
#' @examples
#' getClass("DocSet")
#' @export
DocSet = function(kw2docs = new.env(hash=TRUE), 
   docs2recs=new.env(hash=TRUE), docs2kw=new.env(hash=TRUE), 
   titles=character(), urls=character(), doc_retriever=function(...) NULL) {
 new("DocSet", 
  kw2docs = kw2docs,
  docs2recs = docs2recs,
  docs2kw = docs2kw,
  titles = titles,
  urls = urls,
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
#' @return an environment
#' @export
kw2docs = function(sdata) slot(sdata, "kw2docs")
#' @rdname dsutils
#' @return an environment
#' @export
docs2kw = function(sdata) slot(sdata, "docs2kw")
#' @rdname dsutils
#' @return an environment
#' @export
docs2recs = function(sdata) slot(sdata, "docs2recs")
#' @rdname dsutils
#' @param string character(1) query string
#' @param obj instance of DocSet class
#' @param \dots passed to base::grep
#' @return a data.frame with tokens queried (hits) and associated document ids (docs)
#' @export
searchDocs = function(string, obj, ...)
 {
 hits = grep(string, ls(envir=kw2docs(obj)), value=TRUE, ...)
 z = mget(hits, kw2docs(obj))
 l = vapply(z, length, integer(1))
 hits = rep(hits, l)
 data.frame(hits=hits, docs=unlist(z), stringsAsFactors=FALSE, row.names=NULL)
 }
#' @rdname dsutils
#' @param x character(1) document identifier
#' @return result of calling obj@doc_retriever on arguments x, ...
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

