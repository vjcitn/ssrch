#' parse a document and place content in a DocSet
#' @param csv a character(1) CSV file path
#' @param rec_id_field character(1) field in CSV identifying records
#' @param exclude_fields character vector of fields to ignore while parsing
#' @param title character(1) document title
#' @param cleanFields list of regular expressions identifying fields to ignre
#' @export
parseDoc = function(csv, DocSetInstance=NULL, 
    rec_id_field = "experiment.accession",
    exclude_fields = c("study.accession"),
    title = NA_character_,
    cleanFields = list("..*id$", ".name$", "_name$", "checksum",
       "isolate", "filename", "^ID$", "barcode", "Sample.Name")) {
stopifnot(length(csv)==1, is.atomic(csv))
#
# set up 3 environments: 
# record to keyword, doc to records, doc to keyword
#
 if (is.null(DocSetInstance)) {
   DocSetInstance = new("DocSet",
      kw2docs = new.env(hash=TRUE),
      docs2recs = new.env(hash=TRUE),
      docs2kw = new.env(hash=TRUE),
      doc_retriever = function() NULL)
   }
#
# import CSV check that rec_id_field is present
#
 dat = read.csv(csv, stringsAsFactors=FALSE)
 nn = names(dat)
 stopifnot(rec_id_field %in% nn)

#
# document name
gr = grep(".csv", csv)
if(length(gr)<1) message(".csv not found in input filename")
docname = gsub(".csv", "", csv)
#

 todrop = unique(unlist(lapply(cleanFields, function(x) grep(x, nn))))
 if (length(todrop)>0) dat = dat[,-todrop]

# bind rec_id_field id as rownames
# remove duplicate records
#

 dd = which(duplicated(dat[[rec_id_field]])) 
 if (length(dd)>0) {
   message(sprintf("duplicates found in %s, dropped", rec_id_field))
   dat = dat[-dd,]
   }   
 rownames(dat) = dat[[rec_id_field]]

#
# allstr will consist of all strings present in table, including
# field names
#
 fldnames = setdiff(names(dat), 
    c(exclude_fields, rec_id_field))
 recs = dat[[rec_id_field]]
 docset_identifiers = c(docname, dat[[rec_id_field]])
 allstr = setdiff(unique(c(fldnames, unlist(lapply(dat, force)))), 
        docset_identifiers)
 nc = nchar(allstr)
 bad = which(nc==0)
 if (length(bad)>0) allstr=allstr[-bad]
# clean out dates and numbers with commas
 cln = function(x) { dr = grep("....-..-..|.*...,...", x); if (length(dr)>0) x = x[-dr]; x }
 allstr = cln(allstr)
#
# some columns are numeric data.  we will not be searching for numeric constants
# such information can be used downstream
#
 suppressWarnings({isnum <- which(!is.na(as.numeric(allstr)))})
 if (length(isnum)>0) allstr = allstr[-isnum]
#
 dat = dat[,-c(1,2)]
 alltok = setdiff(unlist(strsplit(allstr, " ")), docset_identifiers)
 alltok = setdiff(alltok, stopWords)
#
# title handling
#
 titleTokens = setdiff(strsplit(title, " ")[[1]], stopWords)
 alltok = c(alltok, titleTokens)
 if (any(is.na(alltok))) alltok = alltok[-which(is.na(alltok))]
 nc = nchar(alltok)
 bad = which(nc==0)
 if (length(bad)>0) alltok=alltok[-bad]
 suppressWarnings({ isnum <- which(!is.na(as.numeric(alltok))) })
 if (length(isnum)>0) alltok = alltok[-isnum]
 alltok = cln(alltok)
#
# when we contribute to an environment we first check that our
# current key is not present, if it is, we just c() new material
# to the available value
#
# step 1 -- bind available strings and tokens into docs2kw
#
#      docs2kw = new.env(hash=TRUE),
#      kw2docs = new.env(hash=TRUE),
#      docs2recs = new.env(hash=TRUE),
#
 vals = unique(c(allstr, alltok))
 curDocs2kw = try(get(docname, env=docs2kw(DocSetInstance)), silent=TRUE)
 if (inherits(curDocs2kw, "try-error")) curDocs2kw=NULL
 assign(docname, c(curDocs2kw, vals), env=docs2kw(DocSetInstance))
#
# step 2 -- kw2docs
 curKw = intersect(vals, ls(envir=kw2docs(DocSetInstance)))
 newkeys = setdiff(vals, curKw)
 for (k in curKw) assign(k, unique(c(
           get(k, envir=kw2docs(DocSetInstance)), docname)), 
            envir=kw2docs(DocSetInstance))
 for (k in newkeys) assign(k, docname,
            envir=kw2docs(DocSetInstance))
# step 3 doc to record mapping
 assign(docname, unique(recs), envir=
           docs2recs(DocSetInstance))
 DocSetInstance
}
