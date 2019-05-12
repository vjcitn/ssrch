
library(ssrch)

#Symbols

# [1] "ctxsearch"       "docs2kw"         "docs2recs"       "DocSet"         
# [5] "docset_cancer68" "kw2docs"         "parseDoc"        "retrieve_doc"   
# [9] "searchDocs"      "titles68"       

context("basic objects")
test_that("key object classes have predicted properties", {
  data(docset_cancer68)
  expect_true(inherits(docset_cancer68, "DocSet"))
  data(titles68)
  expect_true(inherits(titles68,"character"))
  expect_true(inherits(docs2recs(docset_cancer68), "environment"))
  expect_true(all(dim(docset_cancer68@doc_retriever("SRP057500"))==c(290,10)))
})

context("updating a DocSet")
test_that("can update a DocSet in various ways", {
 ds1 = DocSet()
 expect_true(length(slot(ds1, "titles"))==0)
 myob = ssrch::docset_cancer68
 td = tempdir()
 alld = ls(envir=docs2kw(myob))
 r1 = retrieve_doc(alld[1], myob)
 expo = write.csv(r1, paste0(td, "/expo.csv"))
 expo2 = write.csv(r1, paste0(td, "/expo2.csv"))
 ds2 = parseDoc(paste0(td, "/expo.csv"), doctitle=ssrch::titles68[alld[1]])
 expect_true(length(ls(envir=kw2docs(ds2)))==349)
 expect_true(length(slot(ds2, "titles"))==1)
 # now test that content in ds2 is updated to a new DocSet with a second parseDoc call
 ds3 = parseDoc(paste0(td, "/expo2.csv"), DocSetInstance=ds2, doctitle="test")
 expect_true(length(slot(ds3, "titles"))==2)
 # at this point, ds2's environment content has also been altered ... we do not want
 # to use it, in general
 # we can update ds1
 ds4 = parseDoc(paste0(td, "/expo2.csv"), DocSetInstance=ds1, doctitle="test")
 expect_true(length(slot(ds4, "titles"))==1)
})

