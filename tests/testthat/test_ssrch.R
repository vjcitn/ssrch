
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
