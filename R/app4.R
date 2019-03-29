#' interactive app for ssrch DocSet instances
#' @import shiny
#' @param docset an instance of DocSet
#' @return Simply starts an app.
#' @examples
#' if (interactive()) {
#'   oask = options()$example.ask
#'   options(example.ask=FALSE)
#'   n1 = try(docset_searchapp(ssrch::docset_cancer68))
#'   str(n1)
#'   options(example.ask=oask)
#' }
#' @export
docset_searchapp = function(docset) {
 docs = docset # ssrch::docset_cancer68
 titles = slot(docset, "titles") # ssrch::docset_cancer68@titles
 urls = slot(docset, "urls") 
#
# order keywords so that those with alphabetic prefix
# precede those with special characters or numbers
#
 allkw = sort(unique(ls(envir=kw2docs(docs))))
 ini = substr(allkw,1,1)
 fullinds = seq_len(length(allkw))
 preferred = grep("[A-Za-z]", ini)
 spec = setdiff(fullinds, preferred)
 allkw = allkw[c(preferred, spec)]
#
# done
#
 accumtitles = NULL
 accumTokens = NULL
 ui = fluidPage(
  sidebarLayout(
   sidebarPanel(
    helpText(h3("ssrch")),
    helpText("Simple full text search over genomic metadata"),
    selectInput("main", "Search studies for",
     choices = allkw, selected="Triple"),
    selectInput("keep", "Select study accession numbers for metadata retrieval",
        choices=names(titles), multiple=TRUE),
    downloadButton("downloadData", "download list of data.frames"),
    actionButton("cleartabs", "Clear tabs."),
    actionButton("cleartitles", "Clear titles."),
    actionButton("stopBtn", "Stop app."),
           width=3
    ),
   mainPanel(
    helpText("Tabs will appear for studies using selected terms in metadata"),
    helpText("Click on tab to see sample.attributes for all experiments in the study, derived with SRAdbV2"),
    tabsetPanel(id="tabs",
     tabPanel("titles", target="titles",
      dataTableOutput("titleTable")
     ),
     tabPanel("about",
      helpText("This app demonstrates an approach to supporting full text search over genomic metadata recorded in the NCBI SRA."),
      helpText("A snapshot of cancer-related metadata was retrieved
in March 2019 using the Omicidx system of Sean Davis of NCI."),
      helpText("ssrch::ctxsearch uses a convenience subsample of the cancer-related metadata.  The subsample was indexed in a DocSet structure available as ssrch::docset_cancer68.  A view of this object is shown below."),
      verbatimTextOutput("objdesc"),
      helpText("Special methods for organizing and searching the metadata are warranted by the fact that diverse field sets and value sets are used across and even within studies.  Retrieval and partial normalization of metadata from SRAdbV2 is conducted using code in the HumanTranscriptomeCompendium package, available at github.com/vjcitn."),
      helpText("The software stack underlying ssrch is:"),
      verbatimTextOutput("sessInf")
     )
    )
   )
  )
 )

 server = function(input, output, session) {
  output$objdesc = renderPrint( docs )
#
# retrieve requested documents
#
#  getTabs = reactive({
#    z = searchDocs(input$main, docs, ignore.case=TRUE)
#    lapply(z$docs, function(x) retrieve_doc(x, docs))
#    })
#
# render a table of titles of selected documents
#
#  output$titleTable = renderDataTable({
  buildTitleTable = reactive({
   z = searchDocs(input$main, docs, ignore.case=TRUE)
   if (nrow(z)>1 && sum(dd <- duplicated(z$docs))>0) {
      sz = split(z, z$docs)
      kp = sapply(sz, function(x) which.max(nchar(x$hits)))
      for (i in 1:length(sz)) sz[[i]] = sz[[i]][kp[i],,drop=FALSE]
      z = do.call(rbind, sz)
      }
   if (is.null(accumtitles)) accumtitles <<- cbind(z, title=titles[z$docs])
   else accumtitles <<- rbind(accumtitles, cbind(z, title=titles[z$docs]))
   d = which(duplicated(accumtitles$docs))
   if (length(d)>0) accumtitles <<- accumtitles[-d,]
   mkl = function(x) sprintf("<a href=%s>%s</a>",x,gsub(".*=", "", x))
   if (length(urls)>0) accumtitles = cbind(pmid=mkl(urls[accumtitles$docs]),
     accumtitles)
   accumtitles
  })
#
# append titles, tabs as requested
#
  tabStack = NULL
  observeEvent(input$main, {
    z = searchDocs(input$main, docs, ignore.case=TRUE)
    lapply(rev(unique(z$docs)), function(x) {
      tabStack <<- c(tabStack, x)
      insertTab("tabs", tabPanel(x, {
        renderDataTable(retrieve_doc(x, docs))}, id=x),
        target="titles", position="after")})
    accumTokens <<- c(accumTokens, accumtitles$docs)
    output$titleTable = renderDataTable( buildTitleTable(), escape=FALSE )
    })
  observeEvent(input$cleartabs, {
    showNotification("After clearing you must change the query string or displays will not update.")
    for (i in tabStack) removeTab("tabs", target=i) 
    tabStack <<- NULL
    })
  observeEvent(input$cleartitles, {
    showNotification("After clearing you must change the query string or displays will not update.")
    accumtitles <<- NULL
    output$titleTable = renderDataTable( data.frame() ) #buildTitleTable() )
    })

#  observeEvent(input$main, {
#    accumTokens <<- c(accumTokens, accumtitles$docs)
#    output$newnew = renderUI(selectInput("keep", "keep",
#        choices=accumTokens, selected=accumTokens, multiple=TRUE))
#    })
     observeEvent(input$stopBtn, {
       ans = NULL
       if (length(input$keep)>0) {
          ans = lapply(input$keep, function(x) retrieve_doc(x, docs))
          names(ans) = input$keep
          }
       stopApp(returnValue=ans)
       })
     output$sessInf = renderPrint( sessionInfo() )

     output$downloadData <- downloadHandler(
              filename = function() {
                paste('listOfDFs-', Sys.Date(), '.rds', sep='')
                },  
              content = function(con) {
                ans = lapply(input$keep, function(x) retrieve_doc(x, docs))
                names(ans) = input$keep
                saveRDS(ans, file=con)
                }, contentType="application/octet-stream"
               )    

 }
 runApp(list(ui=ui, server=server))
}
