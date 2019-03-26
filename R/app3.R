#' ssrch demo with metadata documents from 68 cancer transcriptomics studies
#' @import shiny
#' @note The metadata were derived by extracting sample.attributes
#' fields from a search with github.com/seandavi/SRAdbV2.  The
#' sample.attributes content varies between studies and sometimes
#' between experiments within studies.  The field sets were
#' unified with the sampleAtts function of github.com/vjcitn/HumanTranscriptomeCompendium.  After unification records were stacked and CSVs were
#' written.
#' @return Simply starts an app.
#' @examples
#' if (interactive()) {
#'   oask = options()$example.ask
#'   options(example.ask=FALSE)
#'   try(ctxsearch2())
#'   options(example.ask=oask)
#' }
#' @export
ctxsearch = function() {
 docs = ssrch::docset_cancer68
 titles = ssrch::docset_cancer68@titles
 allkw = sort(unique(ls(envir=kw2docs(docs))))
 accumtitles = NULL
 accumTokens = NULL
 ui = fluidPage(
  sidebarLayout(
   sidebarPanel(
    helpText(h3("ssrch")),
    helpText("simple metadata search engine"),
    selectInput("main", "search studies for",
     choices = allkw, selected="Triple"), 
    uiOutput("newnew"),
    downloadButton("downloadData", "download list of data.frames"),
           width=3
    ),
   mainPanel(
    helpText("tabs will appear for studies using selected terms in metadata"),
    tabsetPanel(id="tabs",
     tabPanel("titles", target="titles",
      dataTableOutput("titleTable")
     ),
     tabPanel("about",
      verbatimTextOutput("objdesc")
     )
    )
   )
  )
 )

 server = function(input, output) {
  output$objdesc = renderPrint( docs )
#
# retrieve requested documents
#
  getTabs = reactive({
    z = searchDocs(input$main, docs, ignore.case=TRUE)
    lapply(z$docs, function(x) retrieve_doc(x, docs))
    })
#
# render a table of titles of selected documents
#
  output$titleTable = renderDataTable({
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
   accumtitles
  })
#
# append tabs as requested
#
  observeEvent(input$main, {
    z = searchDocs(input$main, docs, ignore.case=TRUE)
    lapply(unique(z$docs), function(x) {
      insertTab("tabs", tabPanel(x, {
        renderDataTable(retrieve_doc(x, docs))}),target="titles", position="after")})
    })

  observeEvent(input$main, {
    accumTokens <<- c(accumTokens, accumtitles$docs)
    output$newnew = renderUI(selectInput("keep", "keep",
        choices=accumTokens, selected=accumTokens, multiple=TRUE))
    })

     output$downloadData <- downloadHandler(
              filename = function() {
                paste('listOfDFs-', Sys.Date(), '.rds', sep='')
                },  
              content = function(con) {
                ans = lapply(input$keep, function(x) retrieve_doc(x, docs))
                saveRDS(ans, file=con)
                }, contentType="application/octet-stream"
               )    

 }
 runApp(list(ui=ui, server=server))
}
