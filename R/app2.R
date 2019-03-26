#' ssrch demo with metadata documents from 68 cancer transcriptomics studies
#' @note The metadata were derived by extracting sample.attributes
#' fields from a search with github.com/seandavi/SRAdbV2.  The
#' sample.attributes content varies between studies and sometimes
#' between experiments within studies.  The field sets were
#' unified with the sampleAtts function of github.com/vjcitn/HumanTranscriptomeCompendium.  After unification records were stacked and CSVs were
#' written.
#' @examples
#' if (interactive()) {
#'   oask = options()$example.ask
#'   options(example.ask=FALSE)
#'   try(ctxsearch())
#'   options(example.ask=oask)
#' }
#' @export
ctxsearch = function() {
 require("shiny")
 docs = ssrch::cancer68docs
 titles = ssrch::titles68
 allkw = sort(unique(ls(envir=kw2stud(docs))))
 accumtitles = NULL
 ui = fluidPage(
  sidebarLayout(
   sidebarPanel(
    helpText(h3("ssrch")),
    helpText("simple metadata search engine"),
    verbatimTextOutput("objdesc"),
    selectInput("main", "search studies for",
     choices = allkw, selected="mutations"), width=3
    ),
   mainPanel(
    helpText("tabs will appear for studies using selected terms in metadata"),
    tabsetPanel(id="tabs",
     tabPanel("titles",
      dataTableOutput("dat")
     )
    )
   )
  )
 )

 server = function(input, output) {
  output$objdesc = renderPrint( docs )
  getTabs = reactive({
    z = searchStudies(input$main, docs, ignore.case=TRUE)
    lapply(z$studies, function(x) retrieve_doc(x, docs))
    })
  output$dat = renderDataTable({
   z = searchStudies(input$main, docs, ignore.case=TRUE)
   if (nrow(z)>1 && sum(dd <- duplicated(z$studies))>0) {
      sz = split(z, z$studies)
print(sz)
      kp = sapply(sz, function(x) which.max(nchar(x$hits)))
      for (i in 1:length(sz)) sz[[i]] = sz[[i]][kp[i],,drop=FALSE]
      z = do.call(rbind, sz)
      }
   if (is.null(accumtitles)) accumtitles <<- cbind(z, title=titles[z$studies])
   else accumtitles <<- rbind(accumtitles, cbind(z, title=titles[z$studies]))
   accumtitles
  })
  observeEvent(input$main, {
    z = searchStudies(input$main, docs, ignore.case=TRUE)
    lapply(unique(z$studies), function(x) {
      appendTab("tabs", tabPanel(x, {
        renderDataTable(retrieve_doc(x, docs))}))})
    })
 }
 runApp(list(ui=ui, server=server))
}
