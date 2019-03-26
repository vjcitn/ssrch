#' @export
tryss = function() {
data("titles68")
data(cancer68docs)
nstud = length(ls(cancer68docs@studenv))
nexp = length(ls(cancer68docs@expenv))
ui = fluidPage(
  sidebarLayout(
   sidebarPanel(
     helpText(h3("Human Transcriptome Compendium study harvester: cancer prototype")),
     helpText(sprintf("This app provides access to annotation and
quantification from %d studies (%d experiments) for which a 
cancer-related keyword was found in study title.", nstud, nexp)),
     helpText("Sample-level data is derived from a March 2019 snapshot of all human transcriptomic studies with
metadata available through SRAdbV2."),
     helpText("Field sets and field names vary across studies and even within studies.  Field sets
were lightly harmonized using the sampleAttr function of the HumanTranscriptomeCompendium package."),
     helpText("Use the search field to query the metadata snapshot for studies of interest.
Type the study identifier in the 'add to cart' field and press return to save the study for
retrieval."),
     selectInput("a", "Search all sample.attribute fields", choices=c(ls(cancer68docs@kwexenv)), 
       selected="triple-negative breast cancer (TNBC)", multiple=FALSE),
     selectInput("cart", "add to cart", choices=ls(cancer68docs@studenv),
       selected="SRP066982", multiple=TRUE) #,
#     downloadButton("downloadData", "get SummarizedExperiment"),
#     actionButton("btnSend", "Stop app")
     ),
   mainPanel(
    tabsetPanel(id = "tabs",
     tabPanel("basics",
      helpText(h4("Studies in cart")),
      tableOutput("b"),
      #helpText("studies"),
      #verbatimTextOutput("c"),
      helpText(h4("All sample-level information in current study (from SRAdbV2)")),
      dataTableOutput("curtitle"),
      dataTableOutput("e")
      ),
     tabPanel("all studies", dataTableOutput("alltitles")),
     tabPanel("about",
      helpText("Annotation derived from Sean Davis' ", a(href="https://api-omicidx.cancerdatasci.org/sra/1.0/ui/", "Omicidx API.")),
      helpText("The HumanTranscriptomeCompendium package (at github: vjcitn) utilizes Sean Davis' BigRNA project to create
images of 181000 RNA-seq studies, retrieve in 2017 from NCBI SRA, and uniformly
processed by the ", a(href="https://combine-lab.github.io/salmon/", "salmon pipeline.")),
      helpText("HumanTranscriptomeCompendium's `htx_load()` provides limited metadata about the samples in the
compendium.  Sean's SRAdbV2 package provides high-resolution metadata for
almost all samples in the compendium.  (Some samples simply lack
usable metadata entries at SRA.)  The limited metadata is provided in the colData
of the SummarizedExperiment returned by `htx_load()`. 
A major limitation is the lack of any information
on experimental state of assayed samples."),
      helpText("Sample-level metadata available from SRAdbV2 reflects the complex structure of information
in the NCBI SRA metadata store.  Fields and value sets vary between studies and
can vary between experiments within studies.  The HumanTranscriptomeCompendium and htxapp packages perform
some normalization of field sets within studies, and provide access to all
information in the sample.attributes fields returned by SRAdbV2 for studies selected
using htxapp."),
       helpText("The sample-level metadata are filtered to match samples actually
available in the HumanTranscriptomeCompendium compendium, and are then added to the metadata component
of the HumanTranscriptomeCompendium-generated SummarizedExperiment, one data.frame per study selected."),
       verbatimTextOutput("sessinf")
   )
  )
  )
)
)


server = function(input, output) {

  output$alltitles = renderDataTable({
   tags = names(titles68)
   data.frame(accession=tags, title=as.character(titles68), stringsAsFactors=FALSE)
   })

  output$b = renderTable(data.frame(st=input$cart, title=titles68[input$cart], stringsAsFactors=FALSE))
  output$c = renderPrint(unique(get(input$a, env=cancer68docs@kwstenv)))
  output$curtitle = renderDataTable( { 
              targs = mget(input$a, env=cancer68docs@kwstenv)[[1]]
              touse = titles68[unlist(targs)]
              z = data.frame(acc=targs, titles=touse)
print(z)
z
              } )
  output$e = renderDataTable( {
              targs = mget(input$a, env=cancer68docs@kwstenv)
              if (length(targs[[1]])>1) {
                 message("keyword hits studies ", paste(unlist(targs), collapse=", "), "; using first")
                 showNotification(paste0("keyword hits studies ", paste(unlist(targs), collapse=", "), "; using first"), type="message")
                 }
              targ = targs[[1]][1]
              for (i in 1:length(targs[[1]]))
                  appendTab(inputId="tabs", tab=
                    tabPanel(targs[[1]][i], helpText("a")))
              z = read.csv(system.file(paste0("cancer_corpus/", targ,".csv"), package="ssrch"), stringsAsFactors=FALSE)
              z })
#     observe({
#              if(input$btnSend > 0)
#                isolate({
#                  ans = hh[,which(hh$study_accession %in% input$cart)]
#                  md = lapply(input$cart, function(x)
#                  read.csv(system.file(paste0("can10kcsv/", x,".csv"), package="htxapp"), stringsAsFactors=FALSE))
#                  names(md) = paste0("sampleAtts", input$cart)
#                  metaids = unlist(lapply(md, function(x) x$experiment.accession))
#                  okids = intersect(colnames(ans), metaids)
#                  md = lapply(md, function(x) x[which(x$experiment.accession %in% okids),])
#                  ans = ans[,okids]
#                  S4Vectors::metadata(ans) = c(S4Vectors::metadata(ans), md)
#                  stopApp(returnValue=ans)
#                  })  
#              })  
#    output$downloadData <- downloadHandler(
#              filename = function() {
#                paste('filteredSE-', Sys.Date(), '.rds', sep='')
#                },  
#              content = function(con) {
#                ans = hh[,which(hh$study_accession %in% input$cart)]
#                md = lapply(input$cart, function(x)
#                read.csv(system.file(paste0("can10kcsv/", x,".csv"), package="htxapp"), stringsAsFactors=FALSE))
#                names(md) = paste0("sampleAtts", input$cart)
#                metaids = unlist(lapply(md, function(x) x$experiment.accession))
#                okids = intersect(colnames(ans), metaids)
#                md = lapply(md, function(x) x[which(x$experiment.accession %in% okids),])
#                ans = ans[,okids]
#                S4Vectors::metadata(ans) = c(S4Vectors::metadata(ans), md)
#                saveRDS(ans, file=con)
#                }, contentType="application/octet-stream"
#               )
    output$sessinf = renderPrint(sessionInfo())
}
runApp(list(ui=ui, server=server))
}
