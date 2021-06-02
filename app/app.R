library(shiny)
library(shinydashboard)
library(plotly)
library(dittoSeq)
library(SummarizedExperiment)
library(pins)
library(BiocManager)
library(rsconnect)
library(shinyjqui)
library(dashboardthemes)
library(ComplexHeatmap)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(rintrojs)
library(DT)
library(reactlog)
library(shinycssloaders)
library(colourpicker)
library(RColorBrewer)
library(ggrepel)
options(repos = BiocManager::repositories())

reactlog_enable()

board_register_rsconnect(server = "https://svlpbakerlab01.stjude.org/")
datasets <- pin_find("RNAseq_data", board = "rsconnect")

header <- dashboardHeader(title = "Gene Expression Explorer", titleWidth = 300)

sidebar <- dashboardSidebar(width = 300,
  pickerInput("dataset", "Select dataset:", choices = datasets$name),
                            
  # Two tabs, one for plots, the other with code used for data pre-processing.
  sidebarMenu(id = "leftsidebar",
    menuItem("Plots", tabName = "plotting", icon = icon("chart-bar"), selected = TRUE),
    menuItem("Metadata & Sample Filtering", tabName = "metadata", icon = icon("table")),
    menuItem("Dataset Pre-Processing Code", tabName = "pin_code", icon = icon("file-code")),
    menuItem("External Dataset Descriptions", tabName = "ext_data", icon = icon("file-alt")),
    menuItem("Internal Dataset Descriptions", tabName = "int_data", icon = icon("file-alt")),
    menuItem("Source Code", icon = icon("github"), href = "https://github.com/j-andrews7/GEE"),
    menuItem("About the App", tabName = "app_desc", icon = icon("book"))
  )
)

body <- dashboardBody(
  useShinyjs(),
  shinyDashboardThemes(
    theme = "grey_light"
  ),
  tabItems(
    tabItem(tabName = "plotting",
      fluidRow(
        box(
          title = "Dataset Description", width = 12, solidHeader = TRUE,
          collapsible = TRUE, collapsed = TRUE, 
          uiOutput("description"),
          valueBoxOutput("sample.count"),
          valueBoxOutput("gene.count"),
          valueBoxOutput("metadata.count")
        )
      ),
      
      fluidRow(
        jqui_resizable(box(
          title = "dittoPlot", width = 4, solidHeader = TRUE, collapsible = TRUE,
          conditionalPanel(condition = "input['dplot.do.hover'] == 1",
            withSpinner(plotlyOutput("dittoplot.int"))
          ),
          conditionalPanel(condition = "input['dplot.do.hover'] == 0",
            withSpinner(plotOutput("dittoplot.static"))
          )
        )
        ),
        
        jqui_resizable(box(
          title = "dittoDimPlot", width = 4, solidHeader = TRUE, collapsible = TRUE,
          conditionalPanel(condition = "input['ddimplot.do.hover'] == 1",
            withSpinner(plotlyOutput("dittodimplot.int"))
          ),
          conditionalPanel(condition = "input['ddimplot.do.hover'] == 0",
            withSpinner(plotOutput("dittodimplot.static"))
          )
        )
        ),
        
        jqui_resizable(box(
          title = "Sample Distance", width = 4, solidHeader = TRUE, collapsible = TRUE,
          withSpinner(plotOutput("distheatmap"))
        )
        )
      ),
      
      fluidRow(
        box(id = "dplot", title = tagList(
          fluidRow(
            column(10,
              icon("cog"), "dittoPlot Settings"),
            column(2, align = "right",
              actionButton("dplot.reset", "Reset Plot"))
            )),
          width = 4, solidHeader = TRUE, 
          collapsible = TRUE, collapsed = TRUE,
            
          uiOutput("dplot.vars"),
          tabBox(width = 12, 
            tabPanel("Basic",
              uiOutput("dplot.basic.settings")
            ),
            tabPanel("Axes & Titles",
              uiOutput("dplot.axes.settings")
            ),
            tabPanel("Aesthetics",
              hidden(div(id = "multi_dplot_aes",
                        uiOutput("multi_dplot.aes.settings")
              )),
              uiOutput("dplot.aes.settings")
            )
          )
        ),
        box(id = "ddimplot",
          title = tagList(
            fluidRow(
              column(10,
                     icon("cog"), "dittoDimPlot Settings"),
              column(2, align = "right",
                     actionButton("ddimplot.reset", "Reset Plot"))
            )), 
          width = 4, solidHeader = TRUE, 
          collapsible = TRUE, collapsed = TRUE,
          uiOutput("ddimplot.vars"),
          tabBox(width = 12, 
                 tabPanel("Basic",
                          uiOutput("ddimplot.basic.settings")
                 ),
                 tabPanel("Axes & Titles",
                          uiOutput("ddimplot.axes.settings")
                 ),
                 tabPanel("Legend",
                          uiOutput("ddimplot.legend.settings")
                 ),
                 tabPanel("Aesthetics",
                          hidden(div(id = "multi_ddimplot_aes",
                                     uiOutput("multi_ddimplot.aes.settings")
                          )),
                          uiOutput("ddimplot.aes.settings")
                 )
          )
        ),
        box(id = "distheat",
          title = tagList(
            fluidRow(
              column(10,
                     icon("cog"), "Sample Distance Settings"),
              column(2, align = "right",
                     actionButton("distheat.reset", "Reset Plot"))
            )), 
          width = 4, solidHeader = TRUE, 
          collapsible = TRUE, collapsed = TRUE,
          uiOutput("distheatmap.settings")
        )
      )
    ),
    
    tabItem(tabName = "metadata",
      fluidRow(
        box(width = 12,
          DTOutput("metadata")
        )
      )
    ),
    
    tabItem(tabName = "pin_code",
      fluidRow(
        box(width = 12,
            uiOutput("pin.code")
        )
      )
    ),
    
    tabItem(tabName = "ext_data",
            fluidRow(
              box(width = 12,
                  DTOutput("ext.data")
              )
            )
    ),
    
    tabItem(tabName = "int_data",
            fluidRow(
              box(width = 12,
                  DTOutput("int.data")
              )
            )
    ),
    
    tabItem(tabName = "app_desc",
      fluidRow(
        box(width = 12,
            h1("About the Gene Expression Explorer"),
            HTML("This Shiny app was originally created/inspired by Laura Hover. It's built upon the unparalleled ",
                 "flexibility of the R/Bioconductor package ",
                 "<a href=http://bioconductor.org/packages/release/bioc/html/dittoSeq.html>dittoSeq</a> to ",
                 "provide a convenient avenue for bulk and single-cell RNA-seq figure generation and exploratory data analysis. <br><br>",
                 "If you use it in your research, please cite the publication:<br>",
                 "Bunis, D. G., Andrews, J., Fragiadakis, G. K., Burt, T. D. & Sirota, M. ",
                 "dittoSeq: universal user-friendly single-cell and bulk RNA sequencing visualization toolkit. ",
                 "<i>Bioinformatics</i> (2020) doi:10.1093/bioinformatics/btaa1011.")
        )
      )
    )
  )
)

ui <- dashboardPage(header = header, sidebar = sidebar, body = body)

server <- function(input, output, session) {
  
  source("utils.R", local = TRUE)
  
  dplot.args <- .init_dplot_args()
  ddimplot.args <- .init_ddimplot_args()
  
  # Render certain UI elements so inputs in hidden elements are still initialized, preventing errors on startup.
  output$dplot.vars <- renderUI({})
  outputOptions(output, "dplot.vars", suspendWhenHidden = FALSE)
  
  output$dplot.basic.settings <- renderUI({})
  outputOptions(output, "dplot.basic.settings", suspendWhenHidden = FALSE)
  
  output$dplot.axes.settings <- renderUI({})
  outputOptions(output, "dplot.axes.settings", suspendWhenHidden = FALSE)
  
  output$dplot.aes.settings <- renderUI({})
  outputOptions(output, "dplot.aes.settings", suspendWhenHidden = FALSE)
  
  output$multi_dplot.aes.settings <- renderUI({})
  outputOptions(output, "multi_dplot.aes.settings", suspendWhenHidden = FALSE)
  
  output$ddimplot.vars <- renderUI({})
  outputOptions(output, "ddimplot.vars", suspendWhenHidden = FALSE)
  
  output$ddimplot.basic.settings <- renderUI({})
  outputOptions(output, "ddimplot.basic.settings", suspendWhenHidden = FALSE)
  
  output$ddimplot.legend.settings <- renderUI({})
  outputOptions(output, "ddimplot.legend.settings", suspendWhenHidden = FALSE)
  
  output$ddimplot.axes.settings <- renderUI({})
  outputOptions(output, "ddimplot.axes.settings", suspendWhenHidden = FALSE)
  
  output$ddimplot.aes.settings <- renderUI({})
  outputOptions(output, "ddimplot.aes.settings", suspendWhenHidden = FALSE)
  
  output$multi_ddimplot.aes.settings <- renderUI({})
  outputOptions(output, "multi_ddimplot.aes.settings", suspendWhenHidden = FALSE)
  
  output$distheatmap.settings <- renderUI({})
  outputOptions(output, "distheatmap.settings", suspendWhenHidden = FALSE)
  
  output$metadata <- renderUI({})
  outputOptions(output, "metadata", suspendWhenHidden = FALSE)
  
  output$ext.data <- renderUI({})
  outputOptions(output, "ext.data", suspendWhenHidden = FALSE)
  
  output$int.data <- renderUI({})
  outputOptions(output, "int.data", suspendWhenHidden = FALSE)
  
  # Load chosen Pin.
  dataset <- reactive({
    pin_get(input$dataset, board = "rsconnect")
  })
  
  ext.data.desc <- reactive({pin_get("external_data_descriptions", board = "rsconnect")})
  int.data.desc <- reactive({pin_get("internal_data_descriptions", board = "rsconnect")})
  
  observeEvent(dataset(),{
    for(f in names(dataset()[["dplot.defaults"]])) {
      dplot.args[[f]] <- dataset()[["dplot.defaults"]][[f]]
    }
    
    for(f in names(dataset()[["ddimplot.defaults"]])) {
      ddimplot.args[[f]] <- dataset()[["ddimplot.defaults"]][[f]]
    }
  })
  
  # This is a long string of markdown text.
  output$pin.code <- renderUI({
    req(dataset)
    sce <- dataset()[["sce"]]
    shiny::markdown(metadata(sce)$pin.code)
  })
  
  output$metadata <- renderDT({
    req(dataset)
    sce <- dataset()[["sce"]]
    df <- as.data.frame(colData(sce))
    DT::datatable(df,
                  filter = "top",
                  extensions = c("Buttons", "Scroller"),
                  options = list(
                    search = list(regex = TRUE),
                    lengthMenu = list(c(10, 25, 50, -1), c("10", "25", "50", "all")),
                    dom = 'Blfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                    scrollX = TRUE,
                    deferRender = TRUE,
                    scrollY = 700,
                    scroller = TRUE)
    )
  })
  
  output$ext.data <- renderDT({
    req(ext.data.desc)
    df <- ext.data.desc()
    DT::datatable(df,
                  filter = "top",
                  extensions = c("Buttons"),
                  options = list(
                    search = list(regex = TRUE),
                    lengthMenu = list(c(10, 25, 50, -1), c("10", "25", "50", "all")),
                    dom = 'Blfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                    deferRender = TRUE,
                    autoWidth = TRUE,
                    columnDefs = list(list(width = "350px", targets = c(2,3))))
    ) %>% DT::formatStyle(columns = c(1, 2, 3, 4, 5, 6, 7), fontSize = '75%')
  })
  
  output$int.data <- renderDT({
    req(int.data.desc)
    df <- int.data.desc()
    DT::datatable(df,
                  filter = "top",
                  extensions = c("Buttons"),
                  options = list(
                    search = list(regex = TRUE),
                    lengthMenu = list(c(10, 25, 50, -1), c("10", "25", "50", "all")),
                    dom = 'Blfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                    deferRender = TRUE,
                    autoWidth = TRUE,
                    columnDefs = list(list(width = "4000px", targets = c(2,3))))
    ) %>% DT::formatStyle(columns = c(1, 2, 3, 4, 5), fontSize = '80%')
  })
  
  # Update metadata table with new values on dataset switch so that any previous filter is not carried over
  # in terms of samples removed from plots.
  proxy <- dataTableProxy("metadata")
  observe({
    replaceData(proxy, colData(dataset()[["sce"]]))
  })
  
  # Source code.
  output$source.code <- renderUI({
    req(dataset)
    shiny::includeMarkdown(knitr::spin("app.R", knit = FALSE, precious = FALSE))
    shiny::includeMarkdown(knitr::spin("utils.R", knit = FALSE, precious = FALSE))
  })
  
  # Dataset description and dims.
  output$description <- renderUI({
    req(dataset)
    sce <- dataset()[["sce"]]
    shiny::markdown(metadata(sce)$description)
  })
  
  output$sample.count <- renderValueBox({
    req(dataset)
    valueBox(
      dim(dataset()[["sce"]])[2], "Samples", 
      icon = icon("vials"), color = "green"
    )
  })
  
  output$gene.count <- renderValueBox({
    req(dataset)
    valueBox(
      dim(dataset()[["sce"]])[1], "Genes", 
      icon = icon("dna"), color = "aqua"
    )
  })
  
  output$metadata.count <- renderValueBox({
    req(dataset)
    valueBox(
      dim(colData(dataset()[["sce"]]))[2], "Metadata Variables", 
      icon = icon("database"), color = "purple"
    )
  })
  
  ### DITTOPLOT
  # basic parameters.
  output$dplot.vars <- renderUI({
    req(dataset)
    genes <- getGenes(dataset()[["sce"]])
    metas <- getMetas(dataset()[["sce"]])
    
    if (is.null(dataset()[["dplot.defaults"]][["genes"]]) & is.null(dataset()[["dplot.defaults"]][["metas"]])) {
      sel.gene <- genes[1]
      sel.meta <- NULL
    } else if (is.null(dataset()[["dplot.defaults"]][["metas"]])) {
      sel.gene <- dataset()[["dplot.defaults"]][["genes"]]
      sel.meta <- NULL
    } else if (is.null(dataset()[["dplot.defaults"]][["genes"]])){
      sel.gene <- NULL
      sel.meta <- dataset()[["dplot.defaults"]][["metas"]]
    } else {
      sel.gene <- dataset()[["dplot.defaults"]][["genes"]]
      sel.meta <- dataset()[["dplot.defaults"]][["metas"]]
    }
    
    tagList(
      pickerInput("dplot.genes", "Gene(s):", genes,
                  selected = sel.gene,
                  multiple = TRUE,
                  options = list(
                    `live-search` = TRUE,
                    `actions-box` = TRUE)),
      
      pickerInput("dplot.metas", "Metadata Variable(s):", metas,
                  selected = sel.meta,
                  multiple = TRUE,
                  options = list(
                    `live-search` = TRUE,
                    `actions-box` = TRUE))
    )
  })
  
  output$dplot.basic.settings <- renderUI({
    req(dataset, multi.dplot, dplot.args)
    sce <- dataset()[["sce"]]
    metas <- getMetas(sce)
    genes <- getGenes(sce)
    
    tagList(
      fluidRow(
        column(6,
          pickerInput("dplot.group.by", 
          span(popify(icon("question-circle"), "Group By",
                      "The metadata variable used for sample grouping.",
                                  placement = "top", trigger = "click"),"Group by:"), 
                      c("", names(colData(sce))),
                      selected = dplot.args$group.by)
        ),
        column(6,
          pickerInput("dplot.color.by", 
                      span(popify(icon("question-circle"), "Color By", 
                                  c("The metadata variable used for sample coloring.",
                                    "<br><br>This looks a bit of a mess when used in interactive mode."),
                                  placement = "top", trigger = "click"),"Color by:"), 
                      c("", names(colData(sce))),
                      selected = dplot.args$color.by)
        )
      ),
      fluidRow(
        column(6,
          pickerInput("dplot.split.by", 
                      span(popify(icon("question-circle"), "Split By", 
                                  "1 or 2 discrete metadata variables to use for splitting the samples into multiple plots.",
                                  placement = "top", trigger = "click"),"Split by:"), 
                      names(colData(sce)),
                      selected = dplot.args$split.by,
                      multiple = TRUE,
                      options =  list(
                        "max-options" = 2,
                        "max-options-text" = "No 3D Faceting Here!"
                      ))
        ),
        column(6,
          pickerInput("dplot.shape.by", 
                      span(popify(icon("question-circle"), "Shape By", 
                                  "The metadata variable to use for setting the shapes of the jitter points.",
                                  placement = "top", trigger = "click"),"Shape by:"), 
                      c("", names(colData(sce))),
                      selected = dplot.args$shape.by)
        )
      ),
      
      fluidRow(
        column(6,
          pickerInput("dplot.plots", 
                      span(popify(icon("question-circle"), "Plots", 
                                  c("The layers of plots to be displayed. Order matters - plots will be placed over each other",
                                    " in the order that they are selected.",
                                    "<br><br>Boxplots are always placed at the back in interactive mode, but will be properly",
                                    " ordered in the downloaded report.",
                                    "<br><br>Ridgeplots will take priority over box and violin plots if selected."),
                                  placement = "top", trigger = "click"),
                           "Plots:"),
                      c("vlnplot", "boxplot", "jitter", "ridgeplot"),
                      selected = dplot.args$plots, 
                      multiple = TRUE)
        ),
        column(6,
          pickerInput("dplot.assay", 
                      span(popify(icon("question-circle"), "Assay", 
                                  c("The counts assay to display. Generally, these will look very similar ",
                                    "(except perhaps the raw counts & TPMs).<br><br>",
                                    "If you are not sure which to use, <b>lognorm</b> is a safe bet, though ",
                                    "<b>vst</b> or <b>rlog</b> may look better in heatmaps. <b>tpm</b> should be used to compare ",
                                    "expression of different genes <i>within</i> a sample.",
                                    "<br><br>",
                                    "Potential Choices (may be limited by dataset):",
                                    "<br><br>",
                                    "<b>raw: </b>", 
                                    "Raw counts, typically output from salmon.<br><br>",
                                    "<b>norm: </b>", 
                                    "Normalized counts via DESeq2.<br><br>",
                                    "<b>cpm: </b>",
                                    "Normalized library size transformed counts-per-million via edgeR.<br><br>",
                                    "<b>lognorm: </b>",
                                    "log2 (normalized counts + 1) via DESeq2.<br><br>",
                                    "<b>tpm: </b>",
                                    "Transcripts-per-million gene-level abundances as collapsed by tximport after salmon transcript quantification.<br><br>",
                                    "<b>vst: </b>",
                                    "Variance stabilization-transformed counts via DESeq2. ",
                                    "Deals with increased variability of low counts on the log scale.<br><br>",
                                    "<b>rlog: </b>",
                                    "Regularized log-transformed counts via DESeq2. ",
                                    "Deals with increased variability of low counts on the log scale.<br><br>",
                                    "See <a href=http://bioconductor.org/packages/release/bioc/vignettes/DESeq2/inst/doc/DESeq2.html#effects-of-transformations-on-the-variance>",
                                    "the DESeq2 vignette</a> for details and examples of why count transformations can be helpful for visualization."
                                  ), placement = "top", trigger = "click"),"Assay:"), 
                      names(assays(sce)),
                      selected = dplot.args$assay)
        )
      ),
      
      fluidRow(
        column(6,
          pickerInput("dplot.hover.data", 
                      span(popify(icon("question-circle"), "Additional Hover Data", 
                                  "Metadata variables to display when hovering over a point in interactive mode.",
                                  placement = "top", trigger = "click"),
                          "Additional Hover Data:"), 
                      metas,
                      selected = dplot.args$hover.data,
                      multiple = TRUE)
        ),
        column(6,
          switchInput("dplot.do.hover", strong("Interactive Mode"), value = dplot.args$do.hover, 
                        onStatus = "success", size = "mini", labelWidth = "250px"),
          switchInput("dplot.use.filts", strong("Use Metadata Filters"), value = TRUE, 
                      onStatus = "success", size = "mini", labelWidth = "250px")
        )
      ),
      
      pickerInput("dplot.adjustment", 
        span(popify(icon("question-circle"), "Adjustment", 
                    c("Adjustments to gene expression/feature values prior to plotting. ",
                      "<br><br>",
                      "Options:",
                      "<br><br>",
                      "<b>z-score: </b>", 
                      "Scaled to produce a relative-to-mean z-score representation.<br><br>",
                      "<b>relative.to.max: </b>",
                      "Divided by the maximum expression value to give a percent of max value (between [0,1]).<br><br>",
                      "<b>Any adjustment is performed prior to cell/sample filtering.</b>"
                    ), placement = "top", trigger = "click"),"Adjustment:"), 
        c("", "z-score", "relative.to.max"),
        selected = dplot.args$adjustment),
      
      fluidRow(
        column(6,
          textInput("dplot.legend.title", "Legend Title:", value = dplot.args$legend.title)
        ),
        column(6,
          switchInput("dplot.legend.show", strong("Show Legend"), value = dplot.args$legend.show,
                         onStatus = "success", labelWidth = "250px", size = "mini")
        )
      ),
    )
  })
  
  # axis & title parameters.
  output$dplot.axes.settings <- renderUI({
    req(dataset, input$dplot.group.by, multi.dplot, dplot.args)
    sce <- dataset()[["sce"]]
    metas <- getMetas(sce)
    
    # Get filtered metadata if necessary for cells.use.
    if (!is.null(input$metadata_rows_all) & input$dplot.use.filts) {
      filt.sce <- colData(sce)[input$metadata_rows_all,]
    } else {
      filt.sce <- colData(sce)
    }
    
    tagList(
      fluidRow(
        column(6,
          textInput("dplot.main", 
                    span(popify(icon("question-circle"), "Plot Title", 
                                "`make` will generate the title from the variables used.",
                                placement = "top", trigger = "click"),"Plot Title:"), 
                    value = dplot.args$main)
        ),
        column(6,
          textInput("dplot.sub", "Plot Subtitle:", value = dplot.args$sub)
        )
      ),
      
      fluidRow(
        column(6,
          textInput("dplot.ylab", 
                    span(popify(icon("question-circle"), "Y-Axis Title", 
                                "`make` will generate the title from the variables used.",
                                placement = "top", trigger = "click"),"Y-Axis Title:"), 
                    value = dplot.args$ylab)
        ),
        column(6,
          textInput("dplot.xlab", "X-axis Title:", value = dplot.args$group.by)
        )
      ),
      
      fluidRow(
        column(6,
          numericInput("dplot.max", "Y-axis Max:", value = dplot.args$max, step = 0.1)
        ),
        column(6,
          numericInput("dplot.min", "Y-axis Min:", value = dplot.args$min, step = 0.1)
        )
      ),
      
      fluidRow(
        column(6,
          switchInput("dplot.x.labels.rotate", strong("Rotate X-axis Labels"),
                         value = dplot.args$x.labels.rotate, onStatus = "success", 
                      labelWidth = "250px", size = "mini")
        ),
        column(6,
          orderInput("dplot.x.reorder", "X-axis Group Order:", items = seq_along(unique(filt.sce[[input$dplot.group.by]])))
        )
      ),
      
      fluidRow(
        column(6,
          numericInput("dplot.split.nrow", "Plot Rows (When Split By Used):", value = dplot.args$split.nrow, 
                       min = 1, step = 1)
        ),
        column(6,
          numericInput("dplot.split.ncol", "Plot Columns (When Split By Used):", value = dplot.args$split.ncol, 
                       min = 1, step = 1)
        )
      ),
      
      fluidRow(
        column(6,
               switchInput("dplot.coord.flip", strong("Flip Axes"),
                           value = FALSE, onStatus = "success", labelWidth = "220px", size = "mini")
        ),
        column(6,
               switchInput("dplot.keep.square", strong("Keep Plot Square"),
                           value = FALSE, onStatus = "success", labelWidth = "220px", size = "mini")
        )
      )
    )
  })
  
  # aes parameters.
  output$dplot.aes.settings <- renderUI({
    req(dataset, multi.dplot)

    sce <- dataset()[["sce"]]
    metas <- getMetas(sce)
    
    tagList(
      fluidRow(
        column(6,
          numericInput("dplot.jitter.size", 
                       span(popify(icon("question-circle"), "Jitter Size", 
                                   "Scalar which sets the size of the jitter shapes.",
                                   placement = "top", trigger = "click"),"Jitter Point Size:"), 
                       value = dplot.args$jitter.size,
                       min = 0.01, 
                       step = 0.05)
        ),
        column(6,
          numericInput("dplot.jitter.width", 
                       span(popify(icon("question-circle"), "Plot Width", 
                                   "Scalar which sets the width/spread of the jitter plot in the x direction.",
                                   placement = "top", trigger = "click"), "Jitter Plot Width:"), 
                       value = dplot.args$jitter.width,
                       min = 0.01, 
                       step = 0.05)
        )
      ),
      
      fluidRow(
        column(6,
          colourInput("dplot.jitter.color", 
            "Jitter Point Outline Color:", 
            value = dplot.args$jitter.color
          )
        ),
        column(6,
          switchInput("dplot.jitter.shape.legend.show", 
            strong("Show Jitter Shape Legend"), 
            value = dplot.args$jitter.shape.legend.show, onStatus = "success", 
            labelWidth = "250px", size = "mini"
          )
        )
      ),
      
      fluidRow(
        column(6,
          numericInput("dplot.boxplot.position.dodge", 
                       span(popify(icon("question-circle"), "Plot Position", 
                                   c("Scalar which adjusts the distance between boxplots when multiple are ",
                                     "drawn per grouping (a.k.a. when group.by and color.by are not equal)."),
                                   placement = "top", trigger = "click"),"Boxplot Position Dodge:"), 
                       value = dplot.args$boxplot.position.dodge,
                       min = 0.01, 
                       step = 0.05)
        ),
        column(6,
          numericInput("dplot.boxplot.width", 
                       span(popify(icon("question-circle"), "Plot Width", 
                                   "Scalar which sets the width/spread of the boxplot in the x direction.",
                                   placement = "top", trigger = "click"), "Boxplot Width:"), 
                       value = dplot.args$boxplot.width,
                       min = 0.01, 
                       step = 0.05)
        )
      ),
      
      fluidRow(
        column(6,
               colourInput("dplot.boxplot.color", 
            "Boxplot Outline Color:", 
            value = dplot.args$boxplot.color
          )
        ),
        column(6,
          switchInput("dplot.boxplot.fill", 
            strong("Color Fill Boxplots"), 
            value = dplot.args$boxplot.fill, onStatus = "success", labelWidth = "250px", size = "mini"
          )
        )
      ),
      
      fluidRow(
        column(6,
          numericInput("dplot.vlnplot.lineweight", 
                       span(popify(icon("question-circle"), "Line Weight", 
                                   "Scalar which sets the thickness of the line that outlines the violin plots.",
                            placement = "top", trigger = "click"),"Violin Plot Line Weight:"), 
                       value = dplot.args$vlnplot.lineweight,
                       min = 0.01, 
                       step = 0.05)
        ),
        column(6,
          numericInput("dplot.vlnplot.width", 
                       span(popify(icon("question-circle"), "Plot Width", 
                                   "Scalar which sets the width/spread of the violin plot in the x direction.",
                            placement = "top", trigger = "click"), "Violin Plot Width:"), 
                       value = dplot.args$vlnplot.width,
                       min = 0.01, 
                       step = 0.05)
        )
      ),
      
      fluidRow(
        column(6,
          pickerInput("dplot.vlnplot.scaling", 
                       span(popify(icon("question-circle"), "Scaling", 
                                   c("String which sets how the widths of the of violin plots are set in relation ", 
                                   "to each other. Options are 'area', 'count', and 'width'. If the deafult is not ",
                                   "right for your data, I recommend trying 'width'."),
                                   placement = "top", trigger = "click"),"Violin Plot Width Scaling:"),
                      choices = c("area", "count", "width"),
                      selected = dplot.args$vlnplot.scaling)
        ),
        column(6,
          numericInput("dplot.ridgeplot.lineweight", 
                       span(popify(icon("question-circle"), "Line Weight", 
                                   "Scalar which sets the thickness of the ridgeplot outline.",
                                   placement = "top", trigger = "click"), "Ridgeplot Line Weight:"), 
                       value = dplot.args$ridgeplot.lineweight,
                       min = 0.01, 
                       step = 0.05)
        )
      ),
      
      fluidRow(
        column(6,
          numericInput("dplot.ridgeplot.scale", 
                       span(popify(icon("question-circle"), "Scale", 
                                   c("Scalar which sets the distance/overlap between ridgeplots. A value of 1 ",
                                   "means the tallest density curve just touches the baseline of the next higher one. ",
                                   "Higher numbers lead to greater overlap."),
                                   placement = "top", trigger = "click"), "Ridgeplot Scale:"), 
                       value = dplot.args$ridgeplot.scale,
                       min = 0.01, 
                       step = 0.05)
        ),
        column(6,
          numericInput("dplot.ridgeplot.ymax.expansion", 
                       span(popify(icon("question-circle"), "Y-max Expansion", 
                                   c("Scalar which adjusts the minimal space between the topmost grouping ",
                                     "and the top of the plot in order to ensure the curve is not cut off by ",
                                     "the plotting grid. The larger the value, the greater the space requested. ",
                                     "When left as NA, dittoSeq will attempt to determine an ideal value itself ",
                                     "based on the number of groups & linear interpolation between these goal posts: ",
                                     "#groups of 3 or fewer: 0.6; #groups=12: 0.1; #groups or 34 or greater: 0.05."),
                                   placement = "top", trigger = "click"), "Ridgeplot Y-max Expansion:"), 
                       value = dplot.args$ridgeplot.ymax.expansion,
                       step = 0.05)
        )
      ),
      
      fluidRow(
        column(6,
          numericInput("dplot.add.line", 
                       "Add Line At:", 
                       value = dplot.args$add.line,
                       step = 0.01)
        ),
        column(6,
          pickerInput("dplot.line.linetype", 
                      "Line Type:",
                      choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                      selected = dplot.args$line.linetype)
          )
      ),
      
      colourInput("dplot.line.color", 
                "Added Line Color:", 
                value = dplot.args$line.color
      )
    )
  })
  
  output$multi_dplot.aes.settings <- renderUI({
    req(dataset, dplot.args)
    
    tagList(
      fluidRow(
        column(6,
               numericInput("dplot.nrow", "Plot Rows:", value = dplot.args$nrow, step = 1)
        ),
        column(6,
               numericInput("dplot.ncol", "Plot Columns:", 
                            value = length(c(input$dplot.genes, input$dplot.metas)), step = 1)
        )
      )
      
    )
  })
  
  # Plots.
  output$dittoplot.int <- renderPlotly({
    # Ensures data is loaded and plot inputs ready prior to trying to render plot.
    req(input$dplot.x.reorder_order, dataset)
    sce <- dataset()[["sce"]]
    
    plot.args <- .collect_dittoplot_args()
    
    if(is.null(plot.args$var) && is.null(plot.args$vars)) {
      return(NULL)
    }
    
    if(multi.dplot()) {
      p <- do.call(multi_dittoPlot, c(object = sce, plot.args))
    } else {
      p <- do.call(dittoPlot, c(object = sce, plot.args))
    }
    
    if (input$dplot.coord.flip) {
      p + coord_flip() 
    } else {
      p
    }
    
  })
  
  output$dittoplot.static <- renderPlot({
    # Ensures data is loaded and plot inputs ready prior to trying to render plot.
    req(input$dplot.x.reorder_order, dataset)
    sce <- dataset()[["sce"]]
    plot.args <- .collect_dittoplot_args()
    
    if(is.null(plot.args$var) && is.null(plot.args$vars)) {
      return(NULL)
    }
    
    if(multi.dplot()) {
      p <- do.call(multi_dittoPlot, c(object = sce, plot.args))
    } else {
      p <- do.call(dittoPlot, c(object = sce, plot.args))
    }
    
    if (input$dplot.coord.flip) {
      p + coord_flip() 
    } else {
      p
    }
    
  })
  
  ### DITTODIMPLOT
  # basic parameters.
  output$ddimplot.vars <- renderUI({
    req(dataset)
    genes <- getGenes(dataset()[["sce"]])
    metas <- getMetas(dataset()[["sce"]])
    
    if (is.null(dataset()[["ddimplot.defaults"]][["genes"]]) & is.null(dataset()[["ddimplot.defaults"]][["metas"]])) {
      sel.gene <- genes[1]
      sel.meta <- NULL
    } else if (is.null(dataset()[["ddimplot.defaults"]][["metas"]])) {
      sel.gene <- dataset()[["ddimplot.defaults"]][["genes"]]
      sel.meta <- NULL
    } else if (is.null(dataset()[["ddimplot.defaults"]][["genes"]])){
      sel.gene <- NULL
      sel.meta <- dataset()[["ddimplot.defaults"]][["metas"]]
    } else {
      sel.gene <- dataset()[["ddimplot.defaults"]][["genes"]]
      sel.meta <- dataset()[["ddimplot.defaults"]][["metas"]]
    }
    
    tagList(
      pickerInput("ddimplot.genes", "Gene(s):", genes,
                  selected = sel.gene,
                  multiple = TRUE,
                  options = list(
                    `live-search` = TRUE,
                    `actions-box` = TRUE)),
      
      pickerInput("ddimplot.metas", "Metadata Variable(s):", metas,
                  selected = sel.meta,
                  multiple = TRUE,
                  options = list(
                    `live-search` = TRUE,
                    `actions-box` = TRUE))
    )
  })
  
  output$ddimplot.basic.settings <- renderUI({
    req(dataset, multi.ddimplot, ddimplot.args)
    sce <- dataset()[["sce"]]
    metas <- getMetas(sce)
    genes <- getGenes(sce)
    
    tagList(
      fluidRow(
        column(6,
               pickerInput("ddimplot.reduction.use", 
                           span(popify(icon("question-circle"), "Dimensionality Reduction",
                                       "The dimensionality reduction plotted.",
                                       placement = "top", trigger = "click"),"Dimensionality Reduction:"), 
                           getReductions(sce),
                           selected = ddimplot.args$reduction.use)
        ),
        column(6,
               pickerInput("ddimplot.order", 
                           span(popify(icon("question-circle"), "Plot Order", 
                                       c("Specifies whether point plotting should be ordered by increasing or decreasing values.",
                                         " Useful for ensuring cells/samples with high expression are not buried."),
                                       placement = "top", trigger = "click"),"Plot Order:"), 
                           c("unordered", "increasing", "decreasing"),
                           selected = ddimplot.args$order)
        )
      ),
      fluidRow(
        column(6,
               pickerInput("ddimplot.split.by", 
                           span(popify(icon("question-circle"), "Split By", 
                                       "1 or 2 discrete metadata variables to use for splitting the samples into multiple plots.",
                                       placement = "top", trigger = "click"),"Split by:"), 
                           names(colData(sce)),
                           selected = ddimplot.args$split.by,
                           multiple = TRUE,
                           options =  list(
                             "max-options" = 2,
                             "max-options-text" = "No 3D Faceting Here!"
                           ))
        ),
        column(6,
               pickerInput("ddimplot.shape.by", 
                           span(popify(icon("question-circle"), "Shape By", 
                                       "The metadata variable to use for setting the shapes of the jitter points.",
                                       placement = "top", trigger = "click"),"Shape by:"), 
                           c("", names(colData(sce))),
                           selected = ddimplot.args$shape.by)
        )
      ),
      
      fluidRow(
        column(6,
               pickerInput("ddimplot.assay", 
                           span(popify(icon("question-circle"), "Assay", 
                                       c("The counts assay to display. Generally, these will look very similar ",
                                         "(except perhaps the raw counts & TPMs).<br><br>",
                                         "If you are not sure which to use, <b>lognorm</b> is a safe bet, though ",
                                         "<b>vst</b> or <b>rlog</b> may look better in heatmaps. <b>tpm</b> should be used to compare ",
                                         "expression of different genes <i>within</i> a sample.",
                                         "<br><br>",
                                         "Potential Choices (may be limited by dataset):",
                                         "<br><br>",
                                         "<b>raw: </b>", 
                                         "Raw counts, typically output from salmon.<br><br>",
                                         "<b>norm: </b>", 
                                         "Normalized counts via DESeq2.<br><br>",
                                         "<b>cpm: </b>",
                                         "Normalized library size transformed counts-per-million via edgeR.<br><br>",
                                         "<b>lognorm: </b>",
                                         "log2 (normalized counts + 1) via DESeq2.<br><br>",
                                         "<b>tpm: </b>",
                                         "Transcripts-per-million gene-level abundances as collapsed by tximport after salmon transcript quantification.<br><br>",
                                         "<b>vst: </b>",
                                         "Variance stabilization-transformed counts via DESeq2. ",
                                         "Deals with increased variability of low counts on the log scale.<br><br>",
                                         "<b>rlog: </b>",
                                         "Regularized log-transformed counts via DESeq2. ",
                                         "Deals with increased variability of low counts on the log scale.<br><br>",
                                         "See <a href=http://bioconductor.org/packages/release/bioc/vignettes/DESeq2/inst/doc/DESeq2.html#effects-of-transformations-on-the-variance>",
                                         "the DESeq2 vignette</a> for details and examples of why count transformations can be helpful for visualization."
                                       ), placement = "top", trigger = "click"),"Assay:"), 
                           names(assays(sce)),
                           selected = ddimplot.args$assay)
        ),
        column(6,
               switchInput("ddimplot.show.others", strong("Show Filtered Samples/Cells in Grey"), 
                           value = ddimplot.args$show.others, 
                           onStatus = "success", size = "mini", labelWidth = "250px")
        )
      ),
      
      fluidRow(
        column(6,
               pickerInput("ddimplot.hover.data", 
                           span(popify(icon("question-circle"), "Additional Hover Data", 
                                       "Metadata variables to display when hovering over a point in interactive mode.",
                                       placement = "top", trigger = "click"),
                                "Additional Hover Data:"), 
                           metas,
                           selected = ddimplot.args$hover.data,
                           multiple = TRUE)
        ),
        column(6,
               switchInput("ddimplot.do.hover", strong("Interactive Mode"), value = ddimplot.args$do.hover, 
                           onStatus = "success", size = "mini", labelWidth = "250px"),
               switchInput("ddimplot.use.filts", strong("Use Metadata Filters"), value = TRUE, 
                           onStatus = "success", size = "mini", labelWidth = "250px")
        )
      ),
      
      fluidRow(
        column(6,
          pickerInput("ddimplot.adjustment", 
                      span(popify(icon("question-circle"), "Adjustment", 
                                  c("Adjustments to gene expression/feature values prior to plotting. ",
                                    "<br><br>",
                                    "Options:",
                                    "<br><br>",
                                    "<b>z-score: </b>", 
                                    "Scaled to produce a relative-to-mean z-score representation.<br><br>",
                                    "<b>relative.to.max: </b>",
                                    "Divided by the maximum expression value to give a percent of max value (between [0,1]).<br><br>",
                                    "<b>Any adjustment is performed prior to cell/sample filtering.</b>"
                                  ), placement = "top", trigger = "click"),"Adjustment:"), 
                      c("", "z-score", "relative.to.max"),
                      selected = ddimplot.args$adjustment)),
        column(6,
         pickerInput("ddimplot.hover.adjustment", 
                     span(popify(icon("question-circle"), "Hover Adjustment", 
                                 c("Adjustments to gene expression/feature hover values prior to plotting. ",
                                   "<br><br>",
                                   "Options:",
                                   "<br><br>",
                                   "<b>z-score: </b>", 
                                   "Scaled to produce a relative-to-mean z-score representation.<br><br>",
                                   "<b>relative.to.max: </b>",
                                   "Divided by the maximum expression value to give a percent of max value (between [0,1]).<br><br>",
                                   "<b>Any adjustment is performed prior to cell/sample filtering.</b>"
                                 ), placement = "top", trigger = "click"),"Hover Adjustment:"), 
                     c("", "z-score", "relative.to.max"),
                     selected = ddimplot.args$adjustment)
        )
      )
    )
  })
  
  # legend parameters
  output$ddimplot.legend.settings <- renderUI({
    req(dataset, ddimplot.args)
    
    tagList(
      fluidRow(
        column(6,
               textInput("ddimplot.legend.title", "Legend Title:", value = ddimplot.args$legend.title),
               numericInput("ddimplot.legend.size", "Legend Size:", value = ddimplot.args$legend.size, 
                            min = 1, step = 0.1)
        ),
        column(6,
               switchInput("ddimplot.legend.show", strong("Show Legend"), value = ddimplot.args$legend.show,
                           onStatus = "success", labelWidth = "250px", size = "mini"),
               textInput("ddimplot.shape.legend.title", "Shape Legend Title:", 
                         value = ddimplot.args$shape.legend.title),
               numericInput("ddimplot.shape.legend.size", "Shape Legend Size:", 
                            value = ddimplot.args$shape.legend.size, min = 1, step = 0.1)
        )
      ),
    )
  })
  
  # axis & title parameters.
  output$ddimplot.axes.settings <- renderUI({
    req(dataset, ddimplot.args)
    sce <- dataset()[["sce"]]
    metas <- getMetas(sce)
    
    tagList(
      fluidRow(
        column(6,
               textInput("ddimplot.main", 
                         span(popify(icon("question-circle"), "Plot Title", 
                                     "`make` will generate the title from the variables used.",
                                     placement = "top", trigger = "click"),"Plot Title:"), 
                         value = ddimplot.args$main)
        ),
        column(6,
               textInput("ddimplot.sub", "Plot Subtitle:", value = ddimplot.args$sub)
        )
      ),
      
      fluidRow(
        column(6,
               textInput("ddimplot.ylab", 
                         span(popify(icon("question-circle"), "Y-Axis Title", 
                                     "`make` will generate the title from the variables used.",
                                     placement = "top", trigger = "click"),"Y-Axis Title:"), 
                         value = ddimplot.args$ylab)
        ),
        column(6,
               textInput("ddimplot.xlab", 
                         span(popify(icon("question-circle"), "X-Axis Title", 
                                     "`make` will generate the title from the variables used.",
                                     placement = "top", trigger = "click"),"X-Axis Title:"),
                         value = ddimplot.args$xlab)
        )
      ),
      
      fluidRow(
        column(6,
               numericInput("ddimplot.dim.1", "Dimension 1:", value = ddimplot.args$dim.1, step = 1)
        ),
        column(6,
               numericInput("ddimplot.dim.2", "Dimension 2:", value = ddimplot.args$dim.2, step = 1)
        )
      ),
      
      fluidRow(
        column(6,
               numericInput("ddimplot.split.nrow", "Plot Rows (When Split By Used):", 
                            value = ddimplot.args$split.nrow, 
                            min = 1, step = 1)
        ),
        column(6,
               numericInput("ddimplot.split.ncol", "Plot Columns (When Split By Used):", 
                            value = ddimplot.args$split.ncol, 
                            min = 1, step = 1)
        )
      ),
      
      fluidRow(
        column(6,
               switchInput("ddimplot.show.axes.numbers", strong("Show Axes Numbers"),
                           value = ddimplot.args$show.axes.numbers, 
                           onStatus = "success", labelWidth = "220px", size = "mini")
        ),
        column(6,
               switchInput("ddimplot.keep.square", strong("Keep Plot Square"),
                           value = FALSE, onStatus = "success", labelWidth = "220px", size = "mini")
        )
      )
    )
  })
  
  # aes parameters.
  output$ddimplot.aes.settings <- renderUI({
    req(dataset, multi.ddimplot)
    
    sce <- dataset()[["sce"]]
    metas <- getMetas(sce)
    
    tagList(
      fluidRow(
        column(6,
               numericInput("ddimplot.size", 
                            span(popify(icon("question-circle"), "Point Size", 
                                        "Scalar which sets the point sizes.",
                                        placement = "top", trigger = "click"),"Point Size:"), 
                            value = ddimplot.args$size,
                            min = 0.01, 
                            step = 0.05)
        ),
        column(6,
               numericInput("ddimplot.opacity", 
                            span(popify(icon("question-circle"), "Opacity", 
                                        "Scalar which sets point transparency. Lower is more transparent",
                                        placement = "top", trigger = "click"), "Opacity:"), 
                            value = ddimplot.args$opacity,
                            min = 0.01, 
                            step = 0.05, max = 1)
        )
      ),
      
      fluidRow(
        column(6,
               colourInput("ddimplot.min.color", 
                           "Minimum Color:", 
                           value = ddimplot.args$min.color
               )
        ),
        column(6,
               colourInput("ddimplot.max.color", 
                           "Maximum Color:", 
                           value = ddimplot.args$max.color
               )
        )
      ),
      
      fluidRow(
        column(6,
               switchInput("ddimplot.do.letter", 
                           strong("Use Letters"), 
                           value = ddimplot.args$do.letter, onStatus = "success", 
                           labelWidth = "250px", size = "mini"
               )
        ),
        column(6,
               switchInput("ddimplot.do.ellipse", 
                           strong("Draw Ellipses"), 
                           value = ddimplot.args$do.ellipse, onStatus = "success", 
                           labelWidth = "250px", size = "mini"
               )
        )
      ),
      
      fluidRow(
        column(6,
               switchInput("ddimplot.do.label", 
                           strong("Label Groups"), 
                           value = ddimplot.args$do.label, onStatus = "success", 
                           labelWidth = "250px", size = "mini"
               ),
               numericInput("ddimplot.labels.size", 
                            span(popify(icon("question-circle"), "Label Size", 
                                        "Scalar which sets the label sizes.",
                                        placement = "top", trigger = "click"),"Label Size:"), 
                            value = ddimplot.args$labels.size,
                            min = 0.01, 
                            step = 0.05)
        ),
        column(6,
               switchInput("ddimplot.labels.highlight", 
                           strong("Highlight Labels"), 
                           value = ddimplot.args$labels.highlight, onStatus = "success", 
                           labelWidth = "250px", size = "mini"
               ),
               switchInput("ddimplot.labels.repel", 
                           strong("Repel Labels"), 
                           value = ddimplot.args$labels.repel, onStatus = "success", 
                           labelWidth = "250px", size = "mini"
               )
        )
      ),
      
      fluidRow(
        column(6,
               switchInput("ddimplot.do.contour", 
                           strong("Use Contours"), 
                           value = ddimplot.args$do.contour, onStatus = "success", 
                           labelWidth = "250px", size = "mini"
               ),
               pickerInput("ddimplot.contour.linetype", 
                           "Line Type:",
                           choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                           selected = ddimplot.args$contour.linetype)
        ),
        column(6,
               colourInput("ddimplot.contour.color", 
                           "Contour Color:", 
                           value = ddimplot.args$contour.color
               )
        )
      )
    )
  })
  
  output$multi_ddimplot.aes.settings <- renderUI({
    req(dataset, ddimplot.args)
    
    tagList(
      fluidRow(
        column(6,
               numericInput("ddimplot.nrow", "Plot Rows:", value = ddimplot.args$nrow, step = 1)
        ),
        column(6,
               numericInput("ddimplot.ncol", "Plot Columns:", 
                            value = length(c(input$ddimplot.genes, input$ddimplot.metas)), step = 1)
        )
      )
      
    )
  })
  
  # Plots.
  output$dittodimplot.int <- renderPlotly({
    # Ensures data is loaded and plot inputs ready prior to trying to render plot.
    req(ddimplot.args, dataset)
    sce <- dataset()[["sce"]]
    
    plot.args <- .collect_dittodimplot_args()
    
    if(is.null(plot.args$var) && is.null(plot.args$vars)) {
      return(NULL)
    }
    
    if(multi.ddimplot()) {
      p <- do.call(multi_dittoDimPlot, c(object = sce, plot.args))
    } else {
      p <- do.call(dittoDimPlot, c(object = sce, plot.args))
    }
    
    p
    
  })
  
  output$dittodimplot.static <- renderPlot({
    # Ensures data is loaded and plot inputs ready prior to trying to render plot.
    req(ddimplot.args, dataset)
    sce <- dataset()[["sce"]]
    plot.args <- .collect_dittodimplot_args()
    
    if(is.null(plot.args$var) && is.null(plot.args$vars)) {
      return(NULL)
    }
    
    if(multi.ddimplot()) {
      p <- do.call(multi_dittoDimPlot, c(object = sce, plot.args))
    } else {
      p <- do.call(dittoDimPlot, c(object = sce, plot.args))
    }
    
    p
    
  })
  
  ### DISTHEATMAP
  output$distheatmap.settings <- renderUI({
    req(dataset)
    sce <- dataset()[["sce"]]
    
    tagList(
      fluidRow(
        column(6,
          pickerInput("distheat.assay", 
                      span(popify(icon("question-circle"), "Assay", 
                                  c("The counts assay to display. Generally, these will look very similar ",
                                    "(except perhaps the raw counts & TPMs).<br><br>",
                                    "If you are not sure which to use, <b>lognorm</b> is a safe bet, though ",
                                    "<b>vst</b> or <b>rlog</b> may look better in heatmaps. <b>tpm</b> should be used to compare ",
                                    "expression of different genes <i>within</i> a sample.",
                                    "<br><br>",
                                    "Potential Choices (may be limited by dataset):",
                                    "<br><br>",
                                    "<b>raw: </b>", 
                                    "Raw counts, typically output from salmon.<br><br>",
                                    "<b>norm: </b>", 
                                    "Normalized counts via DESeq2.<br><br>",
                                    "<b>cpm: </b>",
                                    "Normalized library size transformed counts-per-million via edgeR.<br><br>",
                                    "<b>lognorm: </b>",
                                    "log2 (normalized counts + 1) via DESeq2.<br><br>",
                                    "<b>tpm: </b>",
                                    "Transcripts-per-million gene-level abundances as collapsed by tximport after salmon transcript quantification.<br><br>",
                                    "<b>vst: </b>",
                                    "Variance stabilization-transformed counts via DESeq2. ",
                                    "Deals with increased variability of low counts on the log scale.<br><br>",
                                    "<b>rlog: </b>",
                                    "Regularized log-transformed counts via DESeq2. ",
                                    "Deals with increased variability of low counts on the log scale.<br><br>",
                                    "See <a href=http://bioconductor.org/packages/release/bioc/vignettes/DESeq2/inst/doc/DESeq2.html#effects-of-transformations-on-the-variance>",
                                    "the DESeq2 vignette</a> for details and examples of why count transformations can be helpful for visualization."
                                  ), placement = "top", trigger = "click"),"Assay:"), 
                      choices = names(assays(sce)),
                      options = list(
                        `live-search` = TRUE,
                        `actions-box` = TRUE))),
        column(6,
          pickerInput("distheat.rowanno", 
                      span(popify(icon("question-circle"), "Row Annotations", 
                                  "The metadata variables used to annotate the rows.",
                                  placement = "top", trigger = "click"),"Row Annotations:"), 
                      c("", names(colData(sce))),
                      multiple = TRUE,
                      options = list(
                        `live-search` = TRUE,
                        `actions-box` = TRUE))
        )
      ),
      
      fluidRow(
        column(6,
           pickerInput("distheat.colanno", 
                       span(popify(icon("question-circle"), "Column Annotations", 
                                   "The metadata variables used to annotate the columns.",
                                   placement = "top", trigger = "click"),"Column Annotations:"), 
                       c("", names(colData(sce))),
                       multiple = TRUE,
                       options = list(
                         `live-search` = TRUE,
                         `actions-box` = TRUE))),
        
        column(6,
           colourInput("distheat.low", 
                       "Low Color:", 
                       value = "#E38E0D"
           ))
      ),
      fluidRow(
        column(6,
           colourInput("distheat.mid", 
                       "Midpoint Color:", 
                       value = "#93D1E6"
           )),
        
        column(6,
           colourInput("distheat.high", 
                       "High Color:", 
                       value = "#2303A3"
           ))
      ),
      fluidRow(
        column(6,
               switchInput("distheat.showrows", 
                           strong("Show Rownames"), 
                           value = TRUE, onStatus = "success", labelWidth = "250px", size = "mini"
               )
        ),
        column(6,
               switchInput("distheat.showcols", 
                           strong("Show Colnames"), 
                           value = FALSE, onStatus = "success", labelWidth = "250px", size = "mini"
               )
        )
      ),
      fluidRow(
        column(6,
               numericInput("distheat.cutree_rows", 
                            span(popify(icon("question-circle"), "Row Clusters", 
                                        c("Number of clusters rows are divided into based on hierarchical clustering.",
                                          " <b>The number must be deleted to completely remove the divisions.</b>"),
                                        placement = "top", trigger = "click"),"Row Clusters:"), 
                            value = NA,
                            step = 1, min = 2)
        ),
        column(6,
               numericInput("distheat.cutree_cols", 
                            span(popify(icon("question-circle"), "Column Clusters", 
                                        c("Number of clusters columns are divided into based on hierarchical clustering.",
                                          " <b>The number must be deleted to completely remove the divisions.</b>"),
                                        placement = "top", trigger = "click"),"Column Clusters:"), 
                            value = NA,
                            step = 1, min = 2)
        )
      ),
      fluidRow(
        column(6,
               colourInput("distheat.border_color", 
                           "Border Color:", 
                           value = "grey60"
               )),
        
        column(6,
               switchInput("distheat.legend", 
                           strong("Show Legend"), 
                           value = TRUE, onStatus = "success", labelWidth = "250px", size = "mini"
               ),
               switchInput("distheat.use.filts", strong("Use Metadata Filters"), value = TRUE, 
                           onStatus = "success", size = "mini", labelWidth = "250px")
               )
      ),
      fluidRow(
        column(6,
               numericInput("distheat.fontsize_row", 
                            "Row Fontsize:", 
                            value = 10,
                            step = 0.1, min = 1)
        ),
        column(6,
               numericInput("distheat.fontsize_col", 
                            "Column Fontsize:", 
                            value = 10,
                            step = 0.1, min = 1)
        )
      ),
      fluidRow(
        column(12,
               pickerInput("distheat.angle_col", 
                           "Column Label Angle:", 
                           c(0, 45, 90, 270, 315),
                           selected = 90)
        )
      )
    )
  })
  
  output$distheatmap <- renderPlot({
    req(dataset, input$distheat.assay)
    sce <- dataset()[["sce"]]
    
    if (!is.null(input$metadata_rows_all) && input$distheat.use.filts) {
      samp.dists <- dist(t(assay(sce, input$distheat.assay)[,input$metadata_rows_all]))
      samp.dists.mat <- as.matrix(samp.dists)
      rownames(samp.dists.mat) <- rownames(colData(sce)[input$metadata_rows_all,])
      colnames(samp.dists.mat) <- rownames(colData(sce)[input$metadata_rows_all,])
    } else {
      samp.dists <- dist(t(assay(sce, input$distheat.assay)))
      samp.dists.mat <- as.matrix(samp.dists)
      rownames(samp.dists.mat) <- rownames(colData(sce))
      colnames(samp.dists.mat) <- rownames(colData(sce))
    }
    
    colors <- c(input$distheat.low, input$distheat.mid, input$distheat.high)
    
    if(!is.null(input$distheat.rowanno)) {
      anno.row <- data.frame(colData(sce))[,input$distheat.rowanno, drop = FALSE]
      if (!is.null(input$metadata_rows_all) && input$distheat.use.filts) {
        anno.row <- anno.row[input$metadata_rows_all,, drop = FALSE]
      }
    } else {
      anno.row <- NA
    }
    
    if(!is.null(input$distheat.colanno)) {
      anno.col <- data.frame(colData(sce))[,input$distheat.colanno, drop = FALSE]
      if (!is.null(input$metadata_rows_all) && input$distheat.use.filts) {
        anno.col <- anno.col[input$metadata_rows_all,, drop = FALSE]
      }
    } else {
      anno.col <- NA
    }
    
    ht <- ComplexHeatmap::pheatmap(samp.dists.mat,
             clustering_distance_rows=samp.dists,
             clustering_distance_cols=samp.dists,
             color = colors,
             annotation_row = anno.row,
             annotation_col = anno.col,
             show_rownames = input$distheat.showrows,
             show_colnames = input$distheat.showcols,
             cutree_rows = input$distheat.cutree_rows,
             cutree_cols = input$distheat.cutree_cols,
             border_color = input$distheat.border_color,
             legend = input$distheat.legend,
             fontsize_row = input$distheat.fontsize_row,
             fontsize_col = input$distheat.fontsize_col,
             angle_col = input$distheat.angle_col)
    draw(ht)
  })
  
  # Check if multiple genes/metas selected to use multiplotters as necessary.
  multi.dplot <- reactiveVal(FALSE)
  multi.ddimplot <- reactiveVal(FALSE)

  observeEvent(input$dplot.genes, {
    
    if(length(c(input$dplot.genes, input$dplot.metas)) > 1) {
      if(!multi.dplot()) {
        dplot.args$do.hover <- input$dplot.do.hover
        dplot.args$legend.show <- input$dplot.legend.show
        dplot.args$coord.flip <- input$dplot.coord.flip
        dplot.args$x.labels.rotate <- input$dplot.x.labels.rotate
      }
      multi.dplot(TRUE)
    } else {
      multi.dplot(FALSE)
    }
    
    if(multi.dplot()) {
      shinyjs::show(id = "multi_dplot_aes", anim = TRUE)
      updateSwitchInput(session, "dplot.do.hover", value = FALSE, disabled = TRUE)
      updateSwitchInput(session, "dplot.legend.show", value = FALSE)
      updateSwitchInput(session, "dplot.coord.flip", value = FALSE, disabled = TRUE)
    } else {
      shinyjs::hide(id = "multi_dplot_aes", anim = TRUE)
      updateSwitchInput(session, "dplot.do.hover", value = dplot.args$do.hover, disabled = FALSE)
      updateSwitchInput(session, "dplot.legend.show", value = dplot.args$legend.show)
      updateSwitchInput(session, "dplot.coord.flip", value = dplot.args$coord.flip, disabled = FALSE)
    }
  })

  observeEvent(input$dplot.metas, {
    
    if(length(c(input$dplot.genes, input$dplot.metas)) > 1) {
      if(!multi.dplot()) {
        dplot.args$x.labels.rotate <- input$dplot.x.labels.rotate
        dplot.args$do.hover <- input$dplot.do.hover
        dplot.args$legend.show <- input$dplot.legend.show
        dplot.args$coord.flip <- input$dplot.coord.flip
      }
      multi.dplot(TRUE)
    } else {
      multi.dplot(FALSE)
    }
    
    if(multi.dplot()) {
      updateSwitchInput(session, "dplot.do.hover", value = FALSE, disabled = TRUE)
      updateSwitchInput(session, "dplot.legend.show", value = FALSE)
      updateSwitchInput(session, "dplot.coord.flip", value = FALSE, disabled = TRUE)
    } else {
      updateSwitchInput(session, "dplot.do.hover", value = dplot.args$do.hover, disabled = FALSE)
      updateSwitchInput(session, "dplot.legend.show", dplot.args$legend.show)
      updateSwitchInput(session, "dplot.coord.flip", value = dplot.args$coord.flip, disabled = FALSE)
    }
  })
  
  observeEvent(input$dplot.do.hover, {
    
    if(input$dplot.do.hover) {
      dplot.args$keep.square <- input$dplot.keep.square
      dplot.args$coord.flip <- input$dplot.coord.flip
      dplot.args$legend.show <- input$dplot.legend.show
      updateSwitchInput(session, "dplot.keep.square", value = FALSE, disabled = TRUE)
      updateSwitchInput(session, "dplot.coord.flip", value = FALSE, disabled = TRUE)
      updateSwitchInput(session, "dplot.legend.show", value = dplot.args$legend.show)
    } else {
      updateSwitchInput(session, "dplot.coord.flip", value = dplot.args$coord.flip, disabled = multi.dplot())
      updateSwitchInput(session, "dplot.keep.square", value = dplot.args$keep.square, disabled = FALSE)
    }
  })
  
  observeEvent(input$dplot.plots, {
    dplot.args$do.hover <- input$dplot.do.hover
    if("ridgeplot" %in% input$dplot.plots) {
      updateSwitchInput(session, "dplot.do.hover", value = FALSE, disabled = TRUE)
    } else if(!multi.dplot()) {
      updateSwitchInput(session, "dplot.do.hover", value = dplot.args$do.hover, disabled = FALSE)
    }
  })
  
  observeEvent(input$dplot.group.by, {
    dplot.args$group.by <- input$dplot.group.by
  })
  
  observeEvent(input$ddimplot.genes, {
    
    if(length(c(input$ddimplot.genes, input$ddimplot.metas)) > 1) {
      if(!multi.ddimplot()) {
        ddimplot.args$do.hover <- input$ddimplot.do.hover
        ddimplot.args$legend.show <- input$ddimplot.legend.show
      }
      multi.ddimplot(TRUE)
    } else {
      multi.ddimplot(FALSE)
    }
    
    if(multi.ddimplot()) {
      shinyjs::show(id = "multi_ddimplot_aes", anim = TRUE)
      updateSwitchInput(session, "ddimplot.do.hover", value = FALSE, disabled = TRUE)
      updateSwitchInput(session, "ddimplot.legend.show", value = FALSE)
    } else {
      shinyjs::hide(id = "multi_ddimplot_aes", anim = TRUE)
      updateSwitchInput(session, "ddimplot.do.hover", value = ddimplot.args$do.hover, disabled = FALSE)
      updateSwitchInput(session, "ddimplot.legend.show", value = ddimplot.args$legend.show)
    }
  })
  
  observeEvent(input$ddimplot.metas, {
    
    if(length(c(input$ddimplot.genes, input$ddimplot.metas)) > 1) {
      if(!multi.ddimplot()) {
        ddimplot.args$do.hover <- input$ddimplot.do.hover
        ddimplot.args$legend.show <- input$ddimplot.legend.show
      }
      multi.ddimplot(TRUE)
    } else {
      multi.ddimplot(FALSE)
    }
    
    if(multi.ddimplot()) {
      shinyjs::show(id = "multi_ddimplot_aes", anim = TRUE)
      updateSwitchInput(session, "ddimplot.do.hover", value = FALSE, disabled = TRUE)
      updateSwitchInput(session, "ddimplot.legend.show", value = FALSE)
    } else {
      shinyjs::hide(id = "multi_ddimplot_aes", anim = TRUE)
      updateSwitchInput(session, "ddimplot.do.hover", value = ddimplot.args$do.hover, disabled = FALSE)
      updateSwitchInput(session, "ddimplot.legend.show", value = ddimplot.args$legend.show)
    }
  })
  
  observeEvent(input$ddimplot.do.hover, {
    
    if(input$ddimplot.do.hover) {
      ddimplot.args$keep.square <- input$ddimplot.keep.square
      ddimplot.args$legend.show <- input$ddimplot.legend.show
      updateSwitchInput(session, "ddimplot.keep.square", value = FALSE, disabled = TRUE)
      updateSwitchInput(session, "ddimplot.legend.show", value = ddimplot.args$legend.show)
    } else {
      updateSwitchInput(session, "ddimplot.keep.square", value = ddimplot.args$keep.square, disabled = FALSE)
    }
  })
  
  # Handle when Reset Plot buttons are used.
  observeEvent(input$dplot.reset, {
    dplot.args <- .init_dplot_args()
    for(f in names(dataset()[["dplot.defaults"]])) {
      dplot.args[[f]] <- dataset()[["dplot.defaults"]][[f]]
    }
    
    # These have to be updated manually, shinyjs doesn't recognize them.
    updateColourInput(session, "dplot.jitter.color", value = dplot.args$jitter.color)
    updateColourInput(session, "dplot.boxplot.color", value = dplot.args$boxplot.color)
    updateColourInput(session, "dplot.line.color", value = dplot.args$line.color)

    reset("dplot")
  })
  
  observeEvent(input$ddimplot.reset, {
    ddimplot.args <- .init_ddimplot_args()
    for(f in names(dataset()[["ddimplot.defaults"]])) {
      ddimplot.args[[f]] <- dataset()[["ddimplot.defaults"]][[f]]
    }
    
    # These have to be updated manually, shinyjs doesn't recognize them.
    updateColourInput(session, "ddimplot.min.color", value = ddimplot.args$min.color)
    updateColourInput(session, "ddimplot.max.color", value = ddimplot.args$max.color)
    updateColourInput(session, "ddimplot.contour.color", value = ddimplot.args$contour.color)
    
    reset("ddimplot")
  })
  
  observeEvent(input$distheat.reset, {
    # These have to be updated manually, shinyjs doesn't recognize them.
    updateColourInput(session, "distheat.low", value = "#E38E0D")
    updateColourInput(session, "distheat.mid", value = "#93D1E6")
    updateColourInput(session, "distheat.high", value = "#2303A3")
    updateColourInput(session, "distheat.border_color", value = "grey60")
    
    reset("distheat")
  })
  
}

shinyApp(ui, server)
