# Utility functions for argument collection and handling.
.init_dplot_args <- function() {
  reactiveValues(
    genes = NULL,
    metas = NULL,
    group.by = NULL,
    color.by = NULL,
    shape.by = NULL,
    split.by = NULL,
    extra.vars = NULL,
    cells.use = NULL,
    plots = c("vlnplot", "jitter", "boxplot"),
    assay = NULL,
    adjustment = NULL,
    do.hover = TRUE,
    hover.data = NULL,
    shape.panel = c(16, 15, 17, 23, 25, 8),
    theme = theme_classic(),
    main = "make",
    sub = NULL,
    ylab = "make",
    y.breaks = NULL,
    min = NULL,
    max = NULL,
    xlab = NULL,
    x.labels = NULL,
    x.labels.rotate = TRUE,
    x.reorder = NULL,
    split.nrow = NULL,
    split.ncol = NULL,
    jitter.size = 1,
    jitter.width = 0.2,
    jitter.color = "black",
    jitter.shape.legend.size = NA,
    jitter.shape.legend.show = TRUE,
    boxplot.width = 0.2,
    boxplot.color = "black",
    boxplot.fill = TRUE,
    boxplot.position.dodge = NULL,
    vlnplot.lineweight = 1,
    vlnplot.width = 1,
    vlnplot.scaling = "area",
    ridgeplot.lineweight = 1,
    ridgeplot.scale = 1.25,
    ridgeplot.ymax.expansion = NA,
    add.line = NULL,
    line.linetype = "dashed",
    line.color = "black",
    legend.show = TRUE,
    legend.title = "make",
    nrow = 1,
    ncol = NULL,
    coord.flip = FALSE
  )
}

.init_ddimplot_args <- function() {
  reactiveValues(
    genes = NULL,
    metas = NULL,
    reduction.use = NULL,
    size = 2,
    opacity = 1,
    dim.1 = 1,
    dim.2 = 2,
    cell.use = NULL,
    shape.by = NULL,
    split.by = NULL,
    split.nrow = NULL,
    split.ncol = NULL,
    assay = NULL,
    adjustment = NULL,
    show.others = TRUE,
    show.axes.numbers = TRUE,
    min.color = "#F0E442",
    max.color = "#0072B2",
    min = NULL,
    max = NULL,
    order = "unordered",
    main = "make",
    sub = NULL,
    xlab = "make",
    ylab = "make",
    theme = theme_bw(),
    do.letter = FALSE,
    do.ellipse = FALSE,
    do.label = FALSE,
    labels.size = 5,
    labels.highlight = TRUE,
    labels.repel = TRUE,
    do.hover = TRUE,
    hover.data = NULL,
    hover.adjustment = NULL,
    do.contour = FALSE,
    contour.color = "black",
    contour.linetype = "solid",
    legend.show = TRUE,
    legend.size = 5,
    legend.title = "make",
    shape.legend.size = 5,
    shape.legend.title = NULL,
    do.raster = FALSE,
    raster.dpi = 300
  )
}

.collect_dittoplot_args <- function() {
  
  out <- list()
  ge <- c(input$dplot.genes, input$dplot.metas)
  
  if (!is.null(input$metadata_rows_all) && input$dplot.use.filts) {
    out$cells.use <- input$metadata_rows_all
  } else {
    out$cells.use <- NULL
  }
  
  if (input$dplot.color.by == "") {
    out$color.by <- input$dplot.group.by
  } else {
    out$color.by <- input$dplot.color.by
  }
  
  if (input$dplot.shape.by == "") {
    out$shape.by <- NULL
  } else {
    out$shape.by <- input$dplot.shape.by
  }
  
  if (input$dplot.adjustment == "") {
    out$adjustment <- NULL
  } else {
    out$adjustment <- input$dplot.adjustment
  }
  
  if (is.na(input$dplot.split.nrow)) {
    out$split.nrow <- NULL
  } else {
    out$split.nrow <- input$dplot.split.nrow
  }
  
  if (is.na(input$dplot.split.ncol)) {
    out$split.ncol <- NULL
  } else {
    out$split.ncol <- input$dplot.split.ncol
  }
  
  if (is.na(input$dplot.add.line)) {
    out$add.line <- NULL
  } else {
    out$add.line <- input$dplot.add.line
  }
  
  out$theme <- theme_classic()
  if (input$dplot.keep.square) {
    out$theme <- out$theme + theme(aspect.ratio = 1) 
  }
  
  if(multi.dplot()) {
    if (is.na(input$dplot.nrow)) {
      out$nrow <- NULL
    } else {
      out$nrow <- input$dplot.nrow
    }
    
    if (is.na(input$dplot.ncol)) {
      out$ncol <- NULL
    } else {
      out$ncol <- input$dplot.ncol
    }
    
    out$vars <- ge
  } else {
    out$var <- ge
  }
  
  out$group.by <- input$dplot.group.by
  out$split.by <- input$dplot.split.by
  out$assay <- input$dplot.assay
  out$do.hover <- input$dplot.do.hover
  out$hover.data <- c(input$dplot.hover.data, ge)
  out$plots <- as.character(input$dplot.plots)
  out$legend.show <- input$dplot.legend.show
  out$legend.title <- input$dplot.legend.title
  
  out$main <- input$dplot.main
  out$sub <- input$dplot.sub
  out$ylab <- input$dplot.ylab
  out$xlab <- input$dplot.xlab
  out$min <- input$dplot.min
  out$max <- input$dplot.max
  out$x.labels.rotate <- input$dplot.x.labels.rotate
  out$x.reorder <- as.numeric(input$dplot.x.reorder_order)
  
  out$jitter.size <- input$dplot.jitter.size
  out$jitter.width <- input$dplot.jitter.width
  out$jitter.color <- input$dplot.jitter.color
  out$jitter.shape.legend.show <- input$dplot.jitter.shape.legend.show
  out$boxplot.width <- input$dplot.boxplot.width
  out$boxplot.color <- input$dplot.boxplot.color
  out$boxplot.fill <- input$dplot.boxplot.fill
  out$boxplot.position.dodge <- input$dplot.boxplot.position.dodge
  out$vlnplot.lineweight <- input$dplot.vlnplot.lineweight
  out$vlnplot.width <- input$dplot.vlnplot.width
  out$vlnplot.scaling <- input$dplot.vlnplot.scaling
  out$ridgeplot.lineweight <- input$dplot.ridgeplot.lineweight
  out$ridgeplot.scale <- input$dplot.ridgeplot.scale
  out$ridgeplot.ymax.expansion <- input$dplot.ridgeplot.ymax.expansion
  out$line.linetype <- input$dplot.line.linetype
  out$line.color <- input$dplot.line.color
  
  out
}

.collect_dittodimplot_args <- function() {
  
  out <- list()
  ge <- c(input$ddimplot.genes, input$ddimplot.metas)
  
  if (!is.null(input$metadata_rows_all) && input$ddimplot.use.filts) {
    out$cells.use <- input$metadata_rows_all
  } else {
    out$cells.use <- NULL
  }
  
  if (input$ddimplot.shape.by == "") {
    out$shape.by <- NULL
  } else {
    out$shape.by <- input$ddimplot.shape.by
  }
  
  if (input$ddimplot.adjustment == "") {
    out$adjustment <- NULL
  } else {
    out$adjustment <- input$ddimplot.adjustment
  }
  
  if (is.na(input$ddimplot.split.nrow)) {
    out$split.nrow <- NULL
  } else {
    out$split.nrow <- input$ddimplot.split.nrow
  }
  
  if (is.na(input$ddimplot.split.ncol)) {
    out$split.ncol <- NULL
  } else {
    out$split.ncol <- input$ddimplot.split.ncol
  }
  
  out$theme <- theme_bw()
  if (input$ddimplot.keep.square) {
    out$theme <- out$theme + theme(aspect.ratio = 1) 
  }
  
  if(multi.ddimplot()) {
    if (is.na(input$ddimplot.nrow)) {
      out$nrow <- NULL
    } else {
      out$nrow <- input$ddimplot.nrow
    }
    
    if (is.na(input$ddimplot.ncol)) {
      out$ncol <- NULL
    } else {
      out$ncol <- input$ddimplot.ncol
    }
    
    out$vars <- ge
  } else {
    out$var <- ge
  }
  
  out$reduction.use = input$ddimplot.reduction.use
  out$size <- input$ddimplot.size
  out$opacity <- input$ddimplot.opacity
  out$dim.1 <- input$ddimplot.dim.1
  out$dim.2 <- input$ddimplot.dim.2
  out$split.by <- input$ddimplot.split.by
  out$assay <- input$ddimplot.assay
  out$show.others <- input$ddimplot.show.others
  out$show.axes.numbers <- input$ddimplot.show.axes.numbers
  out$min.color <- input$ddimplot.min.color
  out$max.color <- input$ddimplot.max.color
  out$min <- input$ddimplot.min
  out$max <- input$ddimplot.max
  out$order <- input$ddimplot.order
  out$main <- input$ddimplot.main
  out$sub <- input$ddimplot.sub
  out$ylab <- input$ddimplot.ylab
  out$xlab <- input$ddimplot.xlab
  out$do.letter <- input$ddimplot.do.letter
  out$do.ellipse <- input$ddimplot.do.ellipse
  
  out$do.label <- input$ddimplot.do.label
  out$labels.size <- input$ddimplot.labels.size
  out$labels.highlight <- input$ddimplot.labels.highlight
  out$labels.repel <- input$ddimplot.labels.repel
  
  out$do.hover <- input$ddimplot.do.hover
  out$hover.data <- c(input$ddimplot.hover.data, ge)
  out$hover.adjustment <- input$ddimplot.hover.adjustment
  
  out$do.contour <- input$ddimplot.do.contour
  out$contour.color <- input$ddimplot.contour.color
  out$contour.linetype <- input$ddimplot.contour.linetype

  out$legend.show <- input$ddimplot.legend.show
  out$legend.size <- input$ddimplot.legend.size
  out$legend.title <- input$ddimplot.legend.title
  out$shape.legend.size <- input$ddimplot.shape.legend.size
  
  out
}