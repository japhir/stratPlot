# Super easy depth plotting in R
# Ilja Kocken
# Student of Marine Sciences at Utrecht University
# First version: 2014-04-11
# Latest version: 2015-09-13

SuperPlot <- function(var, ...){
    UseMethod("SuperPlot", var)
}

# Creates a plot based on a depth/age vector and a variable vector
SuperPlot.default <- function(
    var,                       # a vector of the variable values
    yvar,                      # a vector of depth or age values
    add = FALSE,               # logical, add the plot to the current plot
    error = numeric(),         # vector of errors
    errortype = "bars",        # plot as bars or as area
    errorcol = adjustcolor("gray", alpha = 0.9),  # colour of error
    xax = TRUE,                # logical, draw x-axis
    type = "P",                # default type
    fillcol = "blue",          # colour of silhouette 
    ylab = "Depth (mbsf)",     # default ylab
    xlab = "",                 # default xlab xtitle
    ylim = c(max(yvar, na.rm=TRUE), min(yvar, na.rm=TRUE)), # default ylim
    legend = FALSE,             # default no legend
    legendpos = "topright",    # default legend position
    bty = "n",                 # default legend box type 
    mar = c(2, 5, 5, 2) + 0.1, # default plot margins
    ...){                      # possible additional plotting/legend parameters
    # set up plotting margins.
  par(mar = mar)
  if (!add) {  # create empty plot
    plot(var, yvar, ylim = ylim, type = "n", xaxt = "n", xlab = "", ylab = ylab, ...)
    if(xax) 
      axis(3)
    if(!is.null(xlab))
      mtext(xlab, side=3, line=2)
    if(legend)
      legend(legendpos, legend = legend, bty = bty, ...)
  }
  if (length(error) > 0) {  # plot errorstuff
    if(errortype == "bars") {
      PlotErrorBars(var, yvar, error, errorcol = errorcol)
    } else if (errortype == "area")
      PlotErrorArea(var, yvar, error, col = errorcol)
  }
  if (type == "P") {  # the nonstandard polygon type
    # check for complete cases without building a data.frame
    if (anyNA(var)) {
      message("NAs found in var")
      yvar <- yvar[!is.na(var)]
      var <- na.omit(var)
    }
    if (anyNA(yvar)) {
      message("NAs found in yvar")
      var <- var[!is.na(yvar)]
      yvar <- na.omit(yvar)
    }
    # the far left side of the plot
    left <- par("usr")[1]
      # or use: min(var, na.rm = TRUE) - .04 * diff(range(var, na.rm = TRUE))
    polygon(x = c(left, var, left),
            y = c(yvar[1], yvar, tail(yvar, n = 1)),
            col = fillcol, border = NA)
    segments(x0 = rep(left, length(var)),
             y0 = yvar,
             x1 = var)
    type <- "o"
  }
  points(var, yvar, type = type, ...)  # plot main line
}


PlotErrorBars <- function(var, yvar, errorbars,
                          errorwidth = diff(range(yvar))/100,
                          errorcol = adjustcolor("gray", alpha = 0.9)) {
  # plot the errorbars themselves
  segments(x0 = var - 0.5 * errorbars,
           y0 = yvar,
           x1 = var + 0.5 * errorbars,
           y1 = yvar, col = errorcol)
  # add whiskers
  if (errorwidth > 0) {
    segments(x0 = var - 0.5 * errorbars,  # on the left
             y0 = yvar - errorwidth,
             y1 = yvar + errorwidth, col = errorcol)
    segments(x0 = var + 0.5 * errorbars,  # on the right
             y0 = yvar - errorwidth,
             y1 = yvar + errorwidth, col = errorcol)
  }
}

PlotErrorArea <- function(var, yvar, errorregion, hor = TRUE, col = adjustcolor("gray", .3)) {
  # adds an errorregion to a variable
  if (anyNA(var) | anyNA(yvar)) {
    enc <- rle(!is.na(var))             # calculate amount of non-NA polygons
    endIdxs <- cumsum(enc$lengths)      # lengths of polygons
    for(i in 1:length(enc$lengths)){    # for each polygon
      if(enc$values[i]){                # for non-na regions
        endIdx <- endIdxs[i]
        startIdx <- endIdx - enc$lengths[i] + 1
        
        subdat <- var[startIdx:endIdx]
        subsd <- errorregion[startIdx:endIdx]
        subyvar <- yvar[startIdx:endIdx]
        
        x <- c(subdat - subsd, rev(subdat + subsd))
        y <- c(subyvar, rev(subyvar))
      }
    }
  } else {
        x <- c(var - .5 * errorregion, rev(var + .5 * errorregion))
        y <- c(yvar, rev(yvar))
  }
  polygon(x = x, y = y, col = col, border = NA)
  
}

# Takes a dataframe of one or multiple variable(s) to  create a (set of) plot(s)
SuperPlot.data.frame <- function(
    var,                       # a dataframe of variable(s)
    yvar = NULL,               # a vector of depth or age values
    xlab = "",                 # a vector of characters or a list of formulae 
                # if length(xlab) > 1 gives errors when checking but works
    ylab = "Depth (mbsf)",
    type = "P",
    fillcol = "blue",
    oneplot = FALSE,           # logical, if TRUE plot all variables in the same plot
    sscols = 1:ncol(var),      # specifies columns of var to subset
    error = numeric(),         # vector of errors to plot
    errortype = "bars",        # bars or area 
    errorcol = adjustcolor("gray", alpha = 0.9),  # colour of errorbars/region
    ...){
  var <- var[, c(sscols)] # subset only columns of interest   
  # parameter validation
  if(anyNA(var)) { warning("NAs found in var, ignoring") }
  if(!is.null(yvar)){
    if(anyNA(yvar)) { warning("NAs found in yvar, ignoring") }
    warning("Assuming only variables in dataframe")
  } else { # only var is provided
    # find value that has yvar
    depthcol <- grep("depth", names(var), ignore.case = TRUE, value = TRUE)
    agecol <- grep("age", names(var), ignore.case = TRUE, value = TRUE)
    if (length(depthcol) == 1 && length(agecol) == 0)
      ycol <- depthcol
    if (length(depthcol) == 0 && length(agecol) == 1) {
      ycol <- agecol
      if (ylab == "Depth (mbsf)")
        ylab <- "Age (Ma)"
    }
    if (length(depthcol) > 0 && length(agecol) > 0) {
      # TODO: interactive selection of desired yvar
      # TODO: check that this is only one column
      yvar <- var[, agecol]  # for now we just use age if both are available
      if (ylab == "Depth (mbsf)")
        ylab <- "Age (Ma)"
      # omit depth and age
      var <- var[, !names(var) %in% depthcol & !names(var) %in% agecol] 
    } else { 
    yvar <- var[, ycol]
    var   <- var[ , !names(var) %in% ycol]
    }
  }
  # TODO: do the same thing with other parameters that might be different for each
  # plot, such as fillcol, type, error, errortype, errorcol

  # parsing of xlab
  if(xlab != ""){
    if(class(xlab) == "formula"){
      if(length(xlab == 1))
        xlab <- as.expression(xlab)
      else
        lapply(xlab, as.expression)
    }
    if(length(xlab) > 1 && length(xlab) != ncol(var)){ 
      warning("Incorrect length of xlab, ignoring")
      xlab <- ""
    }
  }
  
  # only one variable in the dataframe var
  if(is.null(ncol(var)))
    return(SuperPlot(var, yvar, xlab = xlab, ylab = ylab, error = error, ...))
  # multiple variables
  # everything in one plot
  invisible(      # hide output, such as lists of NULL from lapply
    if(oneplot){
      rangeofall <- c(min(var, na.rm = TRUE), max(var, na.rm = TRUE))
      # plot the first variable
      SuperPlot(var[, 1], yvar, type = type, xlim = rangeofall, ylab = ylab,
                xlab = xlab, error = error, errortype = errortype,
                errorcol = errorcol, fillcol = fillcol, ...) 
      # add the other variables
      lapply(2:ncol(var), function(i) {
        SuperPlot(var[ , i], yvar, add = TRUE, type = type,
                  error = error, errortype = errortype,
                  errorcol = errorcol, fillcol = fillcol, ...)
      })
      # multiple plots
    } else if(xlab == ""){ 
    # no or wrong xlab 
      lapply(1:ncol(var), function(i) { 
        SuperPlot(var[ , i], yvar, xlab = names(var)[i], ylab = ylab,
                     error = error, ...)
      })
    } else if(length(xlab) == 1) # multiplot with provided xlab repeated
      lapply(1:ncol(var), function(i) { 
        SuperPlot(var[ , i], yvar, xlab = xlab, ylab = ylab, error = error, ...)
      })
    # multiple xlabs
    else if(length(xlab) == ncol(var))      
      lapply(1:ncol(var), function(i) { 
        SuperPlot(var[ , i], yvar, xlab = xlab[[i]], ylab = ylab, error = error, ...)
      })
  ) # end of invisible
}

SubSetRange <- function(dat, min, max, column = "depth") {
    dat[dat[, column] > min & dat[, column] < max, ]
}

