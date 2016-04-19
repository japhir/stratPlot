# Super easy depth plotting in R
# Ilja Kocken
# Student of Marine Sciences at Utrecht University
# First version: 2014-04-11
# Latest version: 2016-04-06

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
    xax = 3,                   # draw x-axis, 1 for bot, 3 for top, NA for none
    type = "o",                # default type
    pb = "n",                  # plot bars and/or area using "P", "PB" or "B"
    left = par("usr")[1],      # from where are area and bars are drawn
    fillcol = "steelblue",     # colour of silhouette 
    ylab = "Depth (mbsf)",     # default ylab
    xlab = "",                 # default xlab xtitle
    ylim = c(max(yvar, na.rm=TRUE), min(yvar, na.rm=TRUE)), # default ylim
    legend = FALSE,             # default no legend
    legendpos = "topright",    # default legend position
    bty = "n",                 # default legend box type 
    mar = c(2, 5, 5, 2) + 0.1, # bottom, left, top and right margins
    ...){                      # possible additional plotting/legend parameters
  # set up plotting margins.
  if (identical(xax, 1) && identical(mar, c(2, 5, 5, 2) + 0.1))  # if default, override for bot
    mar <- c(5, 5, 2, 2) + 0.1
  par(mar = mar)
  
  if (!add) {  # create empty plot
    plot(var, yvar, ylim = ylim, type = "n", xaxt = "n", xlab = "", ylab = ylab, ...)
    if(xax %in% c(1,3)) 
      axis(xax)
    if(!is.null(xlab) && xax %in% c(1,3))
      mtext(xlab, side = xax, line=2)
    if(legend)
      legend(legendpos, legend = legend, bty = bty, ...)
  }
  if (pb == "PB" || pb == "BP") { # for polygon and bars
    area <- TRUE
    bars <- TRUE
  } else if (pb == "P") { # polygon
    area <- TRUE
    bars <- FALSE
  } else if (pb == "B") { # bars
    area <- FALSE
    bars <- TRUE
  } else if (pb == "n") {
    area <- FALSE
    bars <- FALSE
  } else warning ("Incorrect pb")
  
  if (area) { 
    if (anyNA(var) || anyNA(yvar)) {
      message("NAs found in var/yvar, currently ignoring")
      nona <- data.frame(var = var, yvar = yvar)
      nona <- nona[complete.cases(nona), ]
      x <- c(left, nona$var, left)
      y <- c(nona$yvar[1], nona$yvar, tail(nona$yvar, n = 1))
      # TODO: create polygons per non-NA region, like we do with errorregion
      #      enc <- rle(!is.na(var))
      #     endIdxs <- cumsum(enc$lengths)
      #    for (i in 1:length(enc$lengths)) {
      #     if (enc$values[i]) {
      #      endIdx <- endIdxs[i]
      #     startIdx <- endIdx - enc$lengths[i] + 1
      #    subvar <- var[startIdx:endIdx]
      #  subyvar <- yvar[startIdx:endIdx]
      #   x <- c(left, subvar, left)
      # y <- c(subyvar[1], yvar, tail(yvar, n = 1))
      #}
      #}
    } else {
    # not sure if I want to use the current left or use:
    # min(var, na.rm = TRUE) - .04 * diff(range(var, na.rm = TRUE))
      x <- c(left, var, left)
      y <- c(yvar[1], yvar, tail(yvar, n = 1))
    }
    polygon(x = x, y = y, col = fillcol, border = NA)
  }
  if (length(error) > 0) {  # plot errorstuff
    if(errortype == "bars") {
      PlotErrorBars(var, yvar, error, errorcol = errorcol)
    } else if (errortype == "area")
      PlotErrorArea(var, yvar, error, col = errorcol)
  }
  if (bars) {
    segments(x0 = rep(left, length(var)), y0 = yvar, x1 = var)
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
  # TODO: still/again? not working with NAs
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
        
        polygon(x = x, y = y, col = col, border = NA)
      }
    }
  } else {
    x <- c(var - .5 * errorregion, rev(var + .5 * errorregion))
    y <- c(yvar, rev(yvar))
    polygon(x = x, y = y, col = col, border = NA)
  }
}

# Takes a dataframe of one or multiple variable(s) to  create a (set of) plot(s)
SuperPlot.data.frame <- function(
    var,                       # a dataframe of variable(s)
    yvar = NULL,               # a vector of depth or age values
    xlab = "",                 # a vector of characters or a list of formulae 
    ylab = "Depth (mbsf)",
    type = "o",
    pb = "n",                  # allows 'PB', 'P' and 'B' for polygon with bars
    fillcols = rep("steelblue", ncol(var)),
    oneplot = FALSE,           # logical, if TRUE plot all variables in the same plot
    stacked = FALSE,           # logical, if TRUE calculate cumulative sum for vars
    sscols = 1:ncol(var),      # specifies columns of var to subset
    error = numeric(),         # vector of errors to plot (note: relative values!)
    errortype = "bars",        # bars or area 
    errorcol = adjustcolor("gray", alpha = 0.9),  # colour of errorbars/region
    ...) {
  var <- var[, c(sscols)] # subset only columns of interest   
  # parameter validation
  if(!is.null(yvar)){
    warning("Assuming only variables in dataframe")
  } else { # only var is provided
    # find value that has yvar
    depthcol <- grep("depth", names(var), ignore.case = TRUE, value = TRUE)
    if (length(depthcol) > 1) warning("multiple depth columns found, using first")
    agecol <- grep("age", names(var), ignore.case = TRUE, value = TRUE)
    if (length(agecol) > 1) warning("multiple age columns found, using first")
    if (length(depthcol) == 1 && length(agecol) == 0) 
      ycol <- depthcol[1]
    if (length(depthcol) == 0 && length(agecol) == 1) {
      ycol <- agecol[1]
      if (ylab == "Depth (mbsf)")
        ylab <- "Age (Ma)"
    }
    if (length(depthcol) > 0 && length(agecol) > 0) {
      # TODO: interactive selection of desired yvar
      yvar <- var[, agecol[1]]  # for now we just use age if both are available
      if (ylab == "Depth (mbsf)")
        ylab <- "Age (Ma)"
      # omit depth and age
      var <- var[, !names(var) %in% depthcol & !names(var) %in% agecol] 
    } else { 
    yvar <- var[, ycol]
    var   <- var[ , !names(var) %in% ycol]
    }
  }
  # TODO: think of a smart way to provide multiple types, pchs, ltys, fillcols, errors etc.

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
    return(SuperPlot(var, yvar, xlab = xlab, ylab = ylab, error = error, errortype = errortype,
                     type = type, pb = pb, ...))
  # multiple variables
  # everything in one plot
  invisible(      # hide output, such as lists of NULL from lapply
    if(oneplot){
      rangeofall <- c(min(var, na.rm = TRUE), max(var, na.rm = TRUE))
      # plot the first variable
      SuperPlot(var[, 1], yvar, type = type, pb = pb,
                xlim = rangeofall, ylab = ylab, xlab = xlab,
                error = error, errortype = errortype, errorcol = errorcol,
                fillcol = fillcols[1], ...) 
      # add the other variables
      if (stacked) {
        lapply(2:ncol(var), function(i) {
          SuperPlot(rowSums(var[, 1:i], na.rm = TRUE), yvar, add = TRUE, type = type,
                    pb = pb, error = error, errortype = errortype,
                    errorcol = errorcol, fillcol = fillcols[i], ...)
        })
      } else {
        lapply(2:ncol(var), function(i) {
          SuperPlot(var[ , i], yvar, add = TRUE, type = type, pb = pb,
                    error = error, errortype = errortype,
                    errorcol = errorcol, fillcol = fillcols[i], ...)
        })
      }
      # multiple plots
    } else if(xlab == ""){ 
    # no or wrong xlab 
      lapply(1:ncol(var), function(i) { 
        SuperPlot(var[ , i], yvar, xlab = names(var)[i], ylab = ylab, type = type, pb = pb,
                  fillcol = fillcols[i], error = error, errortype = errortype, ...)
      })
    } else if(length(xlab) == 1) # multiplot with provided xlab repeated
      lapply(1:ncol(var), function(i) { 
        SuperPlot(var[ , i], yvar, xlab = xlab, type = type, ylab = ylab,
                  error = error, errortype = errortype, pb = pb, fillcol = fillcols[i], ...)
      })
    # multiple xlabs
    else if(length(xlab) == ncol(var))      
      lapply(1:ncol(var), function(i) { 
        SuperPlot(var[ , i], yvar, xlab = xlab[[i]], ylab = ylab, type = type, fillcol = fillcols[i],
                  error = error, errortype = errortype, pb = pb, ...)
      })
  ) # end of invisible
}

SubSetRange <- function(dat, min, max, column = "depth") {
    dat[dat[, column] > min & dat[, column] < max, ]
}

