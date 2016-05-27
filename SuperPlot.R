# Super easy plotting of paleodata in R
# Ilja Kocken
# Student of Marine Sciences at Utrecht University
# First version: 2014-04-11
# Latest version: 2016-04-24

SuperPlot <- function(var, ...){
    UseMethod("SuperPlot", var)
}

# Creates a plot based on a depth/age vector and a variable vector
SuperPlot.default <- function(
    var,                       # a vector of the variable values
    yvar,                      # a vector of depth or age values
    pb = "n",                  # plot bars and/or area using "P", "PB" or "B
    error = numeric(),         # vector of errors
    ydir = "h",                # vertical or horizontal time axis?
    add = FALSE,               # logical, add the plot to the current plot
    ...){                      # possible additional plotting/legend parameters

  if (!(ydir == "v" || ydir == "h"))
    stop("ydir must be either 'v' vertical or 'h' horizontal")
  
  ellipsis <- list(...)

  # plot as bars or as area
  errortype <- co("errortype", "bars")    
  # colour of error
  errorcol  <- co("errorcol", adjustcolor("gray", alpha = 0.9)) 
  # draw x-axis, 1 for bot, 3 for top, NA for none
  xax       <- co("xax", if (ydir == "h") 1 else 3)
  yax       <- co("yax", 2) 
  type      <- co("type", "o")      # default type
  left      <- co("left", 0)        # from where are area and bars are drawn
  fillcol   <- co("fillcol", "steelblue")  # colour of silhouette 
  # default ylab
  ylab      <- co("ylab", if (ydir == "v") "Age (Ma)"
                          else if (ydir == "h") "")  
  # default xlab 
  xlab      <- co("xlab", if (ydir == "v") ""
                          else if (ydir == "h") "Age (Ma)")
  xlim      <- co("xlim", if (ydir == "v") range(var, na.rm = TRUE)
                          else if (ydir == "h") rev(range(yvar, na.rm = TRUE)))
  # default ylim
  ylim      <- co("ylim", if (ydir == "v") rev(range(yvar, na.rm = TRUE))
                          else if (ydir == "h") range(var, na.rm = TRUE))
  legend    <- co("legend", FALSE)         # default no legend
  legendpos <- co("legendpos", "topright") # default legend position
  bty       <- co("bty", "n")              # default legend and plot box type 
  # default margins depend on where the xax is drawn, bottom, left ("v"), bottom + top or top
  mar       <- co("mar", c(if (1 %in% xax || 1 %in% yax) 5 else 2,
                           if (2 %in% xax || 2 %in% yax) 5 else 2,
                           if (3 %in% xax || 3 %in% yax) 5 else 2,
                           if (4 %in% xax || 4 %in% yax) 5 else 2) + .1)

  par(mar = mar, bty = bty)
  
  # create empty plot
  if (!add) {
    # set up blank plotting area
    plot(c(1,1), c(1,1), type = "n",
         xlim = xlim, ylim = ylim, 
         xlab = "", ylab = "", axes = FALSE)
    # add x-axis
    lapply(xax, axis)
    lapply(yax, axis)
    if (!is.null(xlab))
      lapply(xax, function(i) {mtext(xlab, side = i, line = 2)})
    if (!is.null(ylab))
      lapply(yax, function(i) {mtext(ylab, side = i, line = 2)})
  }

  # add plotting variables
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
    polygon(x = if (ydir == "h") y else x,
            y = if (ydir == "h") x else y, col = fillcol, border = NA)
  }
  
  if (length(error) > 0) {  # plot errorstuff
    if(errortype == "bars") {
      PlotErrorBars(if (ydir == "h") yvar else var,
                    if (ydir == "h") var else yvar,
                    error, errorcol = errorcol)
    } else if (errortype == "area")
      PlotErrorArea(if (ydir == "h") yvar else var,
                    if (ydir == "h") var else yvar,
                    error, col = errorcol)
  }
  if (bars) {
    segments(x0 = if (ydir == "h") yvar else rep(left, length(var)),
             y0 = if (ydir == "h") rep(left, length(var)) else yvar,
             x1 = if (ydir == "h") yvar else var,
             y1 = if (ydir == "h") var else yvar)
  }
  points(var, yvar, ...)  # plot main line
  if(legend)
    legend(legendpos, legend = legend, bty = bty, ...)
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
    ydir = "v",                # vertical or horizontal time axis?
    oneplot = FALSE,           # logical, if TRUE plot all variables in the same plot
    stacked = FALSE,           # logical, if TRUE calculate cumulative sum for vars
    error = numeric(),         # vector of errors to plot (note: relative values!)
    ...) {
  ellipsis <- list(...)
  ylab <- co("ylab", "Age (Ma)")
  xlab <- co("xlab", "")
  fillcols <- co("fillcols", rep("steelblue", length(var)))
  
  #subset numeric columns
  var <- var[, sapply(var, is.numeric)]
  # parameter validation
  if(!is.null(yvar)){
    message("Assuming only variables in dataframe")
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
      if (is.null(ellipsis$ylab))
        ylab <- "Age (Ma)"
    }
    if (length(depthcol) > 0 && length(agecol) > 0) {
      # TODO: interactive selection of desired yvar
      yvar <- var[, agecol[1]]  # for now we just use age if both are available
      if (is.null(ellipsis$ylab))
        ylab <- "Age (Ma)"
      # omit depth and age
      var <- var[, !names(var) %in% depthcol & !names(var) %in% agecol] 
    } else { 
    yvar <- var[, ycol]
    var  <- var[ , !names(var) %in% ycol]
    }
  }
  
  # parsing of xlab
  if(!is.null(ellipsis$xlab)) {
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
  if(ncol(var) == 1)
    return(SuperPlot(var, yvar, ...))
  # multiple variables
  # everything in one plot
  invisible(      # hide output, such as lists of NULL from lapply
    if(oneplot){
      rangeofall <- range(var, na.rm = TRUE)
      # plot the first variable
      SuperPlot(var[, 1], yvar, xlim = rangeofall, fillcol = fillcols[1], ...) 
      # add the other variables
      if (stacked) {
        lapply(2:ncol(var), function(i) {
          SuperPlot(rowSums(var[, 1:i], na.rm = TRUE), yvar, add = TRUE,
                    fillcol = fillcols[i], ...)
        })
      } else {
        lapply(2:ncol(var), function(i) {
          SuperPlot(var[ , i], yvar, add = TRUE, fillcol = fillcols[i], ...)
        })
      }
      # multiple plots
    } else if(xlab == ""){ 
    # no or wrong xlab 
      lapply(1:ncol(var), function(i) { 
        SuperPlot(var[ , i], yvar, xlab = names(var)[i], fillcol = fillcols[i], ...)
      })
    } else if(length(xlab) == 1) # multiplot with provided xlab repeated
      lapply(1:ncol(var), function(i) { 
        SuperPlot(var[ , i], yvar, xlab = xlab, fillcol = fillcols[i], ...)
      })
    # multiple xlabs
    else if(length(xlab) == ncol(var))      
      lapply(1:ncol(var), function(i) { 
        SuperPlot(var[ , i], yvar, xlab = xlab[[i]], fillcol = fillcols[i], ...)
      })
  ) # end of invisible
}

SubSetRange <- function(dat, min, max, column = "depth") {
    dat[dat[, column] > min & dat[, column] < max, ]
}

# check if str exists in opt, otherwise defines def
co <- function(str, def, opt = NA) {
  if (exists("ellipsis")) opt <- ellipsis
  if (is.na(opt)) stop("No options/list of ellipsis (...) specified")
  if (is.null(opt[[str]])) {
    # message(paste("hi I'm setting", str, "to", def))
    return(def)
  } else {
    # message(paste("hi I'm setting", str, "to", opt[[str]]))
    return(unlist(opt[[str]]))
  }
}

 
