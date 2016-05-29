# Super easy plotting of paleodata in R
# Ilja Kocken
# Student of Marine Sciences at Utrecht University
# First version: 2014-04-11
# Latest version: 2016-05-27

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
  
  if (!pb %in% c("n", "PB", "BP", "P", "B"))
    stop("Invalid pb, choose 'n' none, 'PB' polygon bar, 'P' polygon or 'B' bar")

  # validate '...', otherwise assign default values
  defs <- SetDefs(var = var, yvar = yvar, ydir = ydir, ...)

  par(mar = defs$mar, bty = defs$bty)
  
  # create empty plot
  if (!add) {
    # set up blank plotting area
    plot(c(1,1), c(1,1), type = "n", xlim = defs$xlim, ylim = defs$ylim,
         xlab = "", ylab = "", axes = FALSE)
      # add axes
    lapply(defs$xax, axis)
    lapply(defs$yax, axis)
    # add axis labels
    if (!is.null(defs$xlab)) {
      lapply(defs$xax, function(i) {mtext(defs$xlab, side = i, line = 2)})
    }
    if (!is.null(defs$ylab)) {
      lapply(defs$yax, function(i) {mtext(defs$ylab, side = i, line = 2)})
      }
  }
  
  # set plotting variables for PB
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
  } 
  
  # plot elements of PB
  if (area) { 
    if (anyNA(var) || anyNA(yvar)) {
      message("NAs found in var/yvar, currently ignoring")
      nona <- data.frame(var = var, yvar = yvar)
      nona <- nona[complete.cases(nona), ]
      x <- c(nona$yvar[1], nona$yvar, tail(nona$yvar, n = 1))
      y <- c(defs$pol0, nona$var, pol0)
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
      x <- c(yvar[1], yvar, tail(yvar, n = 1))
      y <- c(pol0, var, defs$pol0)
    }
    polygon(x = if (ydir == "h") x else y, y = if (ydir == "h") y else x,
            col = defs$fillcol, border = defs$border)
  }

  with(defs, {
    if (length(error) > 0) {  # plot errorstuff
      if(errortype == "bars") {
        PlotErrorBars(var, yvar, error, errorcol = errorcol, ydir = ydir)
      } else if (errortype == "area")
        PlotErrorArea(var, yvar, error, col = errorcol, ydir = ydir)
    }
  })

  # plot PB bars (added after errors to overlap the possible areas)
  if (bars) {
    with(defs, 
         segments(x0 = if (ydir == "h") yvar else rep(pol0, length(var)),
                  y0 = if (ydir == "h") rep(pol0, length(var)) else yvar,
                  x1 = if (ydir == "h") yvar else var,
                  y1 = if (ydir == "h") var else yvar))
  }
  
  # plot the actual record
  with(defs, 
       points(x = if (ydir == "h") yvar else var,
              y = if (ydir == "h") var else yvar, type = type, col = col,
              pch = pch, lty = lty, lwd = lwd))
  
  # plot a legend of one variable... is this necessary?
  with(defs,
  if(!is.na(legend))
    legend(legendpos, legend = legend, bty = bty, lty = lty, lwd = lwd, pch = pch))
}


PlotErrorBars <- function(var, yvar, errorbars,
                          errorwidth = diff(range(yvar))/100,
                          errorcol = adjustcolor("gray", alpha = 0.9),
                          ydir = "h") {
  # TODO also support vector of actual errors values
  # maybe the easy var <-> yvar solution could work here too
  if (ydir == "h")
  # plot the errorbars themselves
    segments(x0 = yvar, y0 = var - 0.5 * errorbars, x1 = yvar,
             y1 = var + 0.5 * errorbars, col = errorcol)
  # add whiskers
  if (errorwidth > 0) {
    segments(x0 = yvar - errorwidth,
             y0 = var - 0.5 * errorbars,
             x1 = yvar + errorwidth, col = errorcol)
    segments(x0 = yvar - errorwidth,
             y0 = var + 0.5 * errorbars,  # on the right
             x1 = yvar + errorwidth, col = errorcol)
  }
  else if (ydir == "v")
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

PlotErrorArea <- function(var, yvar, errorregion, hor = TRUE, col = adjustcolor("gray", .3), ydir = "h") {
  # adds an errorregion to a variable
  # TODO: still/again? not working with NAs
  if (ydir == "h") {
    # easy way out
    tmp <- yvar
    yvar <- var
    var <- tmp
  }
  
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
    ydir = "h",                # vertical or horizontal time axis?
    oneplot = FALSE,           # logical, if TRUE plot all variables in the same plot
    stacked = FALSE,           # logical, if TRUE calculate cumulative sum for vars
    error = numeric(),         # vector of errors to plot (note: relative values!)
    ...) {

  ellipsis <- list(...)

  defs <- SetDefs(var = var, yvar = yvar, ydir = ydir, ellipsis)
  
  co <- function(string, default, options = ellipsis) {
    if (is.null(options[[string]])) {
      # message(paste("hi I'm setting", str, "to", def))
      return(default)
    } else {
      # message(paste("hi I'm setting", str, "to", opt[[str]]))
      out <- unlist(options[[string]])
      #ellipsis[[string]] <- NULL
      #   opt[[str]] <- NULL # remove from list of ...
      return(out)
    }
  }

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
    if (length(depthcol) == 1 && length(agecol) == 0) {
      ycol <- depthcol[1]
    } else if (length(depthcol) == 0 && length(agecol) == 1) {
      ycol <- agecol[1]
      if (is.null(ellipsis$ylab)) {
        if (ydir == "h") defs$xlab = "Age (Ma)" else defs$ylab <- "Age (Ma)"
      }
    } else if (length(depthcol) > 0 && length(agecol) > 0) {
      # TODO: interactive selection of desired yvar
      yvar <- var[, agecol[1]]  # for now we just use age if both are available
      if (is.null(ellipsis$ylab))
        if (ydir == "h") defs$xlab = "Age (Ma)" else defs$ylab <- "Age (Ma)"
      # omit depth and age
      var <- var[, !names(var) %in% depthcol & !names(var) %in% agecol] 
    } else { # no depthcol, no agecol found: use first column 
      yvar <- var[,1]
      var  <- var[ , -1]
    }
  }
  
  # parsing of xlab
  if (ydir == "h") {
    if(!is.null(defs$ylab)) {
      if(class(defs$ylab) == "formula"){
        if(length(defs$ylab == 1))
          defs$ylab <- as.expression(defs$ylab)
        else
          lapply(defs$ylab, as.expression)
      }
      if(length(defs$ylab) > 1 && length(defs$ylab) != ncol(var)){ 
        warning("Incorrect length of ylab, ignoring")
        defs$ylab <- ""
      }
    }
  } else if (ydir == "v") {
    if(!is.null(ellipsis$xlab)) {
      if(class(defs$xlab) == "formula"){
        if(length(defs$xlab == 1))
          defs$xlab <- as.expression(defs$xlab)
        else
          lapply(defs$xlab, as.expression)
      }
      if(length(defs$xlab) > 1 && length(defs$xlab) != ncol(var)){ 
        warning("Incorrect length of xlab, ignoring")
        defs$xlab <- ""
      }
    }
  }
  
  # only one variable in the dataframe var
  if(ncol(var) == 1)
    return(SuperPlot(var, yvar, pb = pb, error = error, ydir = ydir, add = add, defs))
  # multiple variables
  # everything in one plot
  invisible(      # hide output, such as lists of NULL from lapply
    if(oneplot){
      rangeofall <- range(var, na.rm = TRUE)
      # plot the first variable
      defs$xlim <- rangeofall
      defs$fillcol <- fillcols[1]
      SuperPlot(var[, 1], yvar, pb = pb, error = error, ydir = ydir, add = add, defs) 
      # add the other variables
      if (stacked) {
        lapply(2:ncol(var), function(i) {
          defs$fillcol <- fillcols[i]
          SuperPlot(rowSums(var[, 1:i], na.rm = TRUE), yvar, pb = pb,
                    error = error, ydir = ydir, add = TRUE, defs)
        })
      } else {
        lapply(2:ncol(var), function(i) {
          defs$fillcol <- fillcols[i]
          SuperPlot(var[ , i], yvar, pb = pb, error = error, add = TRUE,
                    ydir = ydir, defs)
        })
      }
      # multiple plots
    } else if (ydir == "v" && defs$xlab == ""){
      
    # no or wrong xlab 
      lapply(1:ncol(var), function(i) { 
        defs$xlab <- names(var)[i]
        defs$fillcol <- fillcols[i]
        SuperPlot(var[ , i], yvar, pb = pb, error = error, ydir = ydir,
                  add = add, defs)
      })
    } else if(length(defs$xlab) == 1) # multiplot with provided xlab repeated
      lapply(1:ncol(var), function(i) { 
        defs$fillcol <- fillcols[i]
        SuperPlot(var[ , i], yvar, pb = pb, error = error, ydir = ydir,
                  add = add, defs)
      })
    # multiple xlabs
    else if(length(defs$xlab) == ncol(var))      
      lapply(1:ncol(var), function(i) { 
        defs$xlab <- defs$xlab[[i]]
        defs$fillcol <- fillcols[i]
        SuperPlot(var[ , i], yvar, pb = pb, error = error, ydir = ydir,
                  add = add, defs)
      })
  ) # end of invisible
}

SubSetRange <- function(dat, min, max, column = "depth") {
    dat[dat[, column] > min & dat[, column] < max, ]
}

# check if str exists in opt, otherwise defines def
#CheckEllipsis <- function(str, def, opt) {
#  if (is.null(opt[[str]])) {
#    # message(paste("hi I'm setting", str, "to", def))
#    return(def)
#  } else {
#    # message(paste("hi I'm setting", str, "to", opt[[str]]))
#    out <- unlist(opt[[str]])
#    parent.frame()$options[[str]] <- NULL
 #   opt[[str]] <- NULL # remove from list of ...
#    return(out)
#  }
#}

 

SetDefs <- function(var, yvar, ydir = "h", ...) {
 
  ellipsis <- list(...)

  # set default if not provided with '...'
  co <- function(string, default, options = ellipsis) {
    if (is.null(options[[string]])) {
      # message(paste("hi I'm setting", str, "to", def))
      return(default)
    } else {
      # message(paste("hi I'm setting", str, "to", opt[[str]]))
      out <- unlist(options[[string]])
      #ellipsis[[string]] <- NULL
      #   opt[[str]] <- NULL # remove from list of ...
      return(out)
    }
  }

  # plot as bars or as area
  errortype <- co("errortype", "bars")    
  # colour of error
  errorcol  <- co("errorcol", adjustcolor("gray", alpha = 0.9)) 

  # draw x-axis, 1 for bot, 3 for top, NA for none
  xax       <- co("xax", if (ydir == "h") 1 else 3)
  yax       <- co("yax", 2) 

  type      <- co("type", "o")             # default type
  col       <- co("col", "black")
  lty       <- co("lty", 1)
  lwd       <- co("lwd", 1)
  pch       <- co("pch", 16)
 
  # from where are area and bars are drawn
  pol0      <- co("pol0", 0)        
  fillcol   <- co("fillcol", "steelblue")  # colour of silhouette 
  border    <- co("border", NA)            # border of polygon

  # default ylab
  ylab      <- co("ylab", if (ydir == "h") ""
                          else "Age (Ma)")  
  # default xlab 
  xlab      <- co("xlab", if (ydir == "h") "Age (Ma)"
                          else "")
  # default xlim
  xlim      <- co("xlim", if (ydir == "h") rev(range(yvar, na.rm = TRUE))
                          else range(var, na.rm = TRUE))
  ylim      <- co("ylim", if (ydir == "h") range(var, na.rm = TRUE)
                  else rev(range(yvar, na.rm = TRUE)))
  legend    <- co("legend", NA)         # default no legend
  legendpos <- co("legendpos", "topright") # default legend position
  bty       <- co("bty", "n")              # default legend and plot box type

 # default margins depend on where the xax is drawn,
  # bottom, left ("v"), bottom + top or top
  mar       <- co("mar", c(if (1 %in% xax || 1 %in% yax) 5 else 2,
                           if (2 %in% xax || 2 %in% yax) 5 else 2,
                           if (3 %in% xax || 3 %in% yax) 5 else 2,
                           if (4 %in% xax || 4 %in% yax) 5 else 2) + .1)

  return(list(errortype = errortype,
              errorcol = errorcol,
              xax = xax,
              yax = yax,

              type = type,
              col = col,
              lty = lty,
              lwd = lwd,
              pch = pch,

              pol0 = pol0,
              fillcol = fillcol,
              border = border,

              ylab = ylab,
              xlab = xlab,

              xlim = xlim,
              ylim = ylim,

              legend = legend,
              bty = bty,

              mar = mar))
}
