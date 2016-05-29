# Super easy plotting of paleodata in R
# Ilja Kocken
# Student of Marine Sciences at Utrecht University
# First version: 2014-04-11
# Latest version: 2016-05-28

SuperPlot <- function(var, ...){
    UseMethod("SuperPlot", var)
}

# Creates a plot based on a depth/age vector and a variable vector
SuperPlot.numeric <- function(var,           # numeric vector
                              age = NULL,    # optional numeric vector 
                              agedir = "h",  # "v", "ver", "vertical" or "h" "hor" "horizontal" 
                              pb = "n",        # polygon/bar
                              oneplot = FALSE, # logical, if TRUE plot all variables in the same plot
                              add = FALSE,     # logical, add to plot or start new one
                              error = NULL,    # vector of errors to plot (note: relative values!)
                              stacked = FALSE, # logical, calculate cumulative sum for vars
                              xax = if (agedir == "h") 1 else 3, # default position of x-axis
                              ...,
                              yax = 2,         # default position of yaxis
                              mar = NULL,      # generated based on xax and yax
                              ylab = NULL, xlab = NULL, xlim = NULL,
                              ylim = NULL, bty = "n", col = "black", lwd = 1,
                              lty = 1, type = "o", pch = 16, errortype = NULL,
                              errorcol = NULL, pol0 = NULL, fillcol = NULL,
                              border = NULL, legend = NULL) {
  # check var and age
  if (length(var) != length(age)) {
    stop("Unequal length of var and age")
  }
  if (!is.numeric(var))
    as.numeric(var)
  if (!is.numeric(age))
    as.numeric(age)
  
  # check agedir
  if (!agedir %in% c("v", "ver", "vertical", "h", "hor", "horizontal")) {
    stop("agedir must be either 'v', 'ver', 'vertical' or 'h', 'hor, 'horizontal'")
  } else {
      agedir <- substr(agedir, 1, 1) # agedir is now either 'v' or 'h'
  }
    
  # check pb
  if (!pb %in% c("n", "PB", "BP", "P", "B")) {
    stop("Invalid pb, choose 'n' none, 'PB' polygon bar, 'P' polygon or 'B' bar")
  }

  # check error
  if (!is.null(error)) {
    # check errortype
    if (!is.null(errortype)) {
      if (!errortype %in% c("bars", "area")) {
        stop("Invalid errortype, choose 'bars' or 'area'") 
      }
    } else errortype <- "bars"  
    # check errorcol
    if (!is.null(errorcol)) {
      if (!errorcol %in% colors()) {
        stop("Invalid errorcol")
      }
    } else errorcol <- adjustcolor("gray", .9)  
  }

  # mar (depends on xax and yax, which are not checked)
  if (is.null(mar)) {
    mar <- c(if (1 %in% xax || 1 %in% yax) 5 else 2,
             if (2 %in% xax || 2 %in% yax) 5 else 2,
             if (3 %in% xax || 3 %in% yax) 5 else 2,
             if (4 %in% xax || 4 %in% yax) 5 else 2) + .1
  }
  
  # xlim, ylim
  if (agedir == "h") {
    if (is.null(xlim)) xlim <- rev(range(age, na.rm = TRUE))
    if (is.null(ylim)) ylim <- range(var, na.rm = TRUE)
  } else {
    if (is.null(xlim)) xlim <- range(var, na.rm = TRUE)
    if (is.null(ylim)) ylim <- rev(range(age, na.rm = TRUE))
  }
  
  # xlab, ylab
  if (agedir == "h") {
    if (is.null(xlab)) xlab <- "Age (Ma)"
    if (is.null(ylab)) ylab <- ""
  } else {
    if (is.null(xlab)) xlab <- ""
    if (is.null(ylab)) ylab <- "Age (Ma)"
  }

  # set up plotting margins
  par(mar = mar, bty = bty)
  
  # create empty plot
  if (!add) {
    # set up blank plotting area
    plot(c(1,1), c(1,1), type = "n", xlim = xlim, ylim = ylim,
         xlab = "", ylab = "", axes = FALSE)
    # add axes
    lapply(xax, axis)
    lapply(yax, axis)
    # add axis labels
    lapply(xax, function(i) { mtext(xlab, side = i, line = 2) })
    lapply(yax, function(i) { mtext(ylab, side = i, line = 2) })
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
  
  # plot PB polygon
  if (area) { 
    if (anyNA(var) || anyNA(age)) {
      message("NAs found in var/age, currently ignoring")
      nona <- data.frame(var = var, age = age)
      nona <- nona[complete.cases(nona), ]
      x <- c(nona$age[1], nona$age, tail(nona$age, n = 1))
      y <- c(pol0, nona$var, pol0)
        # TODO: create polygons per non-NA region, like we do with errorregion
        #      enc <- rle(!is.na(var))
        #     endIdxs <- cumsum(enc$lengths)
        #    for (i in 1:length(enc$lengths)) {
        #     if (enc$values[i]) {
        #      endIdx <- endIdxs[i]
        #     startIdx <- endIdx - enc$lengths[i] + 1
        #    subvar <- var[startIdx:endIdx]
        #  subage <- age[startIdx:endIdx]
        #   x <- c(left, subvar, left)
        # y <- c(subage[1], age, tail(age, n = 1))
        #}
        #}
    } else {
        # not sure if I want to use the current left or use:
        # min(var, na.rm = TRUE) - .04 * diff(range(var, na.rm = TRUE))
      x <- c(age[1], age, tail(age, n = 1))
      y <- c(pol0, var, pol0)
    }
    polygon(x = if (agedir == "h") x else y, y = if (agedir == "h") y else x,
            col = fillcol, border = border, ...)
  }
  
  if (length(error) > 0) {  # plot errorstuff
    if(errortype == "bars") {
      PlotErrorBars(var, age, error, errorcol = errorcol, age = age)
    } else if (errortype == "area")
      PlotErrorArea(var, age, error, col = errorcol, age = age)
  }

  # plot PB bars (added after errors to overlap the possible areas)
  if (bars) {
    segments(x0 = if (agedir == "h") age else rep(pol0, length(var)),
             y0 = if (agedir == "h") rep(pol0, length(var)) else age,
             x1 = if (agedir == "h") age else var,
             y1 = if (agedir == "h") var else age, ...)
  }
  
  # plot the actual record
  points(x = if (agedir == "h") age else var,
         y = if (agedir == "h") var else age, type = type, col = col,
         pch = pch, lty = lty, lwd = lwd, ...)
  
  # plot a legend of one variable... is this necessary?
  if(!is.null(legend))
    legend("topright", legend = legend, bty = bty, lty = lty, lwd = lwd,
           pch = pch)
}


PlotErrorBars <- function(var, age, errorbars,
                          errorwidth = diff(range(age))/100,
                          errorcol = adjustcolor("gray", alpha = 0.9),
                          agedir = "h") {
  # TODO also support vector of actual errors values
  # maybe the easy var <-> age solution could work here too
  if (agedir == "h")
  # plot the errorbars themselves
    segments(x0 = age, y0 = var - 0.5 * errorbars, x1 = yvar,
             y1 = var + 0.5 * errorbars, col = errorcol)
  # add whiskers
  if (errorwidth > 0) {
    segments(x0 = age - errorwidth,
             y0 = var - 0.5 * errorbars,
             x1 = age + errorwidth, col = errorcol)
    segments(x0 = age - errorwidth,
             y0 = var + 0.5 * errorbars,  # on the right
             x1 = age + errorwidth, col = errorcol)
  }
  else if (agedir == "v")
    # plot the errorbars themselves
    segments(x0 = var - 0.5 * errorbars,
             y0 = age,
             x1 = var + 0.5 * errorbars,
             y1 = age, col = errorcol)
  # add whiskers
  if (errorwidth > 0) {
    segments(x0 = var - 0.5 * errorbars,  # on the left
             y0 = age - errorwidth,
             y1 = age + errorwidth, col = errorcol)
    segments(x0 = var + 0.5 * errorbars,  # on the right
             y0 = age - errorwidth,
             y1 = age + errorwidth, col = errorcol)
  }
}

PlotErrorArea <- function(var, age, errorregion, hor = TRUE,
                          col = adjustcolor("gray", .3), agedir = "h") {
  # adds an errorregion to a variable
  # TODO: still/again? not working with NAs
  if (agedir == "h") {
    # easy way out
    tmp <- age
    age <- var
    var <- tmp
  }
  
  if (anyNA(var) | anyNA(age)) {
    enc <- rle(!is.na(var))             # calculate amount of non-NA polygons
    endIdxs <- cumsum(enc$lengths)      # lengths of polygons
    for (i in 1:length(enc$lengths)){   # for each polygon
      if(enc$values[i]){                # for non-na regions
        endIdx <- endIdxs[i]
        startIdx <- endIdx - enc$lengths[i] + 1
        
        subdat <- var[startIdx:endIdx]
          subsd <- errorregion[startIdx:endIdx]
        subage <- age[startIdx:endIdx]
        
        x <- c(subdat - subsd, rev(subdat + subsd))
          y <- c(subage, rev(subage))
        
        polygon(x = x, y = y, col = col, border = NA)
      }
    }
  } else {
    x <- c(var - .5 * errorregion, rev(var + .5 * errorregion))
    y <- c(age, rev(age))
    polygon(x = x, y = y, col = col, border = NA)
  }
}

# Takes a dataframe of one or multiple variable(s) to  create a (set of) plot(s)
SuperPlot.data.frame <- function(var, # dataframe
                                 age = NULL, # optional vector 
                                 agedir = "h",  # "v", "ver", "vertical" or "h" "hor" "horizontal" 
                                 pb = "n",     # polygon/bar
                                 oneplot = FALSE, # logical, if TRUE plot all variables in the same plot
                                 genframe = TRUE, # show plots in same window
                                 add = FALSE,
                                 error = NULL,    # vector of errors to plot (note: relative values!)
                                 stacked = FALSE, # logical, calculate cumulative sum for vars
                                 ...,
                                 xax = if (agedir == "h") 1 else 3, # default position of x-axis
                                 yax = 2,         # default position of yaxis
                                 mar = NULL,      # generated based on xax and yax
                                 ylab = NULL,
                                 xlab = NULL,
                                 xlim = NULL, 
                                 ylim = NULL,
                                 bty = "n",
                                 col = "black",
                                 lwd = 1,
                                 lty = 1,                           
                                 type = "o",
                                 pch = 16, 
                                 errortype = NULL, 
                                 errorcol = NULL,  
                                 pol0 = NULL, 
                                 fillcol = NULL,
                                 border = NULL, 
                                 legend = NULL) {
  #subset numeric columns
  var <- var[, sapply(var, is.numeric)]

  # check var and age
  if(!is.null(age)){
    if (length(age) != nrow(var)) {
      stop("Length of age is unequal to nrow(var)")
    }
    message("Assuming only variables in dataframe")
  } else { # only var is provided
    # find value that has age
    depthcol <- grep("depth", names(var), ignore.case = TRUE)
    if (length(depthcol) > 1) warning("multiple depth columns found, using first")
    agecol <- grep("age", names(var), ignore.case = TRUE)
    if (length(agecol) > 1) warning("multiple age columns found, using first")
    if (length(depthcol) >= 1 && length(agecol) == 0) {
      age <- var[, depthcol[1]]
      var <- var[, -depthcol[1]]
    } else if (length(depthcol) == 0 && length(agecol) >= 1) {
      age <- var[, agecol[1]]
      var <- var[, -agecol[1]]
    } else if (length(depthcol) > 0 && length(agecol) > 0) {
      # TODO: interactive selection of desired age
      age <- var[, agecol[1]]  # for now we just use age if both are available
      # omit depth and age
      var <- var[, - c(depthcol[1], agecol[1])] 
    } else { # no depthcol, no agecol found: use first column 
      age  <- var[,  1]
      var  <- var[, -1]
      message("Assuming age or depth in first column of var")
    }
    
  }

  # parsing of x- and ylab 
  if (agedir == "h") {
    if (!is.null(ylab)) {
      if (length(ylab) > 1 && length(ylab) != ncol(var)){ 
        stop("Incorrect length of ylab")
      }
      if (class(ylab) == "formula"){
        if (length(ylab == 1)) {
          ylab <- as.expression(ylab)
        } else {
          lapply(ylab, as.expression)
        }
      }
    } else ylab <- names(var)
    if (length(ylab) > 1 && length(ylab) == ncol(var)) {
      ylabs <- ylab
    }
  } else { # agedir = v
    if (!is.null(xlab)) {
      if(length(xlab) > 1 && length(xlab) != ncol(var)){ 
        stop("Incorrect length of xlab")
      }
      if (class(xlab) == "formula"){
        if(length(xlab == 1)) {
          xlab <- as.expression(xlab)
        } else {
          xlabs <- lapply(xlab, as.expression)
        }
      }
    } else xlabs <- names(var)
    if (length(xlab) > 1 && length(xlab) == ncol(var)) {
      xlabs <- xlab
    }
 }

  # specific plotting variables can also be defined for all variables in var
  if (length(type) > 1) {
    if (length(type) == ncol(var)) {
      types <- type
    } else stop("Incorrect length of type")
  } 
  
  if (length(pch) > 1) {
    if (length(pch) == ncol(var)) {
      pchs <- pch
    } else stop("Incorrect length of pch")
  }

  if (length(col) > 1) {
    if (length(col) == ncol(var)) {
      cols <- col
    } else stop("Incorrect length of col")
  }
  
  if (length(lty) > 1) {
    if (length(lty) == ncol(var)) {
      ltys <- lty
    } else stop("Incorrect length of lty")
  }
  
  if (length(lwd) > 1) {
    if (length(lwd) == ncol(var)) {
      lwds <- lwd
    } else stop("Incorrect length of lwd")
  }
  
  if (length(fillcol) > 1) {
    if (length(fillcol) == ncol(var)) {
      filcols <- fillcol
    } else stop("Incorrect length of fillcol")
  }

  # TODO: also do this for bty?, errortype, errorcol, pol0 and border?
 
  # call SuperPlot.numeric, multiple times if necessary
  if (!oneplot && genframe) {
    if (agedir == "h") {
      par(mfrow = c(ncol(var), 1))
    }
  } else {
    par(mfrow = c(1, ncol(var)))
  }
  
  for (i in seq_along(var)) {
    SuperPlot(if (stacked) rowSums(var[, 1:i]) else var[, i], age, agedir, pb,
              add = if (oneplot && i == 1) TRUE else FALSE, error = error,
              xax = xax, yax = yax, mar = mar,
              ylab = if (exists("ylabs")) ylab[i] else ylab,
              xlab = if (exists("xlabs")) xlabs[i] else xlab,
              xlim = if (oneplot && age == "v") range(var, na.rm = TRUE) else xlim,
              ylim = if (oneplot && age == "h") range(var, na.rm = TRUE) else ylim,
              bty = bty, lty = if (exists("ltys")) ltys[i] else lty,
              col = if (exists("cols")) cols[i] else col,
              lwd = if (exists("lwds")) lwds[i] else lwd,
              type = if (exists("types")) types[i] else type,
              pch = if (exists("pchs")) pchs[i] else pch, errortype = errortype,
              errorcol = errorcol, pol0 = pol0, fillcol = fillcol,
              border = border, ...)
  }
  if (!is.null(legend)) {
    legend("topright", legend = legend, col = if(exists("cols")) cols else col,
           pch = pch, lty = if (exists("ltys")) ltys else lty,
           lwd = if (exists("lwds")) lwds else lwd,
           pch = if (exists("pchs")) pchs else pch)
  }
}

SubSetRange <- function(dat, min, max, column = "depth") {
  return(dat[dat[, column] > min && dat[, column] < max, ])
}




