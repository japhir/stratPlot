## Super easy plotting of paleodata in R
## Ilja Kocken
## Student of Marine Sciences at Utrecht University
## First version: 2014-04-11
## Latest version: 2016-06-27

stratPlot <- function(var, ...){
    UseMethod("stratPlot", var)
}

## Creates a plot based on a depth/age vector and a variable vector
stratPlot.numeric <- function(var,           # numeric vector
                              age = NULL,    # optional numeric vector 
                              agedir = "h",  # "v", "ver", "vertical" or "h" "hor" "horizontal" 
                              pb = "n",      # polygon/bar
                              ##  gapmaker = NULL, # TODO
                              oneplot = FALSE, # logical, if TRUE plot all variables in the same plot
                              add = FALSE,     # logical, add to plot or start new one
                              error = NULL,    # vector of errors to plot (note: relative values!)
                              stacked = FALSE, # logical, calculate cumulative sum for vars
                              xax = if (agedir == "h") 1 else 3, # default position of x-axis
                              yax = 2,         # default position of yaxis
                              mar = "auto",    # generated based on xax and yax or inherited
                              ##  TODO: add standard Geologic Time Scale to region near x or y axis
                              ..., ylab = NULL, xlab = NULL, xlim = NULL,
                              log = "", xntck = 10, yntck = 10, xtck = NULL,
                              ytck = NULL, ylim = NULL, bty = "n", type = "o",
                              pch = 16, errortype = NULL, errorcol = NULL,
                              pol0 = NULL, barscol = NULL, fillcol = NULL,
                              border = NULL, legend = NULL) {
    ##  check var and age
    if (length(var) != length(age)) {
        stop("Unequal length of var and age")
    }

    ## TODO
    ## check gapmaker with vectors?
    ## if (!is.null(gapmaker)) {
    ##  gapMaker()
    ## }
    
    ## check agedir
    if (!agedir %in% c("v", "ver", "vertical", "h", "hor", "horizontal")) {
        stop("agedir must be either 'v', 'ver', 'vertical' or 'h', 'hor, 'horizontal'")
    } else {
        agedir <- substr(agedir, 1, 1) # agedir is now either 'v' or 'h'
    }
    
    ##  check pb
    if (!pb %in% c("n", "PB", "BP", "P", "B")) {
        stop("Invalid pb, choose 'n' none, 'PB' polygon bar, 'P' polygon or 'B' bar")
    }

    ## check error
    if (!is.null(error)) {
        ##  check errortype
        if (!is.null(errortype)) {
            if (!errortype %in% c("bars", "area")) {
                stop("Invalid errortype, choose 'bars' or 'area'") 
            }
        } else errortype <- "bars"  
        ##  check errorcol
        if (!is.null(errorcol)) {
            if (!errorcol %in% colors()) {
                stop("Invalid errorcol")
            }
        } else errorcol <- adjustcolor("gray", .9)  
    }

    ##  mar (depends on xax and yax, which are not checked)
    if (identical(mar, "auto")) {
        mar <- c(if (1 %in% xax || 1 %in% yax) 5 else 2,
                 if (2 %in% xax || 2 %in% yax) 5 else 2,
                 if (3 %in% xax || 3 %in% yax) 5 else 2,
                 if (4 %in% xax || 4 %in% yax) 5 else 2) + .1
    } else if (identical(mar, "inherit")) {
        mar <- par(no.readonly = TRUE)$mar
    }
    
    ##  xlim, ylim
    if (agedir == "h") {
        if (is.null(xlim)) xlim <- rev(range(age, na.rm = TRUE))
        if (is.null(ylim)) ylim <- range(var, na.rm = TRUE)
    } else {
        if (is.null(xlim)) xlim <- range(var, na.rm = TRUE)
        if (is.null(ylim)) ylim <- rev(range(age, na.rm = TRUE))
    }
    
    ##  default xlab, ylab
    if (agedir == "h") {
        if (is.null(xlab)) xlab <- "Age (Ma)"
        if (is.null(ylab)) ylab <- ""
    } else {
        if (is.null(xlab)) xlab <- ""
        if (is.null(ylab)) ylab <- "Age (Ma)"
    }

    ##  set up plotting margins
    par(mar = mar, bty = bty)
    
    ##  create empty plot
    if (!add) {
        ##  set up blank plotting area
        plot(c(1,1), c(1,1), type = "n", xlim = xlim, ylim = ylim, log = log,
             xlab = "", ylab = "", axes = FALSE)

        ##  default axis with values
        lapply(xax, axis)
        lapply(yax, axis)
        ##  add axis labels
        lapply(xax, function(i) { mtext(xlab, side = i, line = 2) })
        lapply(yax, function(i) { mtext(ylab, side = i, line = 2) })
    }
    
    ##  set plotting variables for PB
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
    
    ##  plot PB polygon
    if (area) {
        ## baseline to draw polygon to
        if (is.null(pol0)) {
            ## logarithmic var axis -> draw polygon to minimum value
            if ((agedir == "h" && grepl("y", log)) ||
                (agedir == "v" && grepl("v", log))) {
                pol0 <- min(var)
            } else  {
                pol0 <- 0
            }
        }
        if (is.null(border)) border <- NA
        if (is.null(fillcol)) fillcol <- adjustcolor("steelblue", .8)
        if (anyNA(var) || anyNA(age)) {
            message("NAs found in var/age, currently ignoring")
            nona <- data.frame(var = var, age = age)
            nona <- nona[complete.cases(nona), ]
            x <- c(nona$age[1], nona$age, tail(nona$age, n = 1))
            y <- c(pol0, nona$var, pol0)
            ##  TODO: create polygons per non-NA region, like we do with errorregion
            ##  enc <- rle(!is.na(var))
            ##  endIdxs <- cumsum(enc$lengths)
            ##  for (i in 1:length(enc$lengths)) {
            ##    if (enc$values[i]) {
            ##      endIdx <- endIdxs[i]
            ##      startIdx <- endIdx - enc$lengths[i] + 1
            ##      subvar <- var[startIdx:endIdx]
            ##      subage <- age[startIdx:endIdx]
            ##      x <- c(left, subvar, left)
            ##      y <- c(subage[1], age, tail(age, n = 1))
            ##    }
            ##  }
        } else {
            ##  not sure if I want to use the current left or use:
            ##  min(var, na.rm = TRUE) - .04 * diff(range(var, na.rm = TRUE))
            x <- c(age[1], age, tail(age, n = 1))
            y <- c(pol0, var, pol0)
        }
        polygon(x = if (agedir == "h") x else y, y = if (agedir == "h") y else x,
                col = fillcol, border = border) # no extra options!
    }
    
    if (length(error) > 0) {  # plot errorstuff
        if(errortype == "bars") {
            errorBarsPlot(var, age, error, col = errorcol, agedir = agedir)
        } else if (errortype == "area")
            errorAreaPlot(var, age, error, col = errorcol, agedir = agedir)
    }

    ##  plot PB bars (added after errors to overlap the possible areas)
    if (bars) {
        if (is.null(pol0)) pol0 <- 0
        if (is.null(barscol)) barscol <- "steelblue"
        segments(x0 = if (agedir == "h") age else rep(pol0, length(var)),
                 y0 = if (agedir == "h") rep(pol0, length(var)) else age,
                 x1 = if (agedir == "h") age else var,
                 y1 = if (agedir == "h") var else age, col = barscol) # currenlty no extra options!
        ##  TODO add barcol, barlwd etc?
    }
    
    ##  plot the actual record
    points(x = if (agedir == "h") age else var,
           y = if (agedir == "h") var else age, type = type,
           pch = pch, ...)

    ##  add minor ticks later (to be plotted over polygon)
    if (!add) {
        ## default x-axis minor tick marks
        if (is.null(xtck)) {
            if (grepl("x", log)) {
                xpow <- c(
                    if (xlim[1] == 0) -100
                    else if (xlim[1] > 0) floor(log10(xlim[1]))
                    else - floor(log10(-xlim[1])),
                    if (xlim[2] == 0) -100
                    else if (xlim[2] > 0) ceiling(log10(xlim[2]))
                    else -ceiling(log10(-xlim[2]))) 
                xtck <- c(1:10 %o% 10^((xpow)[1]:(xpow)[2]))
            } else {  # non-log x-axis
                ## find stepsize used in default axis
                stepsize <- abs(diff(axTicks(1)[1:2])) / xntck
                ## add xntck ticks between
                xtck <- seq(from = min(axTicks(1)) - stepsize * xntck,
                            to = max(axTicks(1)) + stepsize * xntck,
                            by = stepsize)
            }
        }

        ##  default y-axis minor tick marks
        if (is.null(ytck)) {
            if (grepl("y", log)) {
                ypow <- c(
                    if (ylim[1] == 0) -100
                    else if (ylim[1] > 0) floor(log10(ylim[1]))
                    else - floor(log10(-ylim[1])),
                    if (ylim[2] == 0) -100
                    else if (ylim[2] > 0) ceiling(log10(ylim[2]))
                    else -ceiling(log10(-ylim[2]))) 
                ytck <- c(1:10 %o% 10^((ypow[1]-1):(ypow[2]-1)))
            } else {
                stepsize <- abs(diff(axTicks(2)[1:2])) / yntck
                ytck <- seq(from = min(axTicks(2)) - stepsize * yntck,
                            to = max(axTicks(2)) + stepsize * yntck,
                            by = stepsize)
            }
        }
        ##  minor tick axis
        lapply(xax, axis, at = xtck, labels = FALSE, tcl = .3)
        lapply(yax, axis, at = ytck, labels = FALSE, tcl = .3)
    }
    
    ##  plot a legend of one variable... is this necessary?
    if(!is.null(legend))
        legend("topright", legend = legend, bty = bty, lty = lty, 
               pch = pch, ...)
}


errorBarsPlot <- function(var = NULL, age, error, 
                          width = diff(range(age)) / 100,
                          col = adjustcolor("gray", alpha = 0.9),
                          agedir = "h") {
    if (is.data.frame(error) || is.matrix(error) && ncol(error) == 2) {
        if (!(nrow(error) == 1 || nrow(error) == length(var)))
            warning("Number of rows in error not equal to var length, recycling")
        if (agedir == "h")
            arrows(age, error[, 1], age, error[,2], col = col, length = 0.05,
                   angle = 90, code = 3)
        else if (agedir == "v")
            arrows(error[, 1], age, error[, 2], age, col = col, length = 0.05,
                   angle = 90, code = 3)
    } else {
        if (!(length(error) == length(var) || length(error) == 1))
            warning("Length of error not equal to var length, recycling")
        if (agedir == "h") {
            arrows(x0 = age, y0 = var - 0.5 * error, x1 = age,
                   y1 = var + 0.5 * error, col = col, length = 0.05,
                   angle = 90, code = 3)
        }
        else if (agedir == "v") {
            arrows(x0 = var - 0.5 * error, y0 = age, x1 = var + 0.5 * error,
                   y1 = age, col = col, length = 0.05, angle = 90, code = 3)
        }
    }
}

errorAreaPlot <- function(var = NULL, age, error, 
                          col = adjustcolor("gray", .3), agedir = "h") {
    if (!is.numeric(error) && is.null(var))
        stop("Provide either dataframe/matrix of low and high error values or vector of relative error values.")
    ##  error is a dataframe/matrix with 2 columns for negative and positive
    ##  absolute error values
    if (is.data.frame(error) || is.matrix(error) && ncol(error) == 2) {
        ##  TODO: support for NA values in age when specifying strict errorvalues
        if (nrow(error) != length(var))
            warning("Number of rows in error not equal to var length, recycling")
        x <- c(age, rev(age))
        y <- c(error[,1], error[,2])
    } else {
        if (anyNA(var) | anyNA(age)) {
            enc <- rle(!is.na(var))             # calculate amount of non-NA polygons
            endIdxs <- cumsum(enc$lengths)      # lengths of polygons
            for (i in seq_along(enc$lengths)){  # for each polygon
                if(enc$values[i]){                # for non-na regions
                    endIdx <- endIdxs[i]
                    startIdx <- endIdx - enc$lengths[i] + 1
                    
                    subdat <- var[startIdx:endIdx]
                    subsd <- error[startIdx:endIdx]
                    subage <- age[startIdx:endIdx]
                    
                    x <- c(subdat - subsd, rev(subdat + subsd))
                    y <- c(subage, rev(subage))
                }
            }
        } else {
            x <- c(age, rev(age))
            y <- c(var - .5 * error, rev(var + .5 * error))
        }
    }
    polygon(x = if (agedir == "h") x else y, y = if (agedir == "h") y else x,
            col = col, border = NA)
}

##  Takes a dataframe of one or multiple variable(s) to  create a (set of) plot(s)
stratPlot.data.frame <- function(var, # dataframe
                                 age = NULL, # optional vector 
                                 agedir = "h",  # "v", "ver", "vertical" or "h" "hor" "horizontal" 
                                 pb = "n",        # polygon/bar
                                 gapsize = NULL,  # lines not drawn for timesteps > gapsize
                                 oneplot = FALSE, # logical, if TRUE plot all variables in the same plot
                                 genframe = TRUE, # show plots in same window
                                 add = FALSE, error = NULL,    # vector of errors to plot (note: relative values!)
                                 stacked = FALSE, # logical, calculate cumulative sum for vars
                                 ..., xax = if (agedir == "h") 1 else 3, # default position of x-axis
                                 yax = 2,         # default position of yaxis
                                 mar = "auto",    # generated based on xax and yax
                                 ylab = NULL, xlab = NULL, xlim = NULL,
                                 ylim = NULL, bty = "n", col = "black",
                                 lwd = 1, lty = 1, type = "o", pch = 16,
                                 errortype = NULL, errorcol = NULL,
                                 pol0 = NULL, fillcol = NULL, border = NULL,
                                 legend = NULL) {
    ## subset numeric columns
    ## var <- var[, sapply(var, is.numeric)]
    ##  TODO: do this as option?

    ##  check var and age
    if (!is.null(age)){
        if (length(age) != nrow(var)) {
            stop("Length of age is unequal to nrow(var)")
        }
        ##  TODO add gapmaker support!
        message("Assuming only variables in dataframe")
    } else { # only var is provided
        ##  find column that has age
        depthcol <- grep("depth", names(var), ignore.case = TRUE)
        if (length(depthcol) > 1) warning("multiple depth columns found, using first")
        agecol <- grep("age", names(var), ignore.case = TRUE)
        if (length(agecol) > 1) warning("multiple age columns found, using first")
        ## depthcol found but agecol isn't
        if (length(depthcol) >= 1 && length(agecol) == 0) {
            ##  check gapsize
            if (!is.null(gapsize)) {
                var <- gapMaker(var, gapsize = gapsize, varname = depthcol[1])
            }
            age <- var[, depthcol[1]]
            var <- var[, -depthcol[1]]

            ##  overwrite agelab if default
            if (agedir == "h") {
                if (is.null(xlab)) xlab <- "Depth (mbsf)"
            } else if (agedir == "v") {
                if (is.null(ylab)) ylab <- "Depth (mbsf)"
            }
        ## agecol found but depthcol isn't
        } else if (length(depthcol) == 0 && length(agecol) >= 1) {
            ##  check gapsize
            if (!is.null(gapsize)) {
                var <- gapMaker(var, gapsize = gapsize, varname = agecol[1])
            }
            age <- var[, agecol[1]]
            var <- var[, -agecol[1]]
        ## both agecol and depthcol found, using agecol
        } else if (length(depthcol) > 0 && length(agecol) > 0) {
            ##  TODO: interactive selection of desired age
            ##  check gapsize
            if (!is.null(gapsize)) {
                var <- gapMaker(var, gapsize = gapsize, varname = agecol[1])
            }
            age <- var[, agecol[1]]  # for now we just use age if both are available
            ##  omit depth and age
            var <- var[, - c(depthcol[1], agecol[1])] 
        } else { # no depthcol, no agecol found: use first column
            ##  check gapsize
            if (!is.null(gapsize)) {
                var <- gapMaker(var, gapsize = gapsize, varname = agecol[1])
            }
            age  <- var[,  1]
            var  <- var[, -1]
            message("Assuming age or depth in first column of var")
        }
        
    }

    ##  number of variables
    if (is.vector(var)) nvar <- 1 else nvar <- ncol(var)

    ##  parsing of x- and ylab 
    if (agedir == "h") {
        if (!is.null(ylab)) {
            if (length(ylab) > 1 && length(ylab) != nvar){ 
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
        if (length(ylab) > 1 && length(ylab) == nvar) {
            ylabs <- ylab
        }
    } else { # agedir = v
        if (!is.null(xlab)) {
            if(length(xlab) > 1 && length(xlab) != nvar){ 
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
        if (length(xlab) > 1 && length(xlab) == nvar) {
            xlabs <- xlab
        }
    }

    ##  specific plotting variables can also be defined for all variables in var
    if (length(type) > 1) {
        if (length(type) == nvar) {
            types <- type
        } else stop("Incorrect length of type")
    } 
    
    if (length(pch) > 1) {
        if (length(pch) == nvar) {
            pchs <- pch
        } else stop("Incorrect length of pch")
    }

    if (length(col) > 1) {
        if (length(col) == nvar) {
            cols <- col
        } else stop("Incorrect length of col")
    }
    
    if (length(lty) > 1) {
        if (length(lty) == nvar) {
            ltys <- lty
        } else stop("Incorrect length of lty")
    }
    
    if (length(lwd) > 1) {
        if (length(lwd) == nvar) {
            lwds <- lwd
        } else stop("Incorrect length of lwd")
    }
    
    if (length(fillcol) > 1) {
        if (length(fillcol) == nvar) {
            filcols <- fillcol
        } else stop("Incorrect length of fillcol")
    }

    if (is.list(ylim)){
        if (length(ylim) == nvar) {
            ylims <- ylim
        } else stop("Incorrect length of ylim")
    }
    ##  TODO: also do this for bty?, errortype, errorcol, pol0 and border?
    
    ##  set up the plotting region
    ##  last is to check if it wasn't a df with only 2 columns
    if (!oneplot && genframe && is.data.frame(var)) {  
        if (agedir == "h") {
            par(mfrow = c(nvar, 1))
        } else {
            par(mfrow = c(1, nvar))
        }
    }
    
    ##  call stratPlot.numeric, multiple times if necessary
    for (i in nvar) {
        stratPlot(if (nvar == 1) var
                  else if (stacked) rowSums(var[, 1:i])
                  else var[, i],
                  age, agedir, pb,
                  add = if (oneplot && i != 1) TRUE else FALSE, error = error,
                  xax = xax, yax = yax, mar = mar,
                  ## TODO: change exists check for something else b/c it currently
                  ## uses the global space
                  ylab = if (exists("ylabs")) ylab[i] else ylab,
                  xlab = if (exists("xlabs")) xlabs[i] else xlab,
                  xlim = if (oneplot && age == "v") range(var, na.rm = TRUE)
                         else xlim,
                  ylim = if (oneplot && age == "h") range(var, na.rm = TRUE)
                         else if (exists("ylims")) ylims[[i]] else ylim,
                  bty = bty, lty = if (exists("ltys")) ltys[i] else lty,
                  col = if (exists("cols")) cols[i] else col,
                  lwd = if (exists("lwds")) lwds[i] else lwd,
                  type = if (exists("types")) types[i] else type,
                  pch = if (exists("pchs")) pchs[i] else pch,
                  errortype = errortype, errorcol = errorcol, pol0 = pol0,
                  fillcol = fillcol, border = border, ...)
    }
    if (!is.null(legend)) {
        legend("topright", legend = legend, col = if(exists("cols")) cols else col,
               lty = if (exists("ltys")) ltys else lty,
               lwd = if (exists("lwds")) lwds else lwd,
               pch = if (exists("pchs")) pchs else pch)
    }
}

stratPlot.list <- function(ls, ...) {
    lapply(ls, stratPlot, ...)
}

##  subset dataframe to range of age/depth
subsetRange <- function(dat, min, max, column = "depth") {
    return(dat[dat[, column] > min & dat[, column] < max, ])
}

##  insert empty rows between values that differ more than gapsize
gapMaker <- function(df, gapsize = .5, varname = "age") {
    toolarge <- which(diff(df[, varname]) > gapsize)
    df <- insertEmpty(df, toolarge)
    return(df)
}

##  insert empty rows in df after afterrow
insertEmpty <- function(df, afterrow) {
    ##  create indices for the data order
    df$id <- seq_len(nrow(df))
    df[max(df$id) + seq_along(afterrow), ] <- NA
    df$id[is.na(df$id)] <- afterrow + .5
    df <- df[order(df$id), ]
    df <- df[ , !names(df) %in% "id"]
    return(df)
}




