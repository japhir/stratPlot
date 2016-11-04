## Super easy plotting of paleodata in R
## Ilja Kocken
## Student of Marine Sciences at Utrecht University
## First version: 2014-04-11
## Latest version: 2016-10-26

## ## test code
##                                         # a vector
## age <- c(0, 0.2, 0.3, 0.5, 0.8, 4, 13, 20, 40, 50, 90)
## var <- c(1, 4, 5, 100, 400, 1000, 50000, 90000, 1000000, 1000000, 5000000)
## err <- c(1.5, 10, 10, 40, 100, 100, 30000, 90000, 100000, 100000, 0)
## stratPlot(age, var, ylab = pCO[2]~(ppm), ylim = c(1e-1, 1e6),
## 	  pol = T, polcol = "#EEEEEE66", bar = T, barcol = "green",
## 	  error = err,
##           errorcol = "red",          
## 	  abc = "A", abcadj = -0.5,
## 	  xax = c(1, 3),
## 	  xntck = 3,
##           gapsize = 1,
##           log = "y", mar = "auto")
## addGTS(age, 5e-2, 1e0)

## stratPlot(data.frame(var0 = c(1, 4, 5, 100, 400, 1000, 50000, 90000, 1000000, 1000000),
##                      age = seq(100, 1000, length.out = 10),  # automatically extracts age column
##                      var1 = rnorm(10, 40)),
##           log = c("y", ""),
##           pol = c(T, F))

stratPlot <- function(var, ...){
    UseMethod("stratPlot", var)
}

## Creates a plot based on a depth/age vector and a variable vector
stratPlot.numeric <- function(age, var, 
                              ## direction of age "v", "ver", "vertical" or "h" "hor" "horizontal"
                              agedir = "h",
                              ## GTS colour scale on age axis
                              GTS = F, Era = F, Period = T, Epoch = T, Age = F, GTSfrac = .05,
                              ## polygon to plot
                              pol = F, pol0 = NULL, polcol = "#4682B4E6", border = NA, 
                              ## bar to plot
                              bar = F, barcol = "orange", barlwd = 2,
                              add = F,  # logical, add to plot or start new one
                              ## vector of relative errors or matrix/df of absolute values to plot
                              error = NULL, errortype = "bars", # or "area"
                              errorcol = "#BEBEBEE6",
                              errorlwd = 1, errorcode = 3,
                              gapsize = NULL,  # don't draw lines when agediff is larger
                              abc = NULL, abcadj = NULL, # add index letter topleft
                              mar = "inherit",  # or "auto" or specified
                              ##  TODO: add standard Geologic Time Scale to region near x or y axis
                              ...,  # other graphical parameters
                              ## default positions of axes (1:4)
                              xax = if (agedir == "h") 1 else 3, yax = 2,
                              xlim = NULL, ylim = NULL, 
                              xlab = NULL, ylab = NULL,
                              xlabfont = 1, ylabfont = 1,
                              xlabadj = NA, ylabadj = NA,
                              xlabalign = c(0.5, NA), ylabalign = c(0.5, NA),
                              ## xlabpos = NULL, ylabpos = NULL,
                              xlabang = NULL, ylabang = NULL,
                              
                              ## positions of minor tick marks
                              xtck = NULL, ytck = NULL, 
                              ## number of minor tick marks between major marks
                              xntck = 2, yntck = 2,
                              ## default plot/log options
                              las = 1, log = "", bty = "n", type = "o",
                              lty = 1, pch = 16) {
    ##  check var and age
    if (length(var) != length(age)) {
        stop("Unequal length of var and age")
    }

    ## TODO
    ## insert gaps where needed
    if (!is.null(gapsize)) {
        toolarge <- which(diff(age) > gapsize)
        df <- insertEmpty(data.frame(age, var), toolarge)
        age <- df$age
        var <- df$var
    }
    
    ## check agedir
    if (!agedir %in% c("v", "ver", "vertical", "h", "hor", "horizontal")) {
        stop("agedir must be either 'v', 'ver', 'vertical' or 'h', 'hor, 'horizontal'")
    } else {
        agedir <- substr(agedir, 1, 1) # agedir is converted to either 'v' or 'h'
    }
    
    if (!add) {
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
            if (is.null(xlim)) xlim <- range(age, na.rm = TRUE)
            if (is.null(ylim)) ylim <- range(var, na.rm = TRUE)
        } else {
            if (is.null(xlim)) xlim <- range(var, na.rm = TRUE)
            if (is.null(ylim)) ylim <- rev(range(age, na.rm = TRUE))
        }
    
        ##  default xlab, ylab
        if (agedir == "h") {
            if (is.null(xlab)) {
                if (max(age, na.rm = T) < 100) {
                    message("Assuming age is given in Ma")
                    xlab <- "Age (Ma)"
                } else {
                    message("Assuming age is given in ka")
                    xlab <- "Age (ka)"
                }
            }
            if (is.null(ylab)) {
                message("No ylab provided")
                ylab <- ""
            }
        } else {
            if (is.null(xlab)) {
                message("No xlab provided")
                xlab <- ""
            }
            if (is.null(ylab)) {
                if (max(age, na.rm = T) < 100) {
                    message("Assuming age is given in Ma")
                    ylab <- "Age (Ma)"
                } else {
                    message("Assuming age is given in ka")
                    ylab <- "Age (ka)"
                }
            }
        }

        ##  set up plotting margins
        par(mar = mar, bty = bty)
    
        ##  set up empty plot
        plot(1, type = "n", xlim = xlim, ylim = ylim, log = log,
             xlab = "", ylab = "", axes = FALSE, ...)
        ## axes added later
    }
    
    if (pol || bar) {
        ## baseline to draw polygon and bar to
        if (is.null(pol0)) {
            ## logarithmic var axis -> draw polygon to minimum value
            if ((agedir == "h" && grepl("y", log)) ||
                (agedir == "v" && grepl("x", log))) {
                pol0 <- min(var)
            } else  {
                pol0 <- 0
            }
        }
    }
    
    ##  plot polygon
    if (pol) {
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
                col = polcol, border = border) 
    }
    
    ## plot error bars/area
    if (!is.null(error)) {
        if (length(error) > 0) {  # plot errorstuff
            if("bars" %in% errortype) {
                errorBarsPlot(age, var, error, col = errorcol, code = errorcode,
                              agedir = agedir, lwd = errorlwd)
            }
            if ("area" %in% errortype) {
                errorAreaPlot(age, var, error, col = errorcol,
                              agedir = agedir)
            }
        }
    }

    ##  plot bars (added after errors to overlap the possible areas)
    if (bar) {
        if (is.null(pol0)) pol0 <- 0
        segments(x0 = if (agedir == "h") age else rep(pol0, length(var)),
                 y0 = if (agedir == "h") rep(pol0, length(var)) else age,
                 x1 = if (agedir == "h") age else var,
                 y1 = if (agedir == "h") var else age,
                 col = barcol, lwd = barlwd)
    }
    
    ##  plot the actual record
    points(x = if (agedir == "h") age else var,
           y = if (agedir == "h") var else age, type = type,
           pch = pch, lty = lty, ...)

    ## add axes
    if (!add) {
        ## lapply so that I can specify both 1 and 2 for example.
        for (i in xax) {
            addAxis(i, lim = xlim, ntck = xntck, las = las)
            addAxlab(xlab, i, adj = xlabadj, font = xlabfont, ang = xlabang, labadj = xlabalign)
        }
        for (i in yax) {
            addAxis(i, lim = ylim, ntck = yntck, las = las)
            addAxlab(ylab, i, adj = ylabadj, font = ylabfont, ang = ylabang, labadj = ylabalign)
        }
        if (GTS) {
            addGTS(agedir = agedir, Era = F, Period = T, Epoch = T, Age = F, frac = GTSfrac)
        }
    }
    
    ## add corner ABC
    if(!is.null(abc)) {
        if (is.null(abcadj)) abcadj <- 0.04 * abs(diff(par("usr")[1:2])) 
        addABC(abc, abcadj)
    }
}


errorBarsPlot <- function(age, var = NULL, error,  # onesided if single vector!
                          width = diff(range(age)) / 100,
                          col = "#BEBEBEE6", code = 3,
                          lwd = 1,
                          agedir = "h") {
    if (is.data.frame(error) || is.matrix(error) && ncol(error) == 2) {
        if (!(nrow(error) == 1 || nrow(error) == length(var)))
            warning("Number of rows in error not equal to var length, recycling")
        if (agedir == "h")
            arrows(age, error[, 1], age, error[,2], col = col, length = 0.05,
                   angle = 90, code = code, lwd = lwd)
        else if (agedir == "v")
            arrows(error[, 1], age, error[, 2], age, col = col, length = 0.05,
                   angle = 90, code = code, lwd = lwd)
    } else {
        if (!(length(error) == length(var) || length(error) == 1))
            warning("Length of error not equal to var length, recycling")
        if (agedir == "h") {
            arrows(x0 = age, y0 = var - error, x1 = age,
                   y1 = var + error, col = col, length = 0.05,
                   angle = 90, code = code, lwd = lwd)
        }
        else if (agedir == "v") {
            arrows(x0 = var - error, y0 = age, x1 = var + error,
                   y1 = age, col = col, length = 0.05, angle = 90,
                   code = code, lwd = lwd)
        }
    }
}

errorAreaPlot <- function(age, var = NULL, error, 
                          col = "#BEBEBE4D", agedir = "h") {
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
                if(enc$values[i]){              # for non-na regions
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
            y <- c(var - error, rev(var + error))
        }
    }
    polygon(x = if (agedir == "h") x else y, y = if (agedir == "h") y else x,
            col = col, border = NA)
}

##  Takes a dataframe of one or multiple variable(s) to  create a (set of) plot(s)
stratPlot.data.frame <- function(var, # dataframe
                                 age = NULL, # optional vector 
                                 agedir = "h",  # "v", "ver", "vertical" or "h" "hor" "horizontal" 
                                 pol = F, bar = F,        # polygon/bar
                                 gapsize = NULL,  # lines not drawn for timesteps > gapsize
                                 oneplot = F, # logical, if TRUE plot all variables in the same plot
                                 genframe = T, # show plots in same window
                                 add = F, error = NULL,    # vector of errors to plot (note: relative values!)
                                 stacked = F, # logical, calculate cumulative sum for vars, currently doesn't work.
                                 ..., xax = if (agedir == "h") 1 else 3, # default position of x-axis
                                 log = "",
                                 yax = 2,         # default position of yaxis
                                 las = 1,         # default direction of axis labels
                                 mar = "auto",    # generated based on xax and yax
                                 ylab = NULL, xlab = NULL, xlim = NULL,
                                 ylim = NULL,
                                 xntck = 2, yntck = 2,
                                 bty = "n", col = "black",
                                 lwd = 1, lty = 1, type = "o", pch = 16,
                                 errortype = "bars", errorcol = "#BEBEBEE6",
                                 pol0 = NULL, polcol = "#4682BEE6", border = NULL,
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
        ##  find column that has age/depth
        depthcol <- grep("depth", names(var), ignore.case = TRUE)
        if (length(depthcol) > 1) warning("multiple depth columns found, using first")
        agecol <- grep("age|time", names(var), ignore.case = TRUE)
        if (length(agecol) > 1) warning("multiple age columns found, using first")
        ## depthcol found but agecol isn't
        if (length(depthcol) >= 1 && length(agecol) == 0) {
            ## extract age/depth from var
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
            ## extract age/depth from var
            age <- var[, agecol[1]]
            var <- var[, -agecol[1]]
        ## both agecol and depthcol found, using agecol
        } else if (length(depthcol) > 0 && length(agecol) > 0) {
            ##  TODO: interactive selection of desired age
            ## extract age/depth from var
            age <- var[, agecol[1]]  # for now we just use age if both are available
            ##  omit depth and age
            var <- var[, - c(depthcol[1], agecol[1])] 
        } else { # no depthcol, no agecol found: use first column
            ## extract age/depth from var
            age  <- var[,  1]
            var  <- var[, -1]
            message("Assuming age or depth in first column of var")
        }
        
    }

    ##  number of variables after extraction of age
    if (is.vector(var)) nvar <- 1 else nvar <- ncol(var)

    ##  parsing of x- and ylab 
    if (agedir == "h") {
        if (!is.null(ylab)) {
            ## TODO: parse ylab in similar manner as xlab, with proper expression check
            if (is.character(ylab) && length(ylab) > 1 && length(ylab) != nvar){ 
                stop("Incorrect length of ylab")
            }
            if (class(ylab) == "formula"){
                if (length(ylab == 1)) {
                    ylab <- as.expression(ylab)
                } else {
                    lapply(ylab, as.expression)
                }
            }
        } else {
            message("Using varnames as ylabs")
            ylab <- names(var)
        }
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
        } else {
            message("Using varnames as xlabs")
            xlabs <- names(var)
        }
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
    } else cols <- NULL
    
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

    if (length(pol) > 1) {
        if (length(pol) == nvar) {
            pols <- pol
        } else stop("Incorrect length of pol")
    }

    if (length(polcol) > 1) {
        if (length(polcol) == nvar) {
            polcols <- polcol
        } else stop("Incorrect length of polcol")
    }

    if (length(bar) > 1) {
        if (length(bar) == nvar) {
            bars <- bar
        } else stop("Incorrect length of bar")
    }

    if (is.list(ylim)){
        if (length(ylim) == nvar) {
            ylims <- ylim
        } else stop("Incorrect length of ylim")
    }
    ##  TODO: also do this for bty?, errortype, errorcol, pol0 and border?
    if (length(log) > 1) {
        if (length(log) == nvar) {
            logs <- log
        } else stop("Incorrect length of log")
    }
   
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
    for (i in seq_len(nvar)) {
        stratPlot(if (nvar == 1) {
                      var
                  } else if (stacked) {
                      if (i == 1) var[, 1]
                      else rowSums(var[, 1:i])
                  } else { var[, i] },
                  age = age, agedir = agedir, gapsize = gapsize,
                  pol = if (exists("pols")) pols[i] else pol,
                  ## pol = pol[i],
                  bar = if (exists("bars")) bars[i] else bar,
                  add = if (is.null(add)) { if ((oneplot || stacked) && i != 1) TRUE else FALSE } else { add },
                  error = error, xax = xax, yax = yax, mar = mar, ## TODO: change exists check for something else b/c it currently uses the global space
                  ylab = if (exists("ylabs")) ylabs[i] else ylab,
                  xlab = if (exists("xlabs")) xlabs[i] else xlab,
                  xntck = xntck, yntck = yntck,
                  xlim = if (oneplot && age == "v") range(var, na.rm = TRUE) else xlim,
                  ylim = if (oneplot && age == "h") range(var, na.rm = TRUE) else if (exists("ylims")) ylims[[i]] else ylim,
                  bty = bty, lty = if (exists("ltys")) ltys[i] else lty,
                  col = if (!is.null(cols)) cols[i] else col,
                  lwd = if (exists("lwds")) lwds[i] else lwd,
                  type = if (exists("types")) types[i] else type,
                  pch = if (exists("pchs")) pchs[i] else pch,
                  log = if (exists("logs")) logs[i] else log,
                  errortype = errortype, errorcol = errorcol, pol0 = pol0,
                  polcol = if (exists("polcols")) polcols[i] else polcol, border = border, ...)
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

addAxis <- function(ax = 1, lim = NULL, ntck = NULL, tck = NULL, las = 1, ...) {
    if (is.null(ntck)) ntck <- 2
    if (ax %in% c(1, 3)) {  # bottom or top x-axis
        log <- par("xlog")
        if(is.null(lim) && log) {
            lim <- c(1e-100, 1e100)  # just use a mega range...
        }
    } else if (ax %in% c(2, 4)) {  # left or right y-axis
        log <- par("ylog")
        if (is.null(lim) && log) {
            lim <- c(1e-100, 1e100)
        }
    } else stop("ax must be 1: bottom, 2: left, 3: top or 4: right")
    
    ## if the axis is on a logarithmic scale
    if (log) {
        ## the log10 range of powers that need something done
        pow <- c(
            if (lim[1] == 0) -100
            else if (lim[1] > 0) floor(log10(lim[1]))
                else - floor(log10(-lim[1])),
                if (lim[2] == 0) -100
                else if (lim[2] > 0) ceiling(log10(lim[2]))
                else -ceiling(log10(-lim[2]))) 
        if (is.null(tck)) 
            tck <- c(1:10 %o% 10^((pow)[1]:(pow)[2]))
        ## log axis with nicer 10^pow labels for all the ax
        axis(ax, at = 10^(pow[1]:pow[2]),
             labels = sapply((pow)[1]:(pow)[2], # or -xpow[1]?
                             function(i) {
                                 as.expression(bquote(10^ .(i)))}),
             las = las, ...)
    } else {  # non-log axis
        ## default axis
        axis(ax, las = las, ...)
        ## find stepsize used in default axis
        if (is.null(tck)) 
            stepsize <- abs(diff(axTicks(ax)[1:2])) / ntck
            ## add ntck ticks between
            tck <- seq(from = min(axTicks(ax)) - stepsize * ntck,
                       to = max(axTicks(ax)) + stepsize * ntck,
                       by = stepsize)
    }
    ## minor x-axis tick marks
    axis(ax, at = tck, labels = FALSE, tcl = -.2)
}

## if (grepl("x", log) || las == 1) 3
                                             ## else 2

## add axis labels
addAxlab <- function (lab = "", side = 1, line = 2, adj = NA, labadj = c(0.5, NA),
                      ang = NULL, pos = NULL, font = 1, x = NULL, y = NULL, ...) {
    ## add axis labels
    ## no angle, and no specific x and y position for the label just use mtext
    if (is.null(ang) || (!is.null(x) && !is.null(y))) {
        mtext(lab, side = side, line = line, adj = adj, pos = pos, font = font, ...)
    } else {
        if (side %in% c(1, 3)) {  # default horizontal axis:
            ## parse x from adj
            if (is.null(x)) {
                if (is.na(adj)) {
                    x <- mean(par("usr")[1:2]) 
                } else {
                    x <- par("usr")[1] + diff(par("usr")[1:2]) * adj
                }
            }
            ## parse y from line
            if (is.null(y)) {
                if (side == 1) {
                    y <- par("usr")[3] - diff(par("usr")[3:4])/28 * line
                } else if (side == 3) {
                    y <- par("usr")[4] + diff(par("usr")[3:4])/28 * line
                }
            }
        } else if (side %in% c(2, 4)) {  # vertical axis 
            ## parse y from adj
            if (is.null(y)) {
                if (is.na(adj)) {
                    y <- mean(par("usr")[3:4]) 
                } else {
                    y <- par("usr")[3] + diff(par("usr")[3:4]) * adj
                }
            }
            ## parse x from line
            if (is.null(x)) {
                if (side == 2) {
                    x <- par("usr")[1] - diff(par("usr")[1:2])/28 * line/2
                } else if (side == 4) {
                    x <- par("usr")[2] + diff(par("usr")[1:2])/28 * line/2
                }
            }
        }
        text(x, y, labels = lab, font = font, srt = ang, pos = pos, xpd = TRUE, adj = labadj, ...)
    }
}

## add large a/b/c in top left corner of current plot
addABC <- function(char = "A", xadj = 0.04 * abs(diff(par("usr")[1:2])),
                    yadj = 0.04 * abs(diff(par("usr")[3:4])), cex = 2) {
    ## TODO: also work for log axes
    xpos <- par("usr")[1] + xadj
    ypos <- par("usr")[4] + yadj
    mtext(char, 3, at = xpos, cex = cex)
    ## text(xpos, ypos, char, cex = cex, xpd = NA)
}

addGTS <- function(age, xleft = NULL, xright = NULL, frac = 0.05,
                   agelim = range(age), agedir = "h",
                   type = NULL,
                   Eon = T, Era = T, Period = T, Epoch = T, Age = T,
                   Chron = T,
                   hort = NULL, relwidth = NULL, cex.text = NULL,
                   verbose = T) {
    ## check types
    if (is.null(type)) type <- c(Eon, Era, Period, Epoch, Age, Chron)
    ## subset types to plot
    alltypes = c("Eon", "Era", "Period", "Epoch", "Age", "Chron")
    types <- alltypes[type]
    if (verbose) cat("\nplotting types:", types)

    ## read GTS table with color and age info
    ## todo: make this sharable
    if (sum(types %in% alltypes[1:5]) > 0) {
        GTS <- read.csv("~/Dropbox/DepthPlotter/GTS_colours.csv", stringsAsFactors = F)
        GTS$hex <- rgb(GTS$R, GTS$G, GTS$B, maxColorValue = 255)
        GTS$mean <- (GTS$end - GTS$start) / 2 + GTS$start
        ## order the type factor
        GTS$type <- factor(GTS$type, alltypes[1:5], ordered = T)
    }

    ## read chron table
    if (sum(types %in% alltypes[6]) > 0) {
        Chron <- read.csv("~/Dropbox/DepthPlotter/Chronages.csv", stringsAsFactors = F)
        Chron$name <- Chron$Pol
        Chron$hex <- "#000000"
        Chron$hex[grepl("r", Chron$Pol)] <- "#FFFFFF"
        Chron$start <- c(22, Chron$GTS2012[-nrow(Chron)])
        Chron$end <- Chron$GTS2012
        Chron$mean <- (Chron$end - Chron$start) / 2 + Chron$start
    }

    ## default xleft/xright
    if (is.null(xleft)) {
        if (agedir == "h") {
            xleft  <- par("usr")[3]
        } else {
            xleft <- par("usr")[1]
        }
    }
    if (verbose) cat(" from ", xleft)
    if (is.null(xright)) {
        if (agedir == "h") {
            fullright <- par("usr")[4]
        } else {
            fullright <- par("usr")[2]
        }
        xright <- xleft + frac * (diff(c(xleft, fullright)))
    }
    if (verbose) cat(" to ", xright, "\n")

    if (is.null(relwidth)) {
        ## default relwidths
        relwidth <- c(0.08, 0.08, 0.1, 0.3, 0.44, 0.44)
        ## subset/make relative to selected types
        relwidth <- relwidth[type] / sum(relwidth[type])
    }

    if (is.null(hort)) {
        if (agedir == "h") hort <- c(F, F, F, F, F, F)[type]
        else hort <- c(F, F, F, T, T, T)[type]
    }

    if (is.null(cex.text)) {
        cex.text <- c(1, 1, 1, .8, .7, 0.7)[type]
    }

    ## this subfunction plots one gts bar
    plotGTS <- function(xleft, xright, gts, ad, td = T, cex.t, textpos = NULL) {
        ## add rectangles with appropriate colour
        rect(if (ad == "h") gts$start else xleft,
             if (ad == "h") xleft else gts$start,
             if (ad == "h") gts$end else xright,
             if (ad == "h") xright else gts$end,
             col = gts$hex, border = darker(gts$hex))
        ## add name in centre
        ## TODO: if the centre is outside of the plot margins, add it to
        ## the closest plot area
        ## TODO: xright if Chron
        text(if (ad == "h") gts$mean else mean(c(xleft, xright)),
             if (ad == "h") mean(c(xleft, xright)) else gts$mean,
             labels = gts$name,
             srt = if (td) 0 else 90,
             cex = cex.t, pos = textpos)
    }

    nbars <- sum(type)
    totwidth <- diff(c(xleft, xright))
    ## loop over type, so one loop is one bar of age info
    for (bar in seq_len(nbars)) {
        xright <- xleft + relwidth[bar] * totwidth
        plotGTS(xleft, xright,
                if (types[bar] != "Chron") GTS[GTS$type == types[bar], ]
                else Chron,
                ad = agedir, td = hort[bar], cex.t = cex.text[bar],
                textpos = if (types[bar] != "Chron") NULL else NULL)
                ## TODO: fix text position for chron to right of xright with pos = 4.
        xleft <- xright
    }
}

## TODO: create addChron function that does the same as addGTS but with
## normal/reversed magnetostrat + names

darker <- function(color = col, factor=1.4){
    col <- col2rgb(color)
    col <- col/factor
    col <- rgb(t(col), maxColorValue=255)
    col
}

lighter <- function(color = col, factor=1.4){
    col <- col2rgb(color)
    col <- col*factor
    col <- rgb(t(as.matrix(apply(col, 1, function(x) if (x > 255) 255 else x))), maxColorValue=255)
    col
}

## adds a linear model with confidence levels to the plot
plotLM <- function(x = NULL, y = NULL, confidence = 0.90,
                   col = "red", lty = 2, lwd = 1) {
    ## linmod = NULL, 
    ## if (is.null(linmod) && (is.null(x) && is.null(y))) stop("Specify x and y or linmod")
    ## if (is.null(linmod)) linmod <- lm(y ~ x)
    linmod <- lm(y ~ x)
    ## TODO: doesn't work with only linmod because of variable names in newdata
    ## plot(a,y,xlim=c(20,90),ylim=c(0,80))
    abline(linmod, col="red")
    newx <- seq(par("usr")[1], par("usr")[2], length.out = 100)
    prd<-predict(linmod,
                 newdata = data.frame(x = newx),
                 interval = c("confidence"), 
                 level = confidence, type="response")
    lines(newx,prd[,2],col = col, lty = lty, lwd = lwd)
    lines(newx,prd[,3],col = col, lty = lty, lwd = lwd)
}

## plots the R2 value of a linear model in a corner
plotLMinfo <- function(linmod, pos = "topleft", digits = 4) {
    lgd <- bquote(R^2~"="~.(format(summary(linmod)$adj.r.squared,
                                           digits = digits)))
    legend(pos, bty="n", legend = lgd)
}
