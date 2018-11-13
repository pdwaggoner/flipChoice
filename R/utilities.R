#' Utilities
#'
#' Create plot showing mean utility or return matrix of per-respondent values
#'
#' @param x A \code{FitChoice} object, a matrix of numeric values, or a list containing the mean utilities 
#'      for each attribute level (e.g. \code{list(Attribute1 = c(Level1 = 0.2, Level2 = 0.4, Level3 = 0.3), 
#'      Attribute2 = c(Level1 = 0.2, Level2 = 0.1))}).
#' @param output For "Line" (default) or "Column, "Bar" a plot showing mean (rescaled)
#'      utilities for each attribute level will be shown. For "Data", a matrix
#'      containing the rescaled respondent parameters will be returned
#' @param scaling Controls if the mean utility is scaled. One of "As is" (no scaling),
#'  "Min = 0", or "Mean = 0".
#' @param attr.order Controls the order in which the attribute are shown.
#'  One of "As is", Increasing", or "Decreasing" (sorting by range).
#' @param levels.order Controls the order in which the levels of each attribute are shown.
#'  One of "As is", "Reverse", Increasing", or "Decreasing"
#' @param attr.levels.not.ordered A comma-separated list of the attributes which should not be ordered.
#' @param weights An optional vector of sampling weights which should be the
#'  same length as the number of respondents in \code{x}.
#' @param subset An optional vector specifying a subset of respondents used to take
#'  the mean utility. It should be the same length as the number of respondents in \code{x}.
#' @param exclude.attributes A comma-separated list of attributes to exclude from the plot.
#' @param colors The colors of the line or columns of the chart as a hex code. This can be
#'  a single color or a vector with length equal to the number of attributes.
#' @param line.width Thickness of line if \code{output} is "Line".
#' @param global.font.family Character; font family for all occurrences of any
#' font attribute for the chart unless specified individually.
#' @param global.font.color Global font color as a named color in character format
#' (e.g. "black") or an a hex code.
#' @param font.units One of "px" or "pt".
#' @param level.label.font.family Character; font family for attribute level labels.
#' @param level.label.font.size Integer; Font size for attribute level labels.
#' @param level.label.font.color Font color as a named color
#' @param level.label.wrap Logical; whether to wrap long attribute level labels.
#' @param level.label.wrap.nchar Integer; number of characters in each line
#'  when \code{level.label.wrap} is \code{TRUE}.
#' @param grid.color The color of the grid lines.
#' @param grid.width Width of the grid lines in pixels.
#' @param values.zero.line.width Width of the line at \code{utility = 0}. This will
#'  be drawn over the grid.
#' @param values.zero.line.color Color of the line at \code{utility = 0}.
#' @param values.minimum The lower bound of the values axis. If not specified it will be
#'  automatically determined from the utility scores
#' @param values.maximum The upper bound of the values axis. If not specified it will be
#'  automatically determined from the utility scores
#' @param values.title Text used as the title for the values axis.
#' @param values.title.font.family Character; font family for values titles.
#' @param values.title.font.size Integer; Font size for values titles.
#' @param values.title.font.color Font color as a named color
#' @param values.tick.font.family Character; font family for values ticks.
#' @param values.tick.font.size Integer; Font size for values ticks.
#' @param values.tick.font.color Font color as a named color
#' @param attr.tick.font.family Character; font family for attribute ticks.
#' @param attr.tick.font.size Integer; Font size for attribute ticks.
#' @param attr.tick.font.color Font color as a named color
#' @param attr.tick.length Length of ticks
#' @param hovertext.decimals Number of decimals shown in hovertext.
#' @param margin.top Margin between plot area and the top of the
#' graphic in pixels
#' @param margin.bottom Margin between plot area and the bottom of the
#' graphic in pixels
#' @param margin.left Margin between plot area and the left of the
#' graphic in pixels
#' @param margin.right Margin between plot area and the right of the
#' graphic in pixels
#' @importFrom flipStatistics Mean
#' @importFrom flipU ConvertCommaSeparatedStringToVector
#' @importFrom plotly plot_ly layout add_trace add_text toRGB config
#' @examples
#' xx <- list(Feed = list(Grain = 0.2, Vegetables = 0.1), 
#'            Weight = list(`55g` = 0.4, `60g` = 0.5, `75g` = 0.6))
#' Utilities(xx)
#' @export
Utilities <- function(x,
                      scaling = c("As is", "Min = 0", "Mean = 0")[2],
                      levels.order = c("As is", "Reverse", "Increasing", "Decreasing")[3],
                      attr.order = c("As is", "Increasing", "Decreasing")[1],
                      attr.levels.not.ordered = NULL,
                      weights = NULL,
                      subset = NULL,
                      exclude.attributes = NULL,
                      output = c("Line", "Column", "Bar", "Data")[1],
                      colors = "#5C9AD3",
                      line.width = 3,
                      global.font.family = "Arial",
                      global.font.color = "#2C2C2C",
                      font.units = "px",
                      grid.color = "#E1E1E1",
                      grid.width = 1,
                      level.label.font.family = global.font.family,
                      level.label.font.color = global.font.color,
                      level.label.font.size = 10,
                      level.label.wrap = TRUE,
                      level.label.wrap.nchar = 20,
                      values.minimum = NULL,
                      values.maximum = NULL,
                      values.title = "Utility",
                      values.title.font.family = global.font.family,
                      values.title.font.color = global.font.color,
                      values.title.font.size = 12,
                      values.zero.line.color = grid.color,
                      values.zero.line.width = 1,
                      values.tick.font.family = global.font.family,
                      values.tick.font.color = global.font.color,
                      values.tick.font.size = 10,
                      attr.tick.font.family = global.font.family,
                      attr.tick.font.color = global.font.color,
                      attr.tick.font.size = 12,
                      attr.tick.length = 5,
                      hovertext.decimals = 3,
                      margin.top = 20,
                      margin.bottom = 50,
                      margin.right = 60,
                      margin.left = 80)
{
    if (tolower(font.units) %in% c("pt", "points"))
    {
        f.scale <- 1.3333
        level.label.font.size <- round(f.scale * level.label.font.size, 0)
        values.title.font.size <- round(f.scale * values.title.font.size, 0)
        values.tick.font.size <- round(f.scale * values.tick.font.size, 0)
        attr.tick.font.size <- round(f.scale * attr.tick.font.size, 0)
    }

    utilities <- NULL
    if (inherits(x, "FitChoice"))
    {
        if (!is.null(x$simulated.respondent.parameters))
            stop("Respondent parameters are from simulation data.")
        params <- x$respondent.parameters
        attributes <- x$attribute.levels
        n.params <- ncol(params)

        # Add fake levels for numeric attributes
        for (attr in names(attributes))
        {
            if (length(attributes[[attr]]) <= 1)
            {
                param.ind <- which(colnames(params) == attr)
                tmp.vals <- x$processed.data$parameter.range[attr,]

                attributes[[attr]] <- as.character(tmp.vals)
                new.data <- cbind(params[,param.ind]*tmp.vals[1], params[,param.ind]*tmp.vals[2])
                colnames(new.data) <- paste0(attr, ": ", as.character(tmp.vals))
                if (param.ind == 0)
                    params <- cbind(new.data, params[,-param.ind])
                else if (param.ind == ncol(params))
                    params <- cbind(params[,-param.ind], new.data)
                else
                    params <- cbind(params[,1:(param.ind-1)], new.data, params[,(param.ind+1):n.params])
            }
        }
    } else if (is.data.frame(x) || is.matrix(x))
    {
        params <- as.matrix(x)
        attributes <- makeAttributeList(colnames(x))

    } else
    {
        params <- NULL
        utilities <- unlist(x)
        attributes <- list()
        u.names <- c()
        for (i in 1:length(x))
        {
            attr <- names(x)[i]
            attributes[[attr]] <- names(x[[i]])
            u.names <- c(u.names, paste0(attr, ": ", names(x[[i]])))
        }
        if (length(u.names) != length(utilities))
            stop("List input has incorrect structure")
        names(utilities) <- u.names
    }

    if (output == "Data" || !is.null(utilities))
    {
        if (length(subset) > 0)
        {
            warning("Filters not applied to utilities.")
            subset <- NULL
        }
        if (length(weights) > 0)
        {
            warning("Weights not applied to utilities.")
            weights <- NULL
        }

    } else
    {
        # Filters and weights
        if (length(subset) > 1 && length(subset) != nrow(params))
            stop("'subset must have length equal to the number of respondents (", nrow(params), ").")
        if (length(weights) > 0 && length(weights) != nrow(params))
            stop("'weights' must length equal to the number of respondents (", nrow(params), ").")
        if (length(subset) == nrow(params))
        {
            params <- params[subset,]
            if (length(weights) > 0)
                weights <- weights[subset]
        }
    }
    if (is.null(utilities))
        utilities <- Mean(params, weights = weights)

    # Exclude attributes
    if (!is.null(exclude.attributes))
        exclude.attributes <- ConvertCommaSeparatedStringToVector(exclude.attributes)
    for(attr in exclude.attributes)
    {
        ind <- which(names(attributes) == attr)
        if (length(ind) == 0)
            warning("Model does not contain attribute '", attr, "'.")
        else if (length(ind) > 0)
        {
            attributes[[ind]] <- NULL
            ind <- which(grepl(paste0("^", attr, ":"), names(utilities)))
            if (length(ind))
                utilities <- utilities[-ind]
            if (output == "Data")
                params <- params[,-ind]
        }
    }

    ind <- which(sapply(attributes, length) <= 1)
    if (length(ind) > 0)
        stop("Cannot show utilities for ", names(attributes)[ind], " which has no levels.")


    if (output == "Data")
    {
        result <- t(apply(params, 1, rescaleUtilities, scaling = scaling, attributes = attributes))
        return(result)
    }

    # Set up data frame for plotting
    df <- data.frame(attribute = rep(names(attributes), sapply(attributes, length)),
                  levels = unname(unlist(attributes)),
                  utility = rescaleUtilities(utilities, scaling, attributes),
                  name = names(utilities),
                  stringsAsFactors = FALSE)

    # Reorder attributes and add NA rows between
    if (!is.null(attr.levels.not.ordered))
        attr.levels.not.ordered <- ConvertCommaSeparatedStringToVector(attr.levels.not.ordered)
    attr.unknown <- setdiff(attr.levels.not.ordered, names(attributes))
    if (length(attr.unknown) > 0)
        warning("Model does not contain attribute '", 
        paste(attr.unknown, collapse = "', '"), "'.")
        
    all.indexes <- c()
    a.range <- tapply(df$utility, df$attribute, function(x){diff(range(x))})
    a.ord <- 1:length(a.range)
    if (attr.order != "As is")
        a.ord <- order(a.range, decreasing = attr.order == "Decreasing")
    for (aai in a.ord)
    {
        lev.ind <- which(df$attribute == names(a.range)[aai])
        excl <- any(names(a.range)[aai] %in% attr.levels.not.ordered)
        if (levels.order == "Reverse" && !excl)
            lev.ind <- rev(lev.ind)
        else if (!excl && levels.order %in% c("Increasing", "Decreasing"))
            lev.ind <- lev.ind[order(df$utility[lev.ind], decreasing = levels.order == "Decreasing")]
        all.indexes <- c(all.indexes, NA, lev.ind)
    }
    df <- df[all.indexes[-1],]

    # Creating the plot
    if (is.null(values.minimum) && sign(min(df$utility, na.rm = TRUE)) == -1)
        values.minimum <- min(1.1 * df$utility, na.rm = TRUE)
    else if (is.null(values.minimum))
        values.minimum <- min(c(0, 0.9 * df$utility), na.rm = TRUE)
    if (is.null(values.maximum))
        values.maximum <- max(1.1 * df$utility, na.rm = TRUE)

    a.list <- unique(df$attribute[!is.na(df$utility)])
    colors <- paste0(rep("", length(a.list)), colors)
    vh <- if (output == "Bar") "h" else "v"
    attr.pos <- tapply(1:nrow(df), df$attribute, function(x){(min(x)+max(x))/2})
    hovertext = sprintf(paste0("%s: %.", hovertext.decimals, "f"), df$levels, df$utility)
    label.text <- df$levels
    if (level.label.wrap)
        label.text <- sapply(label.text, lineBreakEveryN, level.label.wrap.nchar)
    if (output == "Line" && levels.order == "Decreasing")
        label.pos <- paste("right", ifelse(df$utility < 0, "bottom", "top"))
    else if (output == "Line")
        label.pos <- paste("left", ifelse(df$utility < 0, "bottom", "top"))
    else if (output == "Column")
        label.pos <- paste("center", ifelse(df$utility < 0, "bottom", "top"))
    else
        label.pos <- paste(ifelse(df$utility < 0, "left", "right"), "middle")


    p <- plot_ly(as.data.frame(df))
    for (i in 1:length(a.list))
    {
        attr <- a.list[i]
        ind <- which(df$attribute == attr)
        xvals <- if (output == "Bar") df$utility[ind] else ind
        yvals <- if (output == "Bar") ind else df$utility[ind]
        marker <- list(color = colors[i], line = list(width = 0))

        # Main trace
        if (output %in% c("Column", "Bar"))
            p <- add_trace(p, x = xvals, y = yvals, name = attr,
                 text = hovertext[ind], hoverinfo = "name+text",
                 type = "bar", marker = marker, orientation = vh)
        else
            p <- add_trace(p, x = xvals, y = yvals, name = attr,
                 text = hovertext[ind], hoverinfo = "name+text",
                 type = "scatter", mode = "lines",
                 line = list(color = colors[i], width = line.width))

        # Level labels
        if (output == "Bar")
            xvals <- 1.01 * xvals
        else if (output == "Column")
            yvals <- 1.01 * yvals

        p <- add_text(p, x = xvals, y = yvals, text = label.text[ind],
                  hoverinfo = "skip", cliponaxis = FALSE,
                  textposition = label.pos[ind],
                  textfont = list(family = level.label.font.family,
                  size = level.label.font.size, color = level.label.font.color))
    }

    val.axis <- list(title = values.title, range = c(values.minimum, values.maximum),
                autorange = FALSE, showline = FALSE, showgrid = grid.width > 0, color = grid.color,
                gridwidth = grid.width, zerolinewidth = values.zero.line.width,
                zerolinecolor = values.zero.line.color, zeroline = values.zero.line.width > 0,
                titlefont = list(family = values.title.font.family,
                color = values.title.font.color, size = values.title.font.size),
                tickfont = list(family = values.tick.font.family,
                color = values.tick.font.color, size = values.tick.font.size))
    attr.axis <- list(tickmode = "array", tickvals = attr.pos, ticktext = names(attr.pos),
                color = grid.color, showline = FALSE, zeroline = FALSE,
                showgrid = grid.width > 0, ticklen = attr.tick.length, attr.tick.width = 0,
                tickcolor = toRGB("white", alpha = 0.0), tickfont = list(family = attr.tick.font.family,
                color = attr.tick.font.color, size = attr.tick.font.size))

    p <- layout(p, showlegend = FALSE,
                xaxis = if (output == "Bar") val.axis else attr.axis,
                yaxis = if (output == "Bar") attr.axis else val.axis,
                hoverlabel = list(namelength = -1),
                hovermode = if (output == "Bar") "closest" else "x",
                margin = list(t = margin.top, l = margin.left, r = margin.right, b = margin.bottom, pad = 0),
                plot_bgcolor = toRGB("white", alpha = 0), paper_bgcolor = toRGB("white", alpha = 0))
    p <- config(p, displayModeBar = FALSE)
    p$sizingPolicy$browser$padding <- 0
    p
}


# Takes a single string and puts <br> in place of the closest space preceding the n= value character.
## E.g. if n= 20 then count 20 characters.  The space preceding character 20 is replaced by "<br>".
lineBreakEveryN <- function(x, n = 21)
{
    if (n <= 0)
        stop("Wrap line length cannot be smaller than 1")

    w.list <- strsplit(x, " ")[[1]]
    final <- w.list[1]
    c.len <- nchar(final)
    for (ww in w.list[-1])
    {
        new.len <- c.len + nchar(ww) + 1
        if (new.len > n)
        {
            final <- paste0(final, "<br>", ww)
            c.len <- nchar(ww)
        } else
        {
            final <- paste0(final, " ", ww)
            c.len <- new.len
        }
    }
    final
}

rescaleUtilities <- function(utilities, scaling, attributes)
{
    # assume that attributes is a list and
    # the names of unlist(attributes) matches names of utilities
    n <- length(attributes)
    offset <- 0
    tmp.range <- rep(NA, n)

    for (i in 1:n)
    {
        attr <- names(attributes)[i]
        ind <- which(grepl(paste0("^", attr, ":"), names(utilities)))
        if (length(ind) <= 1)
            next

        if (grepl("Min = 0", scaling))
            offset <- min(utilities[ind])
        else if (grepl("Mean = 0", scaling))
            offset <- mean(utilities[ind])
        utilities[ind] <- utilities[ind] - offset
        tmp.range[i] <- diff(range(utilities[ind]))
    }

    mult <- 1
    if (scaling == "Min = 0; Max = 100")
        mult <- 100/rep(tmp.range, sapply(attributes, length))
    else if (grepl("Mean range = 100", scaling))
        mult <- 100/mean(tmp.range)
    else if (grepl("Max range = 100", scaling))
        mult <- 100/max(tmp.range)
    return(mult * utilities)
}


calcRange <- function(data)
{
    nms.all <- names(data)
    nms <- unique(nms.all)
    nvar <- length(nms)
    result <- matrix(NA, nrow = nvar, ncol = 2, dimnames = list(nms, c("min", "max")))
    for (i in 1:nvar)
    {
        ind <- which(nms.all == nms[i])
        tmp.dat <- unlist(data[,ind])
        if (is.numeric(tmp.dat))
            result[i,] <- range(tmp.dat, na.rm = TRUE)
    }
    return(result)
}
