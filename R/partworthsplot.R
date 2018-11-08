#' Partworths plot
#'
#' Create plot showing mean utility of each attribute level in the Choice model
#'
#' @param fit a FitChoice output from \code{FitChoiceModel}
#' @param scaling Controls if the mean utility is scaled. One of "As is" (no scaling),
#'  "Min = 0", or "Mean = 0".
#' @param order Controls the order in which the levels of each attribute are shown.
#'  One of "As is", Increasing", or "Decreasing"
#' @param exclude.order A comma-separated list of the attributes which should not be ordered.
#' @param weights An optional vector of sampling weights which should be the
#'  same length as the number of respondents in \code{fit}.
#' @param subset An optional vector specifying a subset of respondents used to take
#'  the mean utility. It should be the same length as the number of respondents in \code{fit}.
#' @param exclude.attributes A comma-separated list of attributes to exclude from the plot.
#' @param chart.type One of "Line" (default) or "Column"
#' @param colors The colors of the line or columns of the chart as a hex code. This can be
#'  a single color or a vector with length equal to the number of attributes.
#' @param line.width Thickness of line if \code{chart.type} is "Line".
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
#' @export
#'
PartworthsPlot <- function(fit,
                           scaling = c("As is", "Min = 0", "Mean = 0")[2],
                           order = c("As is", "Increasing", "Decreasing")[2],
                           exclude.order = NULL,
                           weights = NULL,
                           subset = NULL,
                           exclude.attributes = NULL,
                           chart.type = c("Line", "Column", "Bar")[1],
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
    if (!is.null(fit$simulated.respondent.parameters))
        stop("Respondent parameters are from simulations")

    if (tolower(font.units) %in% c("pt", "points"))
    {
        f.scale <- 1.3333
        level.label.font.size <- round(f.scale * level.label.font.size, 0)
        values.title.font.size <- round(f.scale * values.title.font.size, 0)
        values.tick.font.size <- round(f.scale * values.tick.font.size, 0)
        attr.tick.font.size <- round(f.scale * attr.tick.font.size, 0)
    }

    # Filters and weights
    params <- RespondentParameters(fit)
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
    utilities <- Mean(params, weights = weights)

    # Exclude attributes
    attributes <- makeAttributeList(fit$param.names.list$unconstrained.respondent.pars)
    if (!is.null(exclude.attributes))
        exclude.attributes <- ConvertCommaSeparatedStringToVector(exclude.attributes)
    attr.length <- sapply(attributes, length)
    ind <- which(attr.length <= 1)
    if (length(ind) > 0)
        exclude.attributes <- c(exclude.attributes, names(attributes)[ind])
    for(attr in exclude.attributes)
    {
        ind <- which(names(attributes) == attr)
        if (length(ind) > 0)
        {
            attributes[[ind]] <- NULL
            ind <- which(grepl(paste0("^", attr, ":"), names(utilities)))
            if (length(ind))
                utilities <- utilities[-ind]
            ind <- which(grepl(paste0("^", attr, "$"), names(utilities)))
            if (length(ind))
                utilities <- utilities[-ind]
        }
    }

    # Set up data frame for plotting
    # Levels of different attributes are separated by a row of NAs
    attr.list <- names(attributes)
    ends = cumsum(sapply(attributes, length))
    ends = ends[-length(ends)]
    for (i in rev(ends))
        utilities = append(utilities, NA, i)
    for (i in (length(attributes) - 1):1)
        attributes = append(attributes, list("NA" = ""), i)

    tmp.names <- rep(names(attributes), sapply(attributes, length))
    tmp.names[which(tmp.names == "NA")] <- NA
    df <- data.frame(attribute = tmp.names,
                  levels = unname(unlist(attributes)),
                  utility = utilities,
                  stringsAsFactors = FALSE)

    # Scale and reorder
    if (!is.null(exclude.order))
        exclude.order <- ConvertCommaSeparatedStringToVector(exclude.order)
    for (attr in names(attributes))
    {
        ind <- which(df$attribute == attr)
        if (length(ind) <= 1)
            next

        offset <- 0
        if (scaling == "Min = 0")
            offset <- min(utilities[ind])
        else if (scaling == "Mean = 0")
            offset <- mean(utilities[ind])

        ind.ord <- ind
        if (order %in% c("Increasing", "Decreasing") && !attr %in% exclude.order)
            ind.ord <- ind[order(df$utility[ind], decreasing = order == "Decreasing")]
        df[ind,] <- df[ind.ord,]
        df$utility[ind] <- df$utility[ind] - offset
    }

    # Creating the plot
    if (is.null(values.minimum) && sign(min(df$utility, na.rm = TRUE)) == -1)
        values.minimum <- min(1.1 * df$utility, na.rm = TRUE)
    else if (is.null(values.minimum))
        values.minimum <- min(c(0, 0.9 * df$utility), na.rm = TRUE)
    if (is.null(values.maximum))
        values.maximum <- max(1.1 * df$utility, na.rm = TRUE)

    colors <- paste0(rep("", fit$n.attributes), colors)
    vh <- if (chart.type == "Bar") "h" else "v"
    attr.pos <- tapply(1:nrow(df), df$attribute, function(x){(min(x)+max(x))/2})
    hovertext = sprintf(paste0("%s: %.", hovertext.decimals, "f"), df$levels, df$utility)
    label.text <- df$levels
    if (level.label.wrap)
        label.text <- sapply(label.text, lineBreakEveryN, level.label.wrap.nchar)
    if (chart.type == "Line" && order == "Decreasing")
        label.pos <- paste("right", ifelse(df$utility < 0, "bottom", "top"))
    else if (chart.type == "Line")
        label.pos <- paste("left", ifelse(df$utility < 0, "bottom", "top"))
    else if (chart.type == "Column")
        label.pos <- paste("center", ifelse(df$utility < 0, "bottom", "top"))
    else
        label.pos <- paste(ifelse(df$utility < 0, "left", "right"), "middle")


    p <- plot_ly(as.data.frame(df))
    for (i in 1:fit$n.attributes)
    {
        attr <- attr.list[i]
        if (is.na(attr) || is.null(attr))
            next

        ind <- which(df$attribute == attr)
        xvals <- if (chart.type == "Bar") df$utility[ind] else ind
        yvals <- if (chart.type == "Bar") ind else df$utility[ind]
        marker <- list(color = colors[i], line = list(width = 0))

        # Main trace
        if (chart.type %in% c("Column", "Bar"))
            p <- add_trace(p, x = xvals, y = yvals, name = attr,
                 text = hovertext[ind], hoverinfo = "name+text",
                 type = "bar", marker = marker, orientation = vh)
        else
            p <- add_trace(p, x = xvals, y = yvals, name = attr,
                 text = hovertext[ind], hoverinfo = "name+text",
                 type = "scatter", mode = "lines",
                 line = list(color = colors[i], width = line.width))

        # Level labels
        if (chart.type == "Bar")
            xvals <- 1.01 * xvals
        else if (chart.type == "Column")
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
                xaxis = if (chart.type == "Bar") val.axis else attr.axis,
                yaxis = if (chart.type == "Bar") attr.axis else val.axis,
                hoverlabel = list(namelength = -1),
                hovermode = if (chart.type == "Bar") "closest" else "x",
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
