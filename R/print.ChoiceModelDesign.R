#' Display choice model design output as an \code{htmlwidget}
#'
#' Creates a \code{htmlwidget} summary of diagnostic information for a
#' choice model design output from \code{\link{ChoiceModelDesign}}.
#' @param x An object of class \code{ChoiceModelDesign} output from
#' \code{\link{ChoiceModelDesign}}.
#' @param css Optional string giving the path to a file containing additional
#' CSS to style the htmlwidget output.
#' @param nsmall Integer; see \code{\link{format}}.
#' @param digits Integer; see \code{\link{format}}.
#' @param ... Currently, ignored.
#' @return An \code{htmlwidget} containing diagnostic information for
#'     the experimental design, including D-error, standard errors,
#'     frequenices, pairwise frequencies, the labeled design, and
#'     prior information. An attribute called \code{"ChartData"} also
#'     contains the labeled design.
#' @seealso \code{\link{ChoiceModelDesign}},
#'     \code{\link[rhtmlMetro]{Box}}
#' @export
#' @method print ChoiceModelDesign
#' @importFrom knitr kable
print.ChoiceModelDesign <- function(x, css = NULL, nsmall = 2, digits = 2, ...)
{
    tfile <- tempfile(fileext = ".html")
    on.exit(if (file.exists(tfile)) file.remove(tfile))
    b.o <- x$balances.and.overlaps

    cata <- function(...)
        cat(..., file = tfile, append = TRUE)
    format1 <- function(x)
        format(x, nsmall = nsmall, digits = digits)

    ## add CSS
    cat("<style type=\"text/css\">\n", file = tfile)
    default.css <- readLines(system.file("css", "cmd.css", package = "flipChoice"))
    if (!is.matrix(x$prior)) ## chg css for prior table if no std. dev. specified
        default.css <- sub("nth-child(3n+3)", "nth-child(even)", default.css, fixed = TRUE)
    cata(default.css, fill = TRUE)
    cata("</style>\n\n")

    if (!is.null(css) && file.exists(css))
    {
        cat("<style type=\"text/css\">\n", file = tfile)
        cata(readLines(css), fill = TRUE)
        cata("</style>\n\n")
    }

    ## Needed so that Box has scollbar
    cata("<div class=\"main-container\">")

    ## Title
    cata("<h1>Choice Model: Experimental Design</h1>")

    ## statistics
    addStatistics(tfile, x, digits, nsmall)

    ## Standard errors
    cata("<details open=\"true\"><summary>Standard Errors</summary>\n")
    cata(makeStandardErrorTable(x$standard.errors, x$attribute.levels,
                                digits = digits, nsmall = nsmall), fill = TRUE)
    cata("</details>")

    ## Frequencies
    cata("<details open=\"true\"><summary>Frequencies</summary>\n")
    cata(makeFrequencyTable(b.o$singles, x$attribute.levels), fill = TRUE)
    cata("</details>")

    ## Pairwise frequencies
    cata("<details open =\"true\"><summary>Pairwise Frequencies</summary>\n")
    mapply(addPairwiseFrequencyTable, b.o$pairs, names(b.o$pairs),
           MoreArgs = list(attr.names = names(x$attribute.levels), tfile = tfile))
    cata("</details>")

    ## Design
    cata("<details><summary>Design</summary>\n")
    cata(knitr::kable(x$labeled.design, align = "c",
                      longtable = TRUE, format = "html", digits = digits),
         fill = TRUE)
    cata("</details>")

    ## Priors
    cata("<details open =\"true\"><summary>Prior</summary>\n")
    if (!is.null(x$prior))
        cata(makePriorTable(x$prior, x$attribute.levels), fill = TRUE)
    else
        cata("<p>None specified.</p></details>")
    cata("</div>", fill = TRUE)

    html <- paste(readLines(tfile), collapse = "\n")
    out <- rhtmlMetro::Box(html, text.as.html = TRUE,
                    font.family = "Circular, Arial, sans-serif",
                    font.size = 8)
    attr(out, "ChartData") <- x$labeled.design
    out
}

addStatistics <- function(tfile, x, digits, nsmall)
{
    cata <- function(...)
        cat(..., file = tfile, append = TRUE)
    format1 <- function(x)
        format(x, nsmall = nsmall, digits = digits)

    b.o <- x$balances.and.overlaps
    cata("<details open=\"true\">\n")
    cata("<summary>Statistics</summary>")
    ## cata("<p style=\"text-align: left;\">")
    ## cata(paste0("<b>D-error: </b>", format1(x$d.error)))
    ## cata(paste0("<span style=\"float: right;\">",
    ##             "<b>Mean version balance: </b>",
    ##             format1(b.o$mean.version.balance),
    ##             "</span></p>\n"))
    ## cata(paste0("<b>Across version balance: </b>",
    ##             format1(b.o$across.version.balance)))
    ## cata(paste0("<span style=\"float: right;\">",
    ##             "<b>Mean version balance: </b>",
    ##             format1(b.o$mean.version.balance),
    ##             "</span></p>\n"))
    cata("<table><tbody><tr>\n")
    cata("<td style=\"text-align: left;\">")
    cata(paste0("<b>Algorithm: </b>", x$design.algorithm, "</td>"))
    cata("<td style=\"text-align: left;\">")
    cata(paste0("<b>D-error: </b>", format1(x$d.error), "</td></tr>"))
    cata(paste0("<tr><td style=\"text-align: left;\">",
                "<b>Mean version balance: </b>",
                format1(b.o$mean.version.balance),
                "</td>\n"))
    cata(paste0("<td style=\"text-align: left;\">",
                "<b>Across version balance: </b>",
                format1(b.o$across.version.balance),
                "</td></tr>\n"))
    cata(paste0("<tr><td style=\"text-align: left;\">",
                "<b>Mean version pairwise balance: </b>",
                format1(b.o$mean.version.pairwise.balance),
                "</td>\n"))
    cata(paste0("<td style=\"text-align: left;\">",
                "<b>Across version pairwise balance: </b>",
                format1(b.o$across.version.pairwise.balance),
                "</td></tr>\n"))
    cata("</tbody></table></details>\n\n")
    invisible()
}

#' @importFrom knitr kable
makeStandardErrorTable <- function(std.err, al, digits, nsmall)
{
    zero.str <- ".000"
    format1 <- function(x)
        sub("^0[.]", ".", format(x, nsmall = nsmall, digits = digits))
    max.len <- max(vapply(al, length, 0L))

    out <- matrix("", nrow = max.len, ncol = 2*length(al))
    cnames <- character(ncol(out))
    cnames[seq(1, ncol(out), by = 2)] <- names(al)
    idx <- 1
    for (i in seq_along(al))
    {
        lvls <- al[[i]]
        n.lvls <- length(lvls)
        out[seq_len(n.lvls), 2*(i-1)+1] <- lvls
        out[seq_len(n.lvls), 2*i] <- c(zero.str, format1(std.err[idx:(idx+n.lvls - 2), 2]))
        idx <- idx + n.lvls-1
    }
    knitr::kable(out, format = "html", col.names = cnames, digits = digits,
                 table.attr = "id=\"std-err-table\"",
                 align = rep(c("l", "r"), length(al)))
}

#' @importFrom knitr kable
makeFrequencyTable <- function(freq, al)
{
    max.len <- max(vapply(al, length, 0L))
    out <- matrix("", nrow = max.len, ncol = 2*length(al))
    cnames <- character(ncol(out))
    cnames[seq(1, ncol(out), by = 2)] <- names(al)
    idx <- 1
    for (i in seq_along(al))
    {
        lvls <- al[[i]]
        n.lvls <- length(lvls)
        out[seq_len(n.lvls), 2*(i-1)+1] <- lvls
        out[seq_len(n.lvls), 2*i] <- freq[[i]]
        idx <- idx + n.lvls-1
    }
    knitr::kable(out, format = "html", col.names = cnames, align = "c")
}

addPairwiseFrequencyTable <- function(tfile, ptable, table.name, attr.names)
{
    cata <- function(...)
        cat(..., file = tfile, append = TRUE)

    ## table.names always has form "attr.name1/attr.name2"
    ## be overly cautious to extract the two names w/o strsplit
    idx1 <- vapply(attr.names, function(n) grepl(paste0("^", n, "[/]"), table.name), FALSE)
    idx2 <- vapply(attr.names, function(n) grepl(paste0("[/]", n, "$"), table.name), FALSE)

    cata(paste0("<h3>", table.name, "</h3>\n"))
    cata(knitr::kable(ptable, row.names = TRUE, col.names = colnames(ptable),
                      format = "html", align = "c",
                      table.attr = "class=\"pairwise-table\""), fill = TRUE)
    invisible()
}

#' @importFrom knitr kable
makePriorTable <- function(prior, al)
{
    zero.str <- ".000"
    n.lvls <- vapply(al, length, 0L)
    max.len <- max(n.lvls)
    sd.prior.given <- is.matrix(prior)
    prior <- as.matrix(prior)

    n.col <- ifelse(sd.prior.given, 3*length(al), 2*length(al))
    out <- matrix("", nrow = max.len, ncol = n.col)
    cnames <- character(ncol(out))
    if (sd.prior.given)
    {
        cnames[seq(1, ncol(out), by = 3)] <- names(al)
        cnames[seq(2, ncol(out), by = 3)] <- "Mean"
        cnames[seq(3, ncol(out), by = 3)] <- "Std. Dev."
    }else
        cnames[seq(1, ncol(out), by = 2)] <- names(al)

    idx <- 1
    for (i in seq_along(al))
    {
        lvls <- al[[i]]
        n.lvls <- length(lvls)
        col.idx <- ifelse(sd.prior.given, 3*(i-1)+1, 2*(i-1)+1)
        out[seq_len(n.lvls), col.idx] <- lvls
        col.idx <- ifelse(sd.prior.given, 3*(i-1)+2, 2*i)
        out[seq_len(n.lvls), col.idx] <- c(zero.str, prior[idx:(idx+n.lvls - 2), 1])
        if (sd.prior.given)
            out[seq_len(n.lvls), 3*i] <- c(zero.str, prior[idx:(idx+n.lvls - 2), 2])
        idx <- idx + n.lvls-1
    }
    knitr::kable(out, format = "html", col.names = cnames,
                 table.attr = "id=\"prior-table\"",
                 align = rep(c("l", "r", "r"), length(al)))

}
