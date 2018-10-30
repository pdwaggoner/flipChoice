
# Converts a text variable and its format into a design that can be used for fitting.


makeRespondentDesign <- function(design.per.respondent, design.format.per.respondent)
{
    choice.design = design.per.respondent
    choice.design = gsub("[[", "", choice.design, fixed = TRUE)
    choice.design = gsub("]]", "", choice.design, fixed = TRUE)

    choice.format = design.format.per.respondent[1]
    choice.format <- strsplit(as.character(choice.format), ",\"" , fixed = TRUE)[[1]]

    nb.versions <- length(design.per.respondent)
    nb.attributes <- length(unlist(strsplit(choice.format[1], "],[", fixed = TRUE)))
    nb.questions <- as.numeric(unlist(strsplit(choice.format[2], "=>", fixed = TRUE))[2])
    nb.alternatives <- as.numeric(unlist(strsplit(choice.format[3], "=>", fixed = TRUE))[2])
    nb.none <- unlist(strsplit(choice.format[4], "=>", fixed = TRUE))[2]
    nb.none <- as.numeric(substr(nb.none, 1, nchar(nb.none) - 1))

    design <- lapply(choice.design, function(x)
    {
        y = strsplit(x, "],[" , fixed = TRUE)[[1]]
        matrix(as.numeric(unlist(strsplit(y, ","))), ncol = nb.alternatives + 1, byrow = TRUE)
    })

    design <- do.call(rbind, design)
    design <- cbind(design[, 1], rep_len(seq(nb.questions), nrow(design)), design[, -1])
    colnames(design) <- c("Question", "Alternative", paste0("Attribute", seq(nb.attributes)))
    design <- addTaskNumber(design, nb.versions, nb.questions, nb.alternatives)
    design <- addVersions(design, nb.versions)

    # TODO FIX HARDCODED NONE ALTERNATIVES
    design <- addNoneAlternatives(design, nb.alternatives + 1, nb.alternatives)

    level.counts <- apply(design[, 5:ncol(design)], 2, max, na.rm = TRUE)
    attribute.levels <- lapply(level.counts, seq)

    result <- list(design.with.none = design,
                   design.algorithm = "Unknown",
                   attribute.levels = attribute.levels)
    return(result)
}
