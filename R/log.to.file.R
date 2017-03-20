log.to.file <- function(r, resource.name, log.file)
{
    out <- data.table('timestamp' = r$timestamp, 'resource.name' = resource.name,
                      'status' = r$status, 'code' = r$code, 'subcode' = r$subcode,
                      'time.added' = "",
                      'time.dropped' = "",
                      'elements.differing' = "",
                      'values.added' = "",
                      'values.dropped' = "",
                      'values.changed' = "",
                      'max.rel.change' = "",
                      'time.changed' = "")


    # time added
    t <- r$elem[[r$timekey]]
    if (length(t$add) > 0) {
         out$time.added <- paste0("+", t$add, collapse="")
    }

    # time dropped
    if (length(t$del) > 0) {
        out$time.dropped <- paste0("-", t$del, collapse="")
    }

    # elements.differing
    elem.diff <- r$elem[names(r$elem) != r$timekey]
    out[, elements.differing := collapse.changes.in.elem.per.dim(elem.diff)]

    # values.added
    out[, values.added := r$add.lines]

    # values.dropped
    out[, values.dropped := r$del.lines]

    # values.changed
    out[, values.changed := r$changed.lines]

    # max.rel.change
    if ("max" %in% names(r)) {
        out[, max.rel.change := sprintf("%+.1f%%", r$max*100)]
    }

    # time.changed
    out[, time.changed := paste0(sort(r$changed[[r$timekey]]), collapse = ",")]

    ### append output to log.file
    fwrite(out, file = log.file, append = TRUE, quote = TRUE, sep = ";", showProgress = FALSE)

    r$out.log <- out
    return(r)
}
