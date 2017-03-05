flow.transform <- function(dc, transformer = NULL) {
    ###
    ###
    # TRANSFORM
    ###
    ###

    stopifnot(is.data.table(dc$DATA$value))
    return(dc)
}
