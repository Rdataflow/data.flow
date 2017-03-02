extract.verify <- function(datacube, datacube.old, val=c("Wert","wert","Value","value"),
                           timekey=c("Jahr","jahr","Year","year"), log.detail, log.file, resource.name, threshold = 0.01) {

    r <- compare.datacubes(datacube, datacube.old, val, timekey, log.detail)

    #code auf ERR, falls add elem != r$timekey
    #code auf ERR, falls del elem


    ## output to log & console
    #verify.log.summary(r)
    #    spalte32<-collapse.changes.in.elem.per.dim(r$elem[setdiff(names(r$elem),r$timekey)])
    #    max<-sprintf("%+.1f%%",r$max*100)
    #    ...compose summary.log (csv)
    #verify.log.console(r)
    #    ...compose console output
    #
    #ev. tk.message.box(TITLE, MSG)
    if (is.numeric(r$max) & length(r$max)>0) {
        if (abs(r$max) >= threshold) {
            #ToDo:
            #code auf WARN, falls max > threshold
            #...
            max.change<-sprintf("%+.1f%%",r$max*100)
            msgBox <- tk_messageBox(title = "WARNING", message = paste0("WARNING: ", resource.name, "\nData changed by ", max.change), icon = "warning", type = "ok")
        }
    }

    # stop on error
    if(r$status>3) {
        msgBox <- tk_messageBox(title = "ERROR", message = paste0("ERROR: ", resource.name, "\nData structure broken"), icon = "error", type = "ok")
        stop("ERROR: dataset structure differs!!!")
    }
}
