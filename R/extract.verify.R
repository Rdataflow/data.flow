extract.verify <- function(datacube, datacube.old, val=c("Wert","wert","Value","value"),
                           timekey=c("Jahr","jahr","Year","year"), log.detail, log.file, resource.name, threshold = 0.01) {
    r <- compare.datacubes(datacube, datacube.old, val, timekey, log.detail)

    ## warn / info level
    #code auf WARN, falls add elem != r$timekey
    #code auf WARN, falls del elem

    if (r$max >= threshold) {
        #ToDo:
        #code auf WARN, falls max > threshold
        #...
    }


    ## output to log & console
    #verify.log.summary(r)
    #    spalte32<-collapse.changes.in.elem.per.dim(r$elem[setdiff(names(r$elem),r$timekey)])
    #    ...compose summary.log (csv)
    #verify.log.console(r)
    #    ...compose console output
    #
    #ev. tk.message.box(TITLE, MSG)

}
