extract.verify <- function(datacube, datacube.old, val=c("Wert","wert","Value","value"),
                           timekey=c("Jahr","jahr","Year","year","Zeit"), log.detail, log.file, log.email, resource.name, warn.threshold, lang = "de") {

    r <- compare.datacubes(datacube, datacube.old, val, timekey, log.detail, warn.threshold)

    r <- eval.compare.datacubes(r)

    ## output to log
    r <- log.to.file(r, resource.name, log.file)

    ## output to console
    log.to.console(r, resource.name)

    ## alert via email
    log.via.email(r, resource.name, log.email, lang)

    ## output as msgbox
    log.to.msgbox(r, resource.name, lang)

    # stop on error, if we didn't stop already
    if(r$code >= 64) {
        stop(paste0("ERROR: ", resource.name, r$msg[[lang]]), "\n\n")
    }
}
