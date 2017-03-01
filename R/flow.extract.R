flow.extract <- function(resource, log.file, log.console) {
    #extract.prepare (=rename)
    if (file.exists(resource$input)) {
        cat(log.console, "preparing... ")
        tmp<-file.rename(resource$input, resource$input.old)
    }

    #extract.get (=download)
    cat(log.console, "downloading... ")
    download.file(resource$url, resource$input, quiet = TRUE, mode = "wb")
    stopifnot(file.exists(resource$input))

    #extract.read (=read.px OR fread)
    cat(log.console, "reading... ")
    dc <- extract.read(resource$input, resource$type)

    #...
    if (file.exists(resource$input.old)) {
        #read old
        cat(log.console, "reading.old... ")
        dc.old <- extract.read(resource$input.old, resource$type)

        #verify structure and range of values
        cat(log.console, "verifying... ")
        extract.verify(dc, dc.old, val=c("Wert","wert","Value","value"),
                       timekey=c("Jahr","jahr","Year","year"), log.detail=resource$log, log.file, resource.name=resource$name)
    }

    return(dc)
}
