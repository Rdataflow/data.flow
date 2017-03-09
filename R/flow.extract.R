flow.extract <- function(resource, log.file, msg.console,
                         val=c("Wert","wert","Value","value"),
                         timekey=c("Jahr","jahr","Year","year")) {
    #extract.get (=download)
    cat(msg.console, "downloading... ")
    download.file(resource$url, resource$input, quiet = TRUE, mode = "wb")
    stopifnot(file.exists(resource$input))

    #extract.read (=read.px OR fread)
    cat(msg.console, "reading... ")
    dc <- extract.read(resource$input, resource$type)
    stopifnot(is.data.table(dc$DATA$value))

    # add data structure definition (timekey, val) to dc
    dc$DSD$timekey <- intersect(names(dc$DATA$value), c("Jahr","jahr","Year","year"))
    dc$DSD$val     <- intersect(names(dc$DATA$value), c("Wert","wert","Value","value"))


    #...
    if (file.exists(resource$input.old)) {
        #read old
        cat(msg.console, "reading.old... ")
        dc.old <- extract.read(resource$input.old, resource$type)
        stopifnot(is.data.table(dc.old$DATA$value))

        ###fake difference to test for verify response
        # dc$DATA$value[1,value:=value+1]                                                 # -
        # dc$DATA$value[1,value:=NA]                                                      # -
        # dc$DATA$value[1,value:=Inf]                                                     # -
        # dc$DATA$value[1,value:=0]                                                       # -
        # dc$DATA$value<-copy(dc$DATA$value); dc$DATA$value[1,Jahr:="2099"]               # info
        # dc$DATA$value[1,value:=1]                                                       # warn
        # dc$DATA$value[1,value:=-10000000]                                               # warn
        # dc$DATA$value<-copy(dc$DATA$value); dc$DATA$value[1,a:=1]                       # expected: error & abort
        # dc$DATA$value<-copy(dc$DATA$value); dc$DATA$value[1,Kanton:="Ausserschweiz"]    # expected: error & abort
        # dc$DATA$value<-dc$DATA$value[Jahr!="2009"]                                      # expected: error & abort



        #verify structure and range of values
        cat(msg.console, "verifying... ")
        extract.verify(dc$DATA$value, dc.old$DATA$value, val=dc$DSD$val,
                       timekey=dc$DSD$timekey, log.detail=resource$log, log.file, resource.name=resource$name)
        ### will stop within extract.verify on broken data structure ...
    }

    ### on success: backup (=rename)
    tmp<-file.rename(resource$input, resource$input.old)

    return(dc)
}
