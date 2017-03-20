flow.extract <- function(resource, log.file, log.email, msg.console,
                         val=c("Wert","wert","Value","value"),
                         timekey=c("Jahr","jahr","Year","year"), warn.threshold = 0.01, lang = "de") {

    #extract.get (=download)
    cat(msg.console, "downloading... ")
    download.file(resource$url, resource$input, quiet = TRUE, mode = "wb")
    stopifnot(file.exists(resource$input))

    #extract.read (=read.px OR fread)
    cat(msg.console, "reading...     ")
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
        # dc$DATA$value[2,value:=NA]                                                      # -
        # dc$DATA$value[3,value:=Inf]                                                     # -
        # dc$DATA$value[4,value:=0]                                                       # -
        # dc$DATA$value[11,Jahr:="2099"]                                                  # info
        # dc$DATA$value[5,value:=1]                                                       # warn
        # dc$DATA$value<-dc$DATA$value[Jahr!="2008"]                                      # warn
        # dc$DATA$value[6,value:=-10000000]                                               # warn
        # dc$DATA$value<-dc$DATA$value[111,Land:="LALALAND"]                              # error & abort
        # dc$DATA$value[1,Kanton:="Ausserschweiz"]                                        # error & abort
        # dc$DATA$value<-dc$DATA$value[Kanton!="Glarus"]                                  # error & abort



        #verify structure and range of values
        cat(msg.console, "verifying...   ")
        extract.verify(dc$DATA$value, dc.old$DATA$value, val = dc$DSD$val, timekey = dc$DSD$timekey,
                       log.detail = resource$log, log.file, log.email, resource.name = resource$name, warn.threshold, lang)
        ### will stop within extract.verify on broken data structure ...
    }

    ### on success: backup (=rename)
    tmp <- file.rename(resource$input, resource$input.old)

    return(dc)
}
