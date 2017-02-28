flow.extract <- function(resource, dir.output, dir.input, dir.input.old, log.file) {
    #extract.prepare (=rename)
    #extract.get (=download)
    #extract.read (=read.px OR fread)
    dc <- extract.read(resource$url, resource$typ)
    #if exists(resource.old) {
    #    extract.read(resource.old)
    #    extract.verify =call to r<-compare.datacubes(timekey=...,val)
    #                               collapse(r$elem[setdiff(names(r$elem),r$timekey)])
    #}
    return(dc)
}
