.onAttach <- function(libname, pkgname) {
    packageStartupMessage("May the flow be with you...")
}


#make data flow from source to destination
flow <- function(resources, dir.commit, dir.output, dir.input, dir.input.old, dir.log, output.type = "csv", log.file) {
    #init resource table
    resources[, input := paste0(dir.input, resources$name, ".", resources$type)]
    resources[, input.old := paste0(dir.input.old, resources$name, ".", resources$type)]
    resources[, output := paste0(dir.output, resources$name, ".", output.type)]
    resources[, commit := paste0(dir.commit, resources$name, ".", output.type)]
    resources[, log := paste0(dir.log, resources$name, ".log")]

    # loop over datacubes
    for (i in 1:nrow(resources)) {
        ####################
        i.msg <- paste0("\n", i, " - ", resources[i, name])
        log.console<-paste0(i.msg, " - EXTRACTING ... ")
        dc <- flow.extract(resources[i], log.file, log.console)
        stopifnot(is.data.table(dc))
        ####################
        cat(i.msg, " - TRANSFORMING ... ", sep = "")
        #dc<-flow.transform(dc, resources[i]$transformer)
        stopifnot(is.data.table(dc))
        ####################
        cat(i.msg, " - LOADING ... ", sep = "")
        flow.load(dc, resources[i]$output, output.type = output.type, val="value") ### ToDo: autodetect val!!!
        stopifnot(file.exists(resources[i]$output))
        rm(dc)
        cat("done\n")
    }
    ########################
    #on success
    cat("COMMITTING all data ... ")
    #flow.commit(dir.output, dir.load)
    cat("done\n")
    #msgBox <- tkmessageBox(title = "data import", message = "data import completed succesfully!", icon = "info", type = "ok")
}
