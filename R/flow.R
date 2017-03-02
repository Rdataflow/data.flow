#make data flow from source to destination
flow <- function(resources, dir.commit, dir.output, dir.input, dir.input.old, dir.log, output.type, log.file) {

    flow.init(resources, dir.commit, dir.output, dir.input, dir.input.old, dir.log, output.type, log.file)

    # loop over datacubes
    for (i in 1:nrow(resources)) {
        i.msg <- paste0("\n", i, " - ", str_sub(resources[i]$name,-30))
        ####################
        log.console<-paste0(i.msg, " - EXTRACTING ... ")
        dc <- flow.extract(resources[i], log.file, log.console)
        ####################
        cat(i.msg, " - TRANSFORMING ... ", sep = "")
        dc<-flow.transform(dc, resources[i]$transformer)
        ####################
        cat(i.msg, " - LOADING ... ", sep = "")
        flow.load(dc, resources[i]$output, output.type = output.type, val="value") ### ToDo: autodetect val!!!
        cat("done\n")
        rm(dc)
    }
    ########################
    #on success
    cat("COMMITTING all data ... ")
    #flow.commit(dir.output, dir.commit)
    cat("done\n")
    #msgBox <- tk_messageBox(title = "SUCCESS", message = "data import completed succesfully!", icon = "info", type = "ok")
}
