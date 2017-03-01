#make data flow from source to destination
flow <- function(resources, dir.commit, dir.output, dir.input, dir.input.old, dir.log, output.type = "csv", log.file) {
    #init resource table
    resources[, input := paste0(dir.input, resource$cubename, ".", resource$type)]
    resources[, input.old := paste0(dir.input.old, resource$cubename, ".", resource$type)]
    resources[, output := paste0(dir.output, resource$cubename, ".", output.type)]
    resources[, commit := paste0(dir.commit, resource$cubename, ".", output.type)]
    resources[, log := paste0(dir.log, resource$cubename, ".log")]

    # loop over datacubes
    for (i in 1:nrow(resources)) {
        ####################
        log.console<-paste0("\n", i, " - EXTRACTING...")
        dc <- flow.extract(resources[i, ], log.file, log.console)
        stopifnot(is.data.table(dc))
        ####################
        cat("\n", i, " - TRANSFORMING...", sep = "")
        #dc<-flow.transform(dc, action, param...)
        stopifnot(is.data.table(dc))
        ####################
        cat("\n", i, " - LOADING...", sep = "")
        #flow.load(dc, file.out)
        #stopifnot(data.is.written.ok???)
        cat("done\n")
    }
    ########################
    #on success {
    cat("COMMITTING...")
    #flow.commit()
    #stopifnot(commit.ok???)
    cat("OK\n")
    #}
}
