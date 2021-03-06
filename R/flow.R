# make data flow from source to destination
#' @export
flow <- function(resources, dir.commit, dir.output, dir.input, dir.input.old, dir.log, output.type = "csv", log.file, log.email, git.exe = "git") {

    resources <- flow.init(resources, dir.commit, dir.output, dir.input, dir.input.old, dir.log, output.type, log.file)

    # loop over datacubes
    for (i in 1:nrow(resources)) {
        name.short <- stri_replace_first_regex(resources[i]$name, "^(.{18})....+(.{19})$", "$1...$2")
        i.msg <- sprintf("\r%2i - %-40s", i, name.short)
        ####################
        msg.console<-paste0(i.msg, " - EXTRACTING ...")
        dc <- flow.extract(resources[i], log.file, log.email, msg.console)
        ####################
        cat(i.msg, " - TRANSFORMING ...              ", sep = "")
        dc<-flow.transform(dc, resources[i]$transformer)
        ####################
        cat(i.msg, " - LOADING ...                   ", sep = "")
        flow.load(dc, resources[i]$output, output.type = output.type)
        # cat("done")
        rm(dc)
    }
    ########################
    #on success COMMIT
    flow.commit(resources$output, resources$commit, log.file, dir.commit, dir.input.old, git.exe)
}
