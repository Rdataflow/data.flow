#make data flow from source to destination
flow <- function(resources, dir.commit, dir.output, dir.input, dir.input.old, log.file) {
    #init resource table
    for (i in 1:nrow(resources)) {
        ####################
        cat("EXTRACTING...")
        dc <- flow.extract(resources[i,], dir.output, dir.input, dir.input.old, log.file)
        #print(dc)
        stopifnot (is.data.table(dc))
        cat("OK\n")
        ####################
        cat("TRANSFORMING...")
        #flow.transform()
        #stopifnot (is.data.table(dc))
        cat("OK\n")
        ####################
        cat("LOADING...")
        #flow.load(dc, file.out)
        #stopifnot (data.is.written.ok???)
        cat("OK\n")
    }
    ########################
    #on success {
    cat("COMMITTING...")
    #flow.commit()
    #stopifnot (commit.ok???)
    cat("OK\n")
    #}
}
