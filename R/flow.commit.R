flow.commit <- function(resources.output, resources.commit, log.file) {

    cat("\nCOMMITTING all data ... ")

    file.rename(from = resources.output, to = resources.commit)

    cat("done\n")
    cat(paste0("===== committed sucessfully =====\n"), file = log.file, append = TRUE)
    #msgBox <- tk_messageBox(title = "SUCCESS", message = "data flow committed succesfully!", icon = "info", type = "ok")

}
