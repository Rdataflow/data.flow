flow.commit <- function(resources.output, resources.commit, log.file, dir.commit, dir.input.old, git.exe) {

    cat("\rCOMMITTING all data ...                                                    \n\n")

    ret <- file.rename(from = resources.output, to = resources.commit)
    stopifnot(ret)

    if (file.exists(git.exe)) {
        # backup history of dir.input.old
        setwd(dir.input.old)
        if (!dir.exists(paste0(dir.input.old,"/.git"))) {
            system2(git.exe, "init")
        }
        system2(git.exe, "add .")
        system2(git.exe, paste0("commit -a -m ", Sys.Date()))

        # backup history of dir.commit
        setwd(dir.commit)
        if (!dir.exists(paste0(dir.commit,"/.git"))) {
            system2(git.exe, "init")
        }
        system2(git.exe, "add .")
        system2(git.exe, paste0("commit -a -m ", Sys.Date()))
        system2(git.exe, "log -1")
    }

    cat("\n=== COMMITTED SUCCESSFULLY ===                                             \n")
    cat("dataflow loaded in :", dir.commit, "\n\n")

    l <- data.table('Timestamp'=Sys.time(), 'csv_name'="ALL", 'Status'="commit", 'Code'="-1")
    fwrite(l, file = log.file, append = TRUE, sep = ";", quote = TRUE)


    #msgBox <- tk_messageBox(title = "SUCCESS", message = "data flow committed succesfully!", icon = "info", type = "ok")

}
