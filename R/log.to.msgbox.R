# warning message box function
log.to.msgbox <- function(r, resource.name, lang) {

    if (r$status == 'ident' | r$status == 'info') {
        return()
    } else if (r$status == 'warning' | r$status == 'error') {

        icon_msg <- r$status

        msgBox <- tk_messageBox(title = r$status,
                                message = paste0(r$status,":  ", resource.name, r$msg[[lang]]),
                                icon = icon_msg,
                                type = "ok")

    } else {
        stop("invalid status")
    }
}
