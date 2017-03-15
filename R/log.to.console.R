# Terminal output compare.datacubes

#output is cubename (reduced), status, substatus

#this function is based on the decode.compare.datacubes

log.to.console <- function(r, cubename = "cubename") {

    cubename_short <- stri_replace_first_regex(cubename, "^(.{13})....+(.{14})$", "$1...$2")

    # put information about new years
    info <- paste(r$elem[[r$timekey]]$add, collapse = ",")

    # # a$elem muss überschrieben werden in a$elem.Jahr und a$elem.rest$
    #
    #
    # # spalte für Jahr definieren
    # # nur wenn neues Jahr dazu kommt
    # if (exists("r$elem[[r$timekey]]$add")) {
    #     Jahr <- paste(r$elem[[r$timekey]]$add)
    # }
    # else {
    #     Jahr <- ""
    # }

    out.term <- sprintf("\r%-30s | %-7s | %-3s | %-17s | %-11s\n", cubename_short, r$status, r$code, r$subcode, info)

    if (r$code > 0) {
        cat(out.term)
    }
}
