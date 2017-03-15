log.via.email <- function(r, resource.name, log.email, lang) {

    if (r$code > 0) {
        subject <- paste("alert data.flow:", resource.name)

        body <- paste('<html><head></head><body style="font-family: Calibri,Arial,sans-serif;">',
                      "<h1>Resource:", resource.name, "</h1>",
                      "<h2>Status:", r$status, "</h2>",
                      paste(r$msg.html[[lang]]),
                      "<hr />", "</p>",
                      "<h3>Detail-Report</h3>",
                      "<p>Code:", r$code, "</p>",
                      "<p>Subcode:", paste0(r$subcodes, collapse = ", "), "</p>",
                      "<p>Elements.diff: ", collapse.changes.in.elem.per.dim(r$elem), "</p>",
                      "<p>max.rel.Change:", sprintf("%+.1f%%", r$max*100), "</p>",
                      "<p>Time.concerned:", paste(sort(r$changed[[r$timekey]]), collapse = ", "), "</p>",
                      "<p>Observations added:", r$add.lines, "</p>",
                      "<p>Observations dropped:", r$del.lines, "</p>",
                      "<p>Observations changed:", r$changed.lines, "</p>",
                      "<footer>May the flow be with you...</footer>",
                      "</body></html>")

        send.email(to = log.email, subject = subject, body = body, html = TRUE)

        # body <- paste("Resource:\t", resource.name, "</p>",
        #               "\n\nMessage:\t", paste(r$msg[[lang]]), "</p>",
        #               "\nStatus:\t", r$status, "</p>",
        #               "\nCode:\t", r$code, "</p>",
        #               "\nSubcode:\t", paste0(r$subcodes, collapse = ", "), "</p>",
        #               "\nElements.diff:\t", collapse.changes.in.elem.per.dim(r$elem), "</p>",
        #               "\nmax.rel.Change:\t", sprintf("%+.1f%%", r$max*100), "</p>",
        #               "\nTime.concerned:\t", paste(sort(r$changed[[r$timekey]]), collapse = ", "), "</p>",
        #               "\nObservations added:\t", r$add.lines, "</p>",
        #               "\nObservations dropped:\t", r$del.lines, "</p>",
        #               "\nObservations changed:\t", r$changed.lines, "</p>",
        #               "\n\nMay the flow be with you...")
        #
        # send.email(to = log.email, subject = subject, body = body)
    }
}
