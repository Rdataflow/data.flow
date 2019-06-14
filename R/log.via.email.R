log.via.email <- function(r, resource.name, log.email, lang) {

    if (r$code > 0) {
        subject <- paste("data.flow:", resource.name, "-", r$status)
        host <- system2("hostname")

        body <- paste('<html><head></head><body style="font-family: Calibri,Arial,sans-serif;">',
                      "<h2>Resource:", resource.name, "</h2>",
                      "<h3>Status:", r$status.html, "</h3>",
                      paste(r$msg.html[[lang]]),
                      "<h4>Host:", system2("hostname", stdout = TRUE), "</h4>",
                      "<hr />", "</p>",
                      "<h4>Detail-Report</h4>",
                      "<p>Code:", r$code, "</p>",
                      "<p>Subcode:", paste0(r$subcodes, collapse = ", "), "</p>",
                      # "<p>Elements.diff: ", collapse.changes.in.elem.per.dim(r$elem), "</p>",
                      # "<p>max.rel.Change:", sprintf("%+.1f%%", r$max*100), "</p>",
                      # "<p>Time.concerned:", paste(sort(r$changed[[r$timekey]]), collapse = ", "), "</p>",
                      "<p>Observations added:", r$add.lines, "</p>",
                      "<p>Observations dropped:", r$del.lines, "</p>",
                      "<p>Observations changed:", r$changed.lines, "</p>",
                      "<footer>May the flow be with you...</footer>",
                      "</body></html>")

        log.attach <- tempfile(pattern = paste0(resource.name, "__"), fileext = ".csv")
        fwrite(r$out.log, file = log.attach, quote = TRUE, sep = ";", showProgress = FALSE)


        send.email(to = log.email, subject = subject, body = body, files = c(log.attach), html = TRUE)

        # body <- paste("Resource:\t", resource.name, "</p>",
        #               "\n\nMessage:\t", paste(r$msg[[lang]]), "</p>",
        #               "\nStatus:\t", r$status, "</p>",
        #               "\nHost:\t", system2("hostname"),
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
