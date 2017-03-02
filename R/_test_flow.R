.test_flow <- function() {

    resource1 <-
        as.data.table(
            list(
                name = "ch.admin.bfs.forststat.holzprod",
                url = "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-0703010000_102",
                type = "px"
            )
        )

    OS <- Sys.info()[["sysname"]]
    if (OS == "Windows") {
        dir.commit <- "C:/temp/daten/"
        dir.output <- "c:/temp/data_staging/"
        dir.input <- "C:/temp/data_input/"
        dir.input.old <- "C:/temp/data_input.old/"
        dir.log <- "C:/temp/data_log/"
        log.file <- "C:/temp/data_log/data.flow.summary.log"
    } else {
        dir.commit <- "/tmp/data/"
        dir.output <- "/tmp/data_staging/"
        dir.input <- "/tmp/data_input/"
        dir.input.old <- "/tmp/data_input.old/"
        dir.log <- "/tmp/data_log/"
        log.file <- "/tmp/data_log/data.flow.summary.log"
    }
    output.type <- "csv"
    flow(resources = resource1, dir.commit, dir.output, dir.input, dir.input.old, dir.log, output.type, log.file)

}
