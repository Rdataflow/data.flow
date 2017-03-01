.test_flow<-function() {




resource1 <- as.data.table(list(name = "ch.admin.bfs.forststat.holzprod", url = "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-0703010000_102",
                                type = "px"))

flow(resources = resource1, dir.commit = "/tmp/data", dir.output = "/tmp/data_staging/",
     dir.input = "/tmp/data_input/", dir.input.old = "/tmp/data_input.old/", dir.log = "/tmp/data_log/", output.type = "csv", log.file = "/tmp/data_log/data.flow.summary.log")


}
