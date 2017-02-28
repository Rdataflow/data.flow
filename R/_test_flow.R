.test_flow<-function(){




resource1 <- as.data.table(list(name = "ch.admin.bfs.forststat-holzprod", url = "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-0703010000_102",
                                type = "px"))

flow(resource1)


}
