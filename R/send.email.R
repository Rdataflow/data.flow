send.email <- function(to, subject, body, files = NULL, html = FALSE) {

    if (Sys.info()[["sysname"]] == "Windows") {

        library(RDCOMClient)
        ## init com api
        OutApp <- COMCreate("Outlook.Application")
        ## create an email
        outMail = OutApp$CreateItem(0)
        ## configure  email parameter
        outMail[["To"]] = to
        outMail[["subject"]] = subject
        if (!html) {
            outMail[["body"]] = body
        }
        else {
            outMail[["BodyFormat"]] <- 2
            outMail[["HTMLbody"]] <- body
        }
        for (file in files) {
            outMail[["Attachments"]]$Add(file)
        }

        ### send mail
        outMail$Send()
    }
}
