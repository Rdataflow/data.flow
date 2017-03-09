# write dataset to file
flow.load <- function(dc, file.output, output.type) {
    if (output.type == "csv") {
        fwrite(x = dc$DATA$value, file = file.output, sep=";", quote=TRUE, showProgress=FALSE)
    }
    else if(output.type == "json") {
        json <- toJSONstat(dc$DATA$value, value = dc$DSD$val, pretty = TRUE)
        write(x = json, file = file.output)
    }
    else {
        stop("invalid output format: ", output.type)
    }
    stopifnot(file.exists(file.output))
}
