# write dataset to file
flow.load <- function(dc, file.output, output.type, val="value") {
    if (output.type == "csv") {
        fwrite(x = dc, file = file.output, sep=";", quote=TRUE, showProgress=FALSE)
    }
    if(output.type == "json") {
        json <- toJSONstat(dc, value = val, pretty = TRUE)
        write(x = json, file = file.output)
    }
}
