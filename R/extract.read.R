# reads files and returns datacube as data.table
# file.input <- "/path/to/filename"
# type <- "px" OR "csv" (datatype)
extract.read <- function(file.input, type) {
    if(type=="px") {
        dc <- read.px(file.input, na.strings = c('"."','".."','"..."','"...."','"....."','"......"','":"'))
        dc$DATA <- as.data.table(dc)
        for (i in seq(names(dc$DATA))) {
            if (is.character(levels(dc$DATA[[i]]))) {
                levels(dc$DATA[[i]]) <- enc2native(levels(dc$DATA[[i]]))
            }
        }
    }

    if (type == "csv") {
        dc <- list()
        dc$DATA <- fread(file.input, showProgress = FALSE)
    }

    # replace NA values with empty
    # dc[is.na(dc)] <- ""

    return(dc)
}
