# reads files and returns datacube as data.table
# file.input <- "/path/to/filename"
# type <- "px" OR "csv" (datatype)
extract.read <- function(file.input, type) {
    if(type=="px") {
        dc <- as.data.table(read.px(file.input, na.strings = c('"."','".."','"..."','"...."','"....."','"......"','":"')))
        for (i in seq(names(dc))) {
            if (is.character(levels(dc[[i]]))) {
                levels(dc[[i]]) <- enc2native(levels(dc[[i]]))
            }
        }
    }

    if (type == "csv") {
        dc <- fread(file.input, showProgress = FALSE)
    }

    # replace NA values with empty
    # dc[is.na(dc)] <- ""

    return(dc)
}
