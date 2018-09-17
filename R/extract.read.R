# reads files and returns datacube as data.table
# file.input <- "/path/to/filename"
# type <- "px" OR "csv" (datatype)
extract.read <- function(file.input, type) {
    if(type=="px") {
        dc <- fread.px(file.input)
        # dc <- read.px(file.input, na.strings = c('"."','".."','"..."','"...."','"....."','"......"','":"'))
        # dc$DATA$value <- as.data.table(dc)
        # for (i in seq(names(dc$DATA$value))) {
        #     if (is.character(levels(dc$DATA$value[[i]]))) {
        #         levels(dc$DATA$value[[i]]) <- enc2native(levels(dc$DATA$value[[i]]))
        #     }
        # }
    }

    if (type == "csv") {
        dc <- list()
        dc$DATA$value <- fread(file.input, showProgress = FALSE)
        dc$DATA$value$Value <- as.numeric(dc$DATA$value$Value)
    }

    # replace NA values with empty
    # dc$DATA$value[is.na(dc)] <- ""

    return(dc)
}
