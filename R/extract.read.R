extract.read <- function(resource.local, type) {
    dc <- as.data.table(read.px(resource.local))
    return(dc)
}
