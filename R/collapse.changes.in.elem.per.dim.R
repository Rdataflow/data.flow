### collapses hierachical list of element changes to string ###
# input<-list(Jahr=list(add=c(2014:2016),del=c(2009)),Forstzone=list(add=c("Bern","Zürich","Genf"),del=character()),Kanton=list(add=character(),del=character()))
# output<-"Jahr(+2014+2015+2016-2009)Forstzone(+Bern+Zürich+Genf)"
collapse.changes.in.elem.per.dim<-function(input, html = FALSE)
{
    s<-""
    for (i in seq_along(input))
    {
        ii<-input[[i]]
        if(length(ii$add)+length(ii$del)>0)
        {
            if (!html) {
                s<-append(s, c(names(input[i]), "("))
                if(length(ii$add)>0)
                    s<-append(s, paste0("+", ii$add, collapse=""))
                if(length(ii$del)>0)
                    s<-append(s, paste0("-", ii$del, collapse=""))
                s<-append(s, ")")
            }
            else {
                s<-append(s, c("<strong>", names(input[i]), "</strong>"))
                if(length(ii$add)>0)
                    s<-append(s, paste0("<br />+ ", ii$add, collapse=""))
                if(length(ii$del)>0)
                    s<-append(s, paste0("<br />- ", ii$del, collapse=""))
                s<-append(s, "<br />")
            }
        }
    }
    ret<-paste0(s,collapse="")
    return(ret)
}


# collapse.changes.in.elem.per.dim<-function(input) {
#     x<-list.map(input,paste0("(",paste0("+",add,collapse=""),paste0("-",del,collapse=""),")"))
#     x<-paste0(names(x),x,collapse="")
#     x<-gsub("+-","-",x)
#     x<-gsub("-)",")",x)
#     return(x)
# }

