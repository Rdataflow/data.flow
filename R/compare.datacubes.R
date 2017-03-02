#compares two datacubes and returns information on differences
compare.datacubes <- function(new, old, val=c("Wert","wert","Value","value"),
                              timekey=c("Jahr","jahr","Year","year"), log.detail="datacubes_details.log") {
    r<-list(timestamp=Sys.time())

    if(identical(new,old)) {
        ### identical datasets
        cat("identical datasets... ")
        r$status<-0
        r$level<-"ID"
        r$title<-"NOCHANGE"
        r$msg$de<-"Datensätze sind identisch."
    }
    else {
        if(identical(names(new),names(old))==FALSE) {
            ### different dimensions!!!
            cat("different dimensions!!!")
            # cat("old:", names(old))
            # cat("new:", names(new))
            r$status<-99
            r$level<-"ERROR"
            r$title<-"ERROR"
            r$msg$de<-"Datensätze haben unterschiedliche Dimensionen."
        }
        else {
            ### same dimensions...
            cat("same dimensions... ")
            r$val<-intersect(names(new),val)
            r$key<-setdiff(names(new),r$val)
            r$timekey<-intersect(r$key,timekey)
            val.new<-paste0(r$val,".new")
            val.old<-paste0(r$val,".old")

            # checking for differnt elements ocurring
            for (i in r$key) {
                ### je Dimension (d.h. ohne Werte): Veränderungen prüfen
                r$elem[[i]]$add<-setdiff(new[[i]],old[[i]])
                r$elem[[i]]$del<-setdiff(old[[i]],new[[i]])
            }
            r$elem <- list.clean(r$elem, function(x) length(x)==0L, recursive = TRUE)
            if (length(r$elem)>0) {
                cat("different elements... ")
            }

            # checking fo different values (comparision by line)
            newlines<-fsetdiff(new,old)[,na:=is.na(value)]
            oldlines<-fsetdiff(old,new)[,na:=is.na(value)]
            delta<-merge(newlines,oldlines,by=r$key,suffixes=c(".new",".old"),all=TRUE)

            add.lines<-delta[is.na(na.old)][,c("na.new","na.old"):=NULL]
            del.lines<-delta[is.na(na.new)][,c("na.new","na.old"):=NULL]
            changed.lines<-delta[!is.na(na.new)&!is.na(na.old)][,c("na.new","na.old"):=NULL]
            changed.lines[,rel.change:=(changed.lines[[val.new]]/changed.lines[[val.old]]-1)][rel.change==Inf,rel.change:=NA]


            ### Ausgabe Anzahl Änderungen pro Jahr
            summary.changed<-changed.lines[, .N, by=c(r$timekey)]
            if (nrow(changed.lines)>0) {
                names(summary.changed)<-c(r$timekey,"count_diff")
                r$changed<-summary.changed
                r$max<-changed.lines$rel.change[which.max(abs(changed.lines$rel.change))]

                # write diff details to dedicated logfile
                now<-Sys.time()
                fwrite(changed.lines[,timestamp:=now], sep=";", quote=TRUE, showProgress=FALSE, append=TRUE, file=log.detail)
                cat("different values...")
            }

            r$status<-1
            r$level<-"TODO: delta INFO ... or ... WARN ..."
            r$title<-"CHANGED"
            r$msg$de<-"Datensätze haben unterschiedliche Werte."
        }
    }
    return(r)
}
