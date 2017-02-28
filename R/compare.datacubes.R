compare.datacubes <- function(new, old, val=c("Wert","wert","Value","value"),
                              timekey=c("Jahr","jahr","Year","year"), log.detail="datacubes_details.log") {
    retval<-list(timestamp=Sys.time())

    if(identical(new,old))
    {
        ### identische Datacubes
        cat("Neuer Datensatz ist identisch zum bisherigen.\n")
        retval$status<-0
        retval$level<-"ID"
        retval$title<-"NOCHANGE"
        retval$msg<-"Datensätze sind identisch."
    }
    else
    {
        ### unterschiedliche Datacubes
        cat("Neuer Datensatz unterscheidet sich vom bisherigen.\n")

        if(identical(names(new),names(old))==FALSE)
        {
            ### unterschiedliche Dimensionen
            cat("Dimensionen unterscheiden sich!")
            cat("\nDimensionen bisher:\n")
            cat(names(old))
            cat("\nDimensionen neu:\n")
            cat(names(new))
            cat("\n")
            retval$status<-99
            retval$level<-"ERROR"
            retval$title<-"ERROR"
            retval$msg<-"Datensätze haben unterschiedliche Dimensionen."
        }
        else
        {
            ### identische Dimensionen
            cat("Dimensionen sind identisch, ihre Wertebereiche unterscheiden sich jedoch.\n")
            val<-intersect(names(new),val)
            newvalkey<-paste0(val,".new")
            oldvalkey<-paste0(val,".old")
            key<-setdiff(names(new),val)
            timekey<-intersect(key,timekey)
            retval$timekey<-timekey

            for (i in key)
            {
                ### je Dimension (d.h. ohne Werte): Veränderungen prüfen
                retval$elem[[i]]$add<-setdiff(new[[i]],old[[i]])
                retval$elem[[i]]$del<-setdiff(old[[i]],new[[i]])
            }
            retval$elem <- list.clean(retval$elem, function(x) length(x)==0L, recursive = TRUE)


            ### Wert-Differenzen prüfen (d.h. gesamte Daten-Zeilen)
            newlines<-fsetdiff(new,old)[,na:=is.na(value)]
            oldlines<-fsetdiff(old,new)[,na:=is.na(value)]
            delta<-merge(newlines,oldlines,by=key,suffixes=c(".new",".old"),all=TRUE)

            add.lines<-delta[is.na(na.old)][,c("na.new","na.old"):=NULL]
            del.lines<-delta[is.na(na.new)][,c("na.new","na.old"):=NULL]
            changed.lines<-delta[!is.na(na.new)&!is.na(na.old)][,c("na.new","na.old"):=NULL]
            changed.lines[,rel.change:=(changed.lines[[newvalkey]]/changed.lines[[oldvalkey]]-1)]


            delta[,rel.change:=(delta[[newvalkey]]/delta[[oldvalkey]]-1)]
            #cat(print(delta[]))

            ### Ausgabe Anzahl Änderungen pro Jahr
            summary.changed<-changed.lines[,.N,by=timekey]
            if (nrow(changed.lines)>0)
            {
                names(summary.changed)<-c(timekey,"Anzahl_diff")
                retval$changed<-summary.changed
                retval$max<-changed.lines$rel.change[which.max(abs(changed.lines$rel.change))]
                retval$max<-sprintf("%+.1f%%",retval$max*100)
                # cat("\n>> Geändert:\n")
                # cat(print(summary.changed))
                # cat("\nMaximale relative Änderung:\t" ,sprintf("%+f",retval$max*100),"%\n")

                ### Write details to logfile
                now<-Sys.time()
                fwrite(changed.lines[,timestamp:=now], sep=";", quote=TRUE, showProgress=FALSE, append=TRUE, file=log.detail)
                #fwrite(delta[,timestamp:=now], sep=";", quote=TRUE, showProgress=FALSE, append=TRUE, file=log.detail)
                #cat("1, CHANGED: Datensätze haben unterschiedliche Werte.\n \n \n")
            }
            retval$status<-1
            retval$level<-"TODO: delta INFO ... or ... WARN ..."
            retval$title<-"CHANGED"
            retval$msg<-"Datensätze haben unterschiedliche Werte."
        }
    }
    return(retval)
}
