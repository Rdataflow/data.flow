#compares two datacubes and returns information on differences
compare.datacubes <- function(new, old, val=c("Wert","wert","Value","value"),
                              timekey=c("Jahr","jahr","Year","year"),
                              log.detail="details.log", warn.threshold = 0.01) {

    r<-list(timestamp=Sys.time())

    # define the subcode flags
    r$subcodes <- c()
    r$subcodes["identical"] <- FALSE
    r$subcodes["dimensions.differ"] <- FALSE
    r$subcodes["elements.differ"] <- FALSE
    r$subcodes["time.add"] <- FALSE
    r$subcodes["time.del"] <- FALSE
    r$subcodes["big.change"] <- FALSE
    r$subcodes["small.change"] <- FALSE


    if (identical(new,old)) {
        ### identical datasets
        r$subcodes["identical"] <- TRUE
    }
    else {
        if (!identical(names(new), names(old))) {
            ### different dimensions!!!
            r$dim$new <- names(new)
            r$dim$old <- names(old)
            r$subcodes["dimensions.differ"] <- TRUE
        }
        else {
            ### same dimensions...
            r$val<-intersect(names(new),val)
            r$key<-setdiff(names(new),r$val)
            r$timekey<-intersect(r$key,timekey)
            val.new<-paste0(r$val,".new")
            val.old<-paste0(r$val,".old")

            ### checking for differnt elements ocurring
            for (i in r$key) {
                r$elem[[i]]$add <- setdiff(new[[i]], old[[i]])
                r$elem[[i]]$del <- setdiff(old[[i]], new[[i]])
            }
            r$elem <- list.clean(r$elem, function(x) length(x)==0L, recursive = TRUE)

            if (length(setdiff(names(r$elem), r$timekey)) > 0) {
                r$subcodes["elements.differ"] <- TRUE
            }

            if ("del" %in% names(r$elem[[r$timekey]])) {
                r$subcodes["time.del"] <- TRUE
            }

            if ("add" %in% names(r$elem[[r$timekey]])) {
                r$subcodes["time.add"] <- TRUE
            }


            ### checking for different values (comparision by line)
            setkeyv(new, names(new)) # needed as a workaround due to odd crashing behaviour w/o keys
            setkeyv(old, names(old))
            newlines <- fsetdiff(new, old)[, na:=is.na(r$val)]
            oldlines <- fsetdiff(old, new)[, na:=is.na(r$val)]

            if (nrow(newlines) + nrow(oldlines) == 0) {
                r$subcodes["identical"] <- TRUE
                return(r)
            }

            delta <- merge(newlines, oldlines, by=r$key, suffixes = c(".new", ".old"), all = TRUE)

            add.lines <- delta[is.na(na.old)][, c("na.new", "na.old") := NULL]
            r$add.lines <- nrow(add.lines)

            del.lines <- delta[is.na(na.new)][, c("na.new", "na.old") := NULL]
            r$del.lines <- nrow(del.lines)

            changed.lines <- delta[!is.na(na.new) & !is.na(na.old)][, c("na.new","na.old") := NULL]
            changed.lines[, rel.change := (changed.lines[[val.new]] / changed.lines[[val.old]] - 1)][rel.change == Inf, rel.change := NA]
            r$changed.lines <- nrow(changed.lines)


            if (r$changed.lines > 0) {
                r$changed <- changed.lines[, .N, by=c(r$timekey[1])]
                names(r$changed) <- c(r$timekey[1], "count.per.time")

                if (!is.na(max(changed.lines$rel.change))) {

                    r$max <- changed.lines$rel.change[which.max(abs(changed.lines$rel.change))]

                    if (abs(r$max) > warn.threshold) {
                        r$subcodes["big.change"] <- TRUE
                    } else {
                        r$subcodes["small.change"] <- TRUE
                    }
                }

                # write diff details to dedicated logfile
                fwrite(changed.lines[, timestamp := r$timestamp], sep=";", quote=TRUE, showProgress=FALSE, append=TRUE, file=log.detail)
            }
        }
    }

    return(r)
}
