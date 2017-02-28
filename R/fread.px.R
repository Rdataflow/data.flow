###############################################
#                                             #
# Copyright by the authors of package pxR     #
#       Carlos J. Gil Bellosta [cre, aut],    #
#       Francisco J. Viciana [aut],           #
#       Oscar Perpinan Lamigueiro [aut],      #
#       Emilio Torres Manzanera [ctb]         #
#                                             #
#                                             #
#   >>>>>>  License:    GPL-3   <<<<<<<<      #
#                                             #
###############################################
#
#  depends on packages:
#   - library(data.table)
#   - library(plyr)
#   - library(stringi)
#
#  fast implementation of read.px
fread.px <- function(filename, from.encoding = "latin1", to.encoding="", na.strings = c('"."', '".."', '"..."', '"...."', '"....."', '"......"', '":"')) {

    fscan=function(fname) {
        s = file.info(fname)$size
        buf = readChar(fname, s, useBytes = TRUE)
    }

    get.attributes <- function(x) {
        x <- stri_replace_first_regex(x, "([A-Z-]*)\\((.*)\\).*", "$1;$2" )  ## separates label-attribute with ";"
        x <- ldply(stri_split_fixed(x, ";"),
                   function(y) c(y, "value")[1:2])
    }

    break.clean <- function(x) {
        x <- stri_trim(stri_split_fixed(x, '\"', simplify = TRUE) )          ## breaks by '"'
        x[! x %in% c("," , "")]                                              ## and drops spurious seps
    }

    # read whole file and clean trailing spaces and ';' and line breaks
    a <- fscan(filename)
    Encoding(a) <- from.encoding
    a <- stri_trim_right(a)
    a <- stri_replace_last_fixed(a, ";", "")
    a <- stri_replace_all_regex(a, '[\\r\\n]', "")

    # determine position of ';' and '"'
    semicolon <- stri_locate_all_fixed(a, ";")[[1]][,1]
    quotation <- stri_locate_all_fixed(a, '"')[[1]][,1]

    # take ';' only after even numbers of '"' -- these are the proper cuts
    cuts <- Filter( function(x) sum(quotation < x) %% 2 == 0, semicolon )

    # vector of cutten strings inbetween ';' & clean up
    a <- stri_sub(a, c(1, cuts + 1), c(cuts - 1, stri_length(a)))
    a <- a[!is.na(a)]
    a <- a[a != ""]

    # divide into cols by '=' -- label(incl. attr) and value
    a <- do.call(rbind, stri_split_fixed(a, "=", n = 2))

    # extract attribute from label -- resulting in 3 cols -- label, attr, value
    a <- data.frame(cbind(get.attributes(a[, 1]), a[, 2], stringsAsFactors = FALSE))
    colnames(a) <- c("label", "attribute", "value")

    # make proper names
    a$label     <- make.names(stri_trim(a$label))
    a$attribute <- make.names(stri_trim(stri_replace_all_fixed(a$attribute, '\"', "")))

    # cleanup leading / trailing '"' among values - except for DATA part [data part might end in "..."]
    a.data                     <- a[a$label == "DATA", "value"]
    a.value                    <- stri_replace_all_regex(a$value, '^\\"|\\"$', "")   # removes " at beginning / end
    a.value[a$label == "DATA"] <- a.data

    ## build a px object: list with px class attribute ##
    names(a.value)             <- a$attribute
    px <- tapply(a.value, a$label, as.list)


    ## these metadata keys contain vectors (comma separated)
    ## we need to split them (and clean the mess: extra spaces, etc.)
    px$STUB$value    <- if(!is.null(px$STUB))    make.names(break.clean(px$STUB$value))
    px$HEADING$value <- if(!is.null(px$HEADING)) make.names(break.clean(px$HEADING$value))

    px$VALUES <- lapply(px$VALUES, break.clean)

    if (!is.null(px$CODES))
        px$CODES <- lapply(px$CODES, break.clean)

    # tweak to return specified encoding to be consumed correctly by fwrite
    px <- lapply(px, function(x) lapply(x, function(y) if(is.character(y)) enc2native(y)))
    px <- lapply(px, function(x) lapply(x, function(y) if(is.character(y)) iconv(y, to = to.encoding)))

    # tweak to return specified encoding to be consumed correctly by fwrite
    for (i in seq(px)) {
        names(px[[i]]) <- iconv(names(px[[i]]), to = to.encoding)
    }

    #### read the data part into a molten datatable ###

    ## there are two cases: files with/without KEYS keyword
    ## which need to be processed independently
    # if ("KEYS" %in% a$label ){
    #
    #     ## read the whole block
    #     tc <- textConnection(px$DATA$value); on.exit( close(tc) )
    #     raw <- read.table(tc, sep = ",", colClasses = "factor")
    #
    #     ## extract and process the data part (the numbers)
    #     data.part <- as.character(raw[, ncol(raw)] )                            # numbers (last column of the data.frame)
    #     data.part <- stri_replace_all_fixed(data.part, '"-"', 0)                # 0's might be encoded as "-"
    #     data.part <- scan(text = data.part, na.strings = na.strings, quiet = T)
    #
    #     ## extract and process the keys part (it needs to be staked a number of times,
    #     ##  as many as there are entries in the data vector in each row in the block)
    #     keys.part <- raw[, -ncol(raw), drop = FALSE]
    #     keys.part <- keys.part[ rep(1:nrow(keys.part), each = length(data.part) / nrow(keys.part) ), , drop = FALSE ]
    #     colnames(keys.part) <- names(px$KEYS)
    #
    #     ## change CODES (if any) in keys part to VALUES (consistency issue)
    #     # for (col.name in colnames(keys.part)[unlist(px$KEYS) == "CODES"])
    #     #  keys.part[[col.name]] <- mapvalues(keys.part[[col.name]],
    #     #                                     from = px$CODES[[col.name]],
    #     #                                     to   = px$VALUES[[col.name]])
    #     # fvf.20141222:
    #     for (col.name in colnames(keys.part)){
    #         if (px$KEYS[[col.name]] == 'CODES')   {
    #             keys.part[[col.name]]  <- factor(keys.part[[col.name]], levels = px$CODES[[col.name]])
    #             levels(keys.part[[col.name]]) <- px$VALUES[[col.name]]  ## all levels a VALUES
    #         } else  keys.part[[col.name]]  <- factor(keys.part[[col.name]], levels = px$VALUES[[col.name]] )
    #     }
    #
    #
    #     ## extract and process the variables that are not keys
    #     no.keys.part <- px$VALUES[setdiff(names(px$VALUES), names(px$KEYS))]
    #     no.keys.part <- expand.grid(rev(no.keys.part))
    #
    #     ## put everything together & cleanup
    #     px$DATA$value <- data.frame( keys.part,
    #                                  no.keys.part,
    #                                  value = data.part,
    #                                  row.names = NULL)
    # } else {

        # vector of numeric data values, incl. NA -- 0 can be encoded as "-"
        data.values <- stri_replace_all_fixed(px$DATA$value, '"-"', 0)
        data.values <- stri_split_fixed(data.values, " ")[[1]]
        data.values <- type.convert(data.values, as.is = TRUE, na.strings = na.strings)

        # prepare datatable trough cross-joining the variables
        names.vals <- c( rev(px$HEADING$value), rev( px$STUB$value ) )
        output.grid <- do.call(CJ,c(rev(px$VALUES[names.vals]),sorted=F))[]
        setcolorder(output.grid,length(px$VALUES[names.vals]):1)

        # sanity check: length of variables an values match? - to avoid the problem of "reclycling" of values
        if (nrow(output.grid) != length(data.values))
            stop( "The input file is malformed: data and varnames length differ" )

        # add data vales to cross-joined table of variables
        output.grid[, value := data.values]
        colnames(output.grid) <- c(names.vals, "value")
        px$DATA$value          <- output.grid

        # tweak to return specified encoding to be consumed correctly by fwrite
        cnames <- names(px$DATA$value)
        cnames <- enc2native(cnames)
        cnames <- iconv(cnames,to=to.encoding)
        Encoding(names(px$DATA$value))<-to.encoding
        names(px$DATA$value) <- cnames

    # }

    class(px) <- "px"
    px
}
