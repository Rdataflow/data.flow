eval.compare.datacubes <- function(r) {

    ### status evaluation
    r$code <- sum(
        128 * r$subcodes["dimensions.differ"],
         64 * r$subcodes["elements.differ"],
         16 * r$subcodes["time.del"],
          8 * r$subcodes["big.change"],
          2 * r$subcodes["small.change"],
          1 * r$subcodes["time.add"],
        na.rm = TRUE
    )

    if (r$code >= 64) {
        r$status <- 'error'
    } else if (r$code >= 8) {
        r$status <- 'warning'
    } else if (r$code >= 1) {
        r$status <- 'info'
    } else {
        r$status <- 'ident'
    }

    ### messaging
    if (r$subcodes["dimensions.differ"]) {
        r$msg$de <- paste0(r$msg$de, paste0("\n\nDie Datenquelle weist neuerdings veränderte Dimensionen auf!!!",
                                            "\n  bisher: ", paste(r$dim$new, collapse = ", "),
                                            "\n  neu: ", paste(r$dim$old, collapse = ", ")))
        r$msg.html$de <- paste0(r$msg.html$de, paste0("<h2>Veränderte Dimensionen</h2>",
                                              "<p>Die Datenquelle weist neuerdings veränderte Dimensionen auf!!!",
                                              "<br />bisher: <ul>", paste("<li>", r$dim$new, "</li>", collapse = ", "), "</ul>",
                                              "<br />neu: <ul>", paste("<li>", r$dim$old, "</li>", collapse = ", "), "</ul>",
                                              "</p>"))
    }
    if (r$subcodes["elements.differ"]) {
        r$msg$de <- paste0(r$msg$de, paste0("\n\nDie Datenquelle weist neulich veränderte Elemente auf, ",
                                            "\nwelche nicht die Zeit betreffen!\n",
                                            collapse.changes.in.elem.per.dim(r$elem[names(r$elem) != r$timekey])))
        r$msg.html$de <- paste0(r$msg.html$de, paste0("<h2>Veränderte Elemente</h2>",
                                              "<p>Die Datenquelle weist neulich veränderte Elemente auf, ",
                                              "<br />welche nicht die Zeit betreffen!<br />",
                                              collapse.changes.in.elem.per.dim(r$elem[names(r$elem) != r$timekey], html = TRUE),
                                              "</p>"))
    }
    if (r$subcodes["time.del"]) {
        r$msg$de <- paste0(r$msg$de, paste0("\n\nDie Datenquelle führt die bisherige Zeitreihe nicht weiter...\n",
                                            paste0("-", r$elem[[r$timekey]]$del)))
        r$msg.html$de <- paste0(r$msg.html$de, paste0("<h2>Zeitreihe reduziert</h2>",
                                              "<p>Die Datenquelle führt die bisherige Zeitreihe nicht weiter...",
                                              paste0("<br />-", r$elem[[r$timekey]]$del),
                                              "</p>"))
    }
    if (r$subcodes["big.change"]) {
        r$msg$de <- paste0(r$msg$de, paste0("\n\nDie Datenquelle weist erhebliche Wertkorrekturen auf \n im Umfang von: ",
                                            sprintf("%+.1f%%", r$max*100)))
        r$msg.html$de <- paste0(r$msg.html$de, paste0("<h2>Erhebliche Wertkorrekturen</h2>",
                                              "<p>Die Datenquelle weist erhebliche Wertkorrekturen auf im Umfang von: <br />",
                                              sprintf("%+.1f%%", r$max*100),
                                              "</p>"))
    }
    if (r$subcodes["small.change"]) {
        r$msg$de <- paste0(r$msg$de, paste0("\n\nDie Datenquelle weist marginale Wertkorrekturen auf \n im Umfang von: ",
                                            sprintf("%+.1f%%", r$max*100)))
        r$msg.html$de <- paste0(r$msg.html$de, paste0("<h2>Marginale Wertkorrekturen</h2>",
                                              "<p>Die Datenquelle weist marginale Wertkorrekturen auf im Umfang von: <br />",
                                              sprintf("%+.1f%%", r$max*100),
                                              "</p>"))
    }
    if (r$subcodes["time.add"]) {
        r$msg$de <- paste0(r$msg$de, paste0("\n\nDie Datenquelle hat die Zeitreihe erweitert...\n",
                                            paste0("+", r$elem[[r$timekey]]$add)))
        r$msg.html$de <- paste0(r$msg.html$de, paste0("<h2>Zeitreihe erweitert</h2>",
                                              "<p>Die Datenquelle hat die Zeitreihe erweitert...",
                                              paste0("<br />+", r$elem[[r$timekey]]$add),
                                              "</p>"))
    }


    # filter elements of subcode on TRUE
    r$subcodes <- names(r$subcodes[which(r$subcodes)])
    r$subcode <- paste0(r$subcodes, collapse = "|")

    return(r)
}
