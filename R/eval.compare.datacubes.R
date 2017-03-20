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
        r$status.html <- '<span style="color:#cc0000;">error</span>'
    } else if (r$code >= 8) {
        r$status <- 'warning'
        r$status.html <- '<span style="color:#ff8800;">warning</span>'
    } else if (r$code >= 1) {
        r$status <- 'info'
        r$status.html <- '<span style="color:#008800;">info</span>'
    } else {
        r$status <- 'ident'
        r$status.html <- '<span style="color:#222222;">ident</span>'
    }

    ### messaging
    if (r$subcodes["dimensions.differ"]) {
        r$msg$de <- paste0(r$msg$de, paste0("\n\nDie Datenquelle weist neuerdings veränderte Dimensionen auf!!!",
                                            "\n  bisher: ", paste(r$dim$new, collapse = ", "),
                                            "\n  neu: ", paste(r$dim$old, collapse = ", ")))
        r$msg.html$de <- paste0(r$msg.html$de, paste0("<h3>Veränderte Dimensionen</h3>",
                                              "<p>Die Datenquelle weist neuerdings veränderte Dimensionen auf!!!",
                                              "<br /><strong>bisher:</strong> ", paste("<br />- ", r$dim$new, collapse = ""),
                                              "<br /><strong>neu:</strong> ", paste("<br />- ", r$dim$old, collapse = ""),
                                              "</p>"))
    }
    if (r$subcodes["elements.differ"]) {
        r$msg$de <- paste0(r$msg$de, paste0("\n\nDie Datenquelle weist neulich veränderte Elemente auf, ",
                                            "\nwelche nicht die Zeit betreffen!\n",
                                            collapse.changes.in.elem.per.dim(r$elem[names(r$elem) != r$timekey])))
        r$msg.html$de <- paste0(r$msg.html$de, paste0("<h3>Veränderte Elemente</h3>",
                                              "<p>Die Datenquelle weist neulich veränderte Elemente auf, ",
                                              "<br />welche nicht die Zeit betreffen!<br />",
                                              collapse.changes.in.elem.per.dim(r$elem[names(r$elem) != r$timekey], html = TRUE),
                                              "</p>"))
    }
    if (r$subcodes["time.del"]) {
        r$msg$de <- paste0(r$msg$de, paste0("\n\nDie Datenquelle führt die bisherige Zeitreihe nicht weiter...\n",
                                            paste0("- ", r$elem[[r$timekey]]$del)))
        r$msg.html$de <- paste0(r$msg.html$de, paste0("<h3>Zeitreihe reduziert</h3>",
                                              "<p>Die Datenquelle führt die bisherige Zeitreihe nicht weiter...",
                                              paste0("<br />- ", r$elem[[r$timekey]]$del),
                                              "</p>"))
    }
    if (r$subcodes["big.change"]) {
        r$msg$de <- paste0(r$msg$de, paste0("\n\nDie Datenquelle weist erhebliche Wertkorrekturen auf ",
                                            "\nim Umfang von: ", sprintf("%+.1f%%", r$max*100),
                                            "\nbetreffend die Jahre: ", paste(sort(r$changed[[r$timekey]]), collapse = ", ")))
        r$msg.html$de <- paste0(r$msg.html$de, paste0("<h3>Erhebliche Wertkorrekturen</h3>",
                                              "<p>Die Datenquelle weist erhebliche Wertkorrekturen auf",
                                              "<br />im Umfang von: <strong>", sprintf("%+.1f%%", r$max*100), "</strong>",
                                              "<br />betreffend die Jahre: <strong>", paste(sort(r$changed[[r$timekey]]), collapse = ", "), "</strong>",
                                              "</p>"))
    }
    if (r$subcodes["small.change"]) {
        r$msg$de <- paste0(r$msg$de, paste0("\n\nDie Datenquelle weist marginale Wertkorrekturen auf ",
                                            "\nim Umfang von: ", sprintf("%+.1f%%", r$max*100),
                                            "\nbetreffend die Jahre: ", paste(sort(r$changed[[r$timekey]]), collapse = ", ")))
        r$msg.html$de <- paste0(r$msg.html$de, paste0("<h3>Marginale Wertkorrekturen</h3>",
                                              "<p>Die Datenquelle weist marginale Wertkorrekturen auf",
                                              "<br />im Umfang von: <strong>", sprintf("%+.1f%%", r$max*100), "</strong>",
                                              "<br />betreffend die Jahre: <strong>", paste(sort(r$changed[[r$timekey]]), collapse = ", "), "</strong>",
                                              "</p>"))
    }
    if (r$subcodes["time.add"]) {
        r$msg$de <- paste0(r$msg$de, paste0("\n\nDie Datenquelle hat die Zeitreihe erweitert...\n",
                                            paste0("+ ", r$elem[[r$timekey]]$add)))
        r$msg.html$de <- paste0(r$msg.html$de, paste0("<h3>Zeitreihe erweitert</h3>",
                                              "<p>Die Datenquelle hat die Zeitreihe erweitert...",
                                              paste0("<br />+ ", r$elem[[r$timekey]]$add),
                                              "</p>"))
    }


    # filter elements of subcode on TRUE
    r$subcodes <- names(r$subcodes[which(r$subcodes)])
    r$subcode <- paste0(r$subcodes, collapse = "|")

    return(r)
}
