# init and check everything to start flow
flow.init <- function(resources, dir.commit, dir.output, dir.input, dir.input.old, dir.log, output.type, log.file) {

    cat("\n=== ", nrow(resources), "resources to flow  ===\n")

    ### dir.xyz exists ??? - else dir.create
    for (i in c(dir.commit, dir.output, dir.input, dir.input.old, dir.log)) {
        if (!dir.exists(i)) {
            dir.create(i)
        }
    }

    ### init resource table with dir.xyz
    resources[, input := paste0(dir.input, "/", resources$name, ".", resources$type)]
    resources[, input.old := paste0(dir.input.old, "/", resources$name, ".", resources$type)]
    resources[, output := paste0(dir.output, "/", resources$name, ".", output.type)]
    resources[, commit := paste0(dir.commit, "/", resources$name, ".", output.type)]
    resources[, log := paste0(dir.log, "/", resources$name, ".log")]

    return(resources)
}
