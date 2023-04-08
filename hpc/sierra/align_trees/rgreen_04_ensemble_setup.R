#!/usr/bin/env Rscript

m <- seq(10,5300,10)
ensemble_id <- 1:30

vals <- list()
count <- 1

for (i in 1:length(ensemble_id)){
    for (j in 1:length(m)){
        vals[[count]] <- c(ensemble_id[i], m[j])
        count <- count + 1
    }  
}

vals <- do.call(rbind, vals)

write.table(vals, "ensemble_inputs.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)
