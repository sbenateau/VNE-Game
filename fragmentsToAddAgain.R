dplyr::summarise_at(.vars = correspond(Parameters[[2]], EquivalenceVar), .funs = c("mean","se"))
-            colnames(Results[[i]])[which(colnames(Results[[i]]) == "mean")]  <- correspond(Parameters[[2]], EquivalenceVar)
-          } else if (Parameters[[3]] == "Me"){
-            #https://tbradley1013.github.io/2018/10/01/calculating-quantiles-for-groups-with-dplyr-summarize-and-purrr-partial/

dplyr::summarise_at(.vars = correspond(Parameters[[2]], EquivalenceVar), .funs = "quantile")
-            colnames(Results[[i]])[which(colnames(Results[[i]]) == "50%")]  <- correspond(Parameters[[2]], EquivalenceVar)

dplyr::summarise_at(.vars = correspond(Parameters[[2]], EquivalenceVar), .funs = "quantile")
-            colnames(Results[[i]])[which(colnames(Results[[i]]) == "50%")]  <- correspond(Parameters[[2]], EquivalenceVar)
