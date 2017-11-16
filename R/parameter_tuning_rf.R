# Plot relationship betwen AUC and each parameter

parameter_tuning <- parameterTuneRF(ntree = c(10, 50, 100, 500),
                                   maxnodes = c(10, 50, 100),
                                   mtry = c(1, 2, 3))

# convert the entries of the parameter tuning df to numeric
parameter_tuning <- data.frame(sapply(parameter_tuning,
                                      function(x) as.numeric(as.character(x))))

grid.arrange(plotParameterTuning("ntree"),
             plotParameterTuning("maxnodes"),
             plotParameterTuning("mtry"), ncol = 3, nrow = 1)