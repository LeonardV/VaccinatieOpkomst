# Auteur: Jan van de Kassteele - RIVM
# Fit parallel random forest

rfparallel <- function(formula, data, ntree = 500, ncores = 4, importance = FALSE) {
  # formula    = random forest formule
  # data       = data om te fitten
  # ntree      = aantal bomen om te groeien
  # ncores     = aantal CPUs 
  # importance = variable importance bijhouden?
  
  # Laad packages
  require(parallel)
  require(doParallel)
  require(foreach)
  
  # Maak cluster met ncores nodes (CBS computer heeft 4 CPU's)
  cl <- makeCluster(ncores)
  #clusterEvalQ(cl, expr = .libPaths("G:/8_Utilities/R/Lib3"))
  clusterEvalQ(cl, expr = .libPaths())
  registerDoParallel(cl)
  
  # Fit Random Forest model aan data
  # Omdat we het parallel doen mag je ntree delen door ncores
  rf.model <- foreach(
    i = 1:ncores,
    .combine = combine,
    .packages = "randomForest") %dopar%
    randomForest(
      formula = formula,
      data = data,
      ntree = round(ntree/ncores),
      importance = importance)
  
  # Stop cluster
  stopCluster(cl)
  
  # Return modelfit
  return(rf.model)
}
