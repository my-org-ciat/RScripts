library(foreach)
library(doParallel)

#detect the number of cores, leave one free core for
#other tasks
n_cores <- parallel::detectCores() - 1

#define cluster
my.cluster <- parallel::makeCluster(n.cores, type = "PSOCK")
#check cluster definition 
print(my.cluster)

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

#check if it is registered
foreach::getDoParRegistered()

#how many workers are available? (optional)
foreach::getDoParWorkers()

#loop parallel execution
x <- foreach(
  i = 1:10, 
  .combine = 'c'# combine as vector
) %dopar% {
  sqrt(i)
}

#stop cluster 
parallel::stopCluster(cl = my.cluster)
