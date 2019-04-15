library(grf)

generate_data <- function(n, p, seed) {
  set.seed(seed)
  X = matrix(rnorm(n*p), n, p)
  W = rbinom(n, 1, 0.4 + 0.2 * (X[,1] > 0))
  Tau = pmax(X[,1], 0)
  Y = Tau * W + X[,2] + pmin(X[,3], 0) + rnorm(n)
  list(X=X, W=W, Tau=Tau, Y=Y, name="simple")
}


array_task_id <- Sys.getenv("ARRAY_TASK_ID")
if (nchar(array_task_id) > 0) {
  first_seed <- as.integer(array_task_id)
} else {
  first_seed <- sample.int(1000000, size=1)
}
print("FIRST SEED")
print(first_seed)

config <- expand.grid(
  tm=c("none", "earth", "kriging"),
  n=c(500, 1000, 2000, 5000),
  num.fit.trees=c(100, 500, 1000),
  num.fit.reps=c(10000, 100000),
  p=c(5, 20),
  seed=first_seed + seq(10),
  stringsAsFactors = FALSE
)

result <- sapply(seq(nrow(config)), function(i) {
  cfg <- config[i,]
  print(cfg)
  df <- generate_data(cfg$n, cfg$p, seed=cfg$seed)
  if (cfg$tm == "none") {
    tm = NULL
  } else {
    tm = cfg$tm
  }
  cfg$data <- df$name
  tau.forest = causal_forest(
    df$X, df$Y, df$W, 
    tune.parameters = cfg$tm != "none", 
    tuning.method=tm)
  tau.hat.oob = predict(tau.forest)$predictions
  mse.oob = mean((tau.hat.oob - df$Tau)^2) 
  c(cfg, mse.oob=mse.oob)
})

write.csv(t(result), file=sprintf("results_%d.csv", first_seed))
