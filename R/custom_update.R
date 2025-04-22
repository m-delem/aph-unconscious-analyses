# Custom brms update function ----------------------------------------------
custom_update <- function(fit, data, filename, ...) {
  # detecting the number of cores to use
  n_cores <- parallel::detectCores() - 4
  # defining the number of iterations per chain (+ 1000 warm-up)
  n_iter <- ceiling(40000 / n_cores) + 1000
  
  update(
    fit, 
    newdata = data,
    chains = n_cores,
    cores  = n_cores,
    iter   = n_iter,
    file = here::here(filename),
    file_compress = "xz",
    ...
  )
}