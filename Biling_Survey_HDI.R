HDI <- function(fun, cover = .95, range = c(0, 1)){
  
  mode = optimize(fun, interval = range, maximum = TRUE, tol = 1e-12)[[1]]
  total.area = integrate(fun, range[1], range[2])[[1]]
  
  O <- function(d){
    parea <- integrate(fun, mode-d, mode+d)[[1]] / total.area
    (cover - parea)^2
  }
  o <- optimize(O, c(0, range[2]/2 - 1e-2))[[1]]
  
  return(c(mode-o, mode+o))
}