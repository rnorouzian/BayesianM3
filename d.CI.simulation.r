
d.CI.sim <- function(d, n1, n2 = NA, conf.level = .95, n.sim = 5, ylabel = FALSE){
  
CI.d <- function(da = d, n1a = n1, n2a = n2, conf.levela = conf.level){
  
alpha = (1 - conf.levela)/2
    N = ifelse(is.na(n2a), n1a, (n1a * n2a)/(n1a + n2a))
   df = ifelse(is.na(n2a), n1a - 1, (n1a + n2a) - 2)
 d.SE = 1/sqrt(N)  ;   t = da/d.SE
    
f <- function (ncp, alpha, q, df) {
    abs(suppressWarnings(pt(q = t, df = df, ncp, lower.tail = FALSE)) - alpha)
  }
    
a = lapply(14:ifelse(da!= 0, d*sqrt(N)+5, 30), function(x) c(-x, x))
    
CI = matrix(NA, length(a), 2)
    
for(i in 1:length(a)){
      
CI[i,] = sapply(c(alpha, 1-alpha),
function(x) optimize(f, interval = a[[i]], alpha = x, q = t, df = df, tol = 1e-12)[[1]]*d.SE)
  }  
    
CI[which.max(ave(1:nrow(CI), do.call(paste, round(data.frame(CI), 3)), FUN = seq_along)), ]
    
  }
  
  
fun <- function(db = d, n1b = n1, n2b = n2, conf.levelb = conf.level){ 
  
   N = ifelse(is.na(n2b), n1b, (n1b * n2b)/(n1b + n2b))
  df = ifelse(is.na(n2b), n1b - 1, (n1b + n2b) - 2)
d.SE = 1/sqrt(N)  ;   t = db/d.SE
    
  ds = rt(1, df, t)*d.SE
c(CI = CI.d(da = ds, n1a = n1, n2a = n2, conf.levela = conf.level), ds = ds)
  }
  
  sim <- t(replicate(n.sim, fun()))
  
  capture = sim[ ,1] <= d & d <= sim[ ,2]
  
  y = unlist(lapply(1:n.sim, function(x) c(x, x)))
  
  original.par = par(no.readonly = TRUE)
  on.exit(par(original.par))
  
  G = function(){
  par(mgp = c(2, .2, 0), tck = -.015)  
  plot(sim[, 1:2], y, ty = "n", ylab = NA, yaxt = "n", xlab = "Effect Size", font.lab = 2)
  axis(1, at = d, col.axis = 2, col = 2, font = 2)
  abline(h = 1:n.sim, col = 8, lty = 3)
  if(ylabel) axis(2, at = 1:n.sim, labels = paste0("Researcher ", rev(1:n.sim)), font = 2, las = 1, cex.axis = .8, tck = -.006)
  abline(v = d, lty = 2, col = 2) 
  segments(sim[ ,1], 1:n.sim, sim[ ,2], 1:n.sim, lend = 1, col = ifelse(capture, 1, 2))
  points(sim[, 3], 1:n.sim, pch = 19, col = ifelse(capture, 1, 2), cex = ifelse(n.sim > 50, .6, .65))
  }
  
  library(ReporteRs)
  
  doc = addPlot(docx(), fun = G, vector.graphic = TRUE, width = 4.5, height = 5.5,  
                par.properties = parCenter(), editable = TRUE)
  
  writeDoc(doc, file = "d_CI.docx" )
  
  noquote(paste0("Coverage = ", mean(capture)*1e2, "%")) 
}

d.CI.sim(d = .5, n1 = 20, n.sim = 20, ylabel = TRUE)

 
######  

######


CI.bi = function(n, p, n.sim, ylabel = FALSE){
  
fun <- function(n1 = n, p1 = p){
    x = rbinom(1, size = n1, prob = p1)
   pe = x/n1
   CI = binom.test(x, n1, p1)[[4]]
  c(L = CI[1], U = CI[2], pe = pe)
  }
  
  sim <- t(replicate(n.sim, fun()))
  capture = sim[ ,1] <= p & p <= sim[ ,2]
  
  y = unlist(lapply(1:n.sim, function(x) c(x, x)))
  
  original.par = par(no.readonly = TRUE)
  on.exit(par(original.par))
  
  G = function(){
  par(mgp = c(2, .2, 0), tck = -.015)
  plot(sim[, 1:2], y, ty = "n", ylab = NA, yaxt = "n", xlab = "Proportion of Agreement", font.lab = 2)
  
  abline(h = 1:n.sim, col = 8, lty = 3)
  abline(v = p, lty = 2, col = 2)
  segments(sim[ ,1], 1:n.sim, sim[ ,2], 1:n.sim, lend = 1, col = ifelse(capture, 1, 2))
  axis(1, at = p, col.axis = 2, col = 2, font = 2)
  if(ylabel) axis(2, at = 1:n.sim, labels = paste0("Researcher ", rev(1:n.sim)), font = 2, las = 1, cex.axis = .8, tck = -.006)
  points(sim[, 3], 1:n.sim, pch = 19, col = ifelse(capture, 1, 2), cex = ifelse(n.sim > 50, .6, .65))
  }
  
  library(ReporteRs)
  
  doc = addPlot(docx(), fun = G, vector.graphic = TRUE, width = 4, height = 5,  
                par.properties = parCenter(), editable = TRUE)
  
  writeDoc(doc, file = "Binom_CI.docx" )
  
  noquote(paste0("Coverage = ", mean(capture)*1e2, "%")) 
}
# Example of use:
CI.bi(n = 15, p = .6, n.sim = 20, ylabel = T)

