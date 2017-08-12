
d.overlap = function(n1, n2 = NA, d){
  
  options(warn = -1)
    N = ifelse(is.na(n2), n1, (n1 * n2)/(n1 + n2))
   df = ifelse(is.na(n2), n1 - 1, (n1 + n2) - 2)
 d.SE = 1/sqrt(N)  ; ncp.min = min(d)*sqrt(N) ;  ncp.max = max(d)*sqrt(N)
  
min.d = d.SE*qt(1e-3, df, ncp.min)  ;  max.d = d.SE*qt(0.99, df, ncp.max)
  
  par(mar = c(5.1, 4.1, 6.1, 2.1))
    
for(i in 1:length(d)){
      
H = curve( dt(x*sqrt(N), df, d[i]*sqrt(N))*sqrt(N), min.d, max.d, n = 1e3, xlab = "Effect Size", 
           ylab = NA, las = 1, font = 2, lwd = 2, ty = "n", font.lab = 2, add = i != 1, 
          bty = "n", yaxt = "n", mgp = c(1, .4, -.6))
      
polygon(H, col = adjustcolor(i, .7), border = NA)
      
text(d[i], max(H$y), bquote(bolditalic(H[.(i-1)])), pos = 3, xpd = TRUE)

axis(1, at = d[i], col = i, col.axis = i, mgp = c(1, .4, -.6), font = 2)

segments(d[i], 0, d[i], max(H$y), lty = 3)
 
   }

}

d.overlap(n1 = 6, d = seq(0, 2, .5))
