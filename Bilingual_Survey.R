biling.survey = function(N = 40, pBi = .5, ...){
  
ResponseSequence = sample(x = 0:1, prob = c(1 - pBi, pBi), size = N, replace = TRUE)  # Generate a random sample of N responses
                                                                                        # (B = 1, M = 0)
runProp = cumsum(ResponseSequence) / 1:N   # Compute the running proportion of Yeses:

original.par = par(no.readonly = TRUE)   ;  on.exit(par(original.par))    
par(mgp = c(2, .5, 0), las = 1, tck = -.02, font.lab = 2, cex.lab = 1, ...)

plot.ts(runProp, ty = "o", ylim = c(0, 1), yaxt = "n", pch = 21, bg = 3, xlab = "Number of Respondents", ylab = "Proportion of Bilingual Ed.",
        main = "Bilingual Education Survey", cex = .9)
    
axis(2, at = seq(0, 1, len = 6), labels = paste0(seq(0, 100, len = 6), "%"))
    
abline(h = pBi, lty = 2, col = 2)
    
text(mean(par("usr")[1:2]), pBi, "TRUE Proportion of \"B\"", pos = 3, font = 2, col = 4)
    
ResponseLetters = paste( c("M","B")[ResponseSequence[1:10] + 1], collapse ="" )    # Display the first 10 response sequence
    
displayString = paste0("Response Sequence = ", ResponseLetters, ". . .")
    
text(N, c(1, .95), c(displayString, paste0("Proportion of B in ", N, " responses = ", runProp[N])), adj = c(1, .5), col = "red4", font = 2, cex = .8)
}
#Example of use:
biling.survey(N = 100, pBi = .75)
