biling.survey = function(N = 1e4, pYes = .5, random = TRUE, responses = FALSE){
  
  #  N     the total number of people to survey, denoted N.
  #  pYes  TRUE probability of getting a Yes.  
  if(!random) set.seed(0) else set.seed(NULL) 
  
  # Generate a random sample of N responses (Yes = 1, No = 0):
  ResponseSequence = sample(x = c(0, 1), prob = c(1 - pYes, pYes), size = N, replace = TRUE)
  
  # Compute the running proportion of Yeses:
  r = cumsum( ResponseSequence ) # Cumulative sum of Yeses at end of n steps.
  n = 1:N                        # listing the Number of Respondents at each step.
  runProp = r / n                # Proportion of Yeses at end of n steps.
  
    original.par = par(no.readonly = TRUE)
    on.exit(par(original.par))
    par(mgp = c(2, .5, 0), las = 1, tck = -.02, font.lab = 2)
    
    plot.ts(runProp, ty = "o", ylim = c(0, 1), 
            yaxt = "n", pch = 21, bg = 3, xlim = c(1, N),
            xlab = "Number of Respondents", ylab = "Proportion of Yes",
            main = "Bilingual Education Survey", cex = .9)
    
    axis(1, at = 1)
    axis(2, at = seq(0, 1, len = 6), labels = paste0(seq(0, 100, len = 6), "%"))
    
    # Plot a dotted horizontal reference line:
    abline( h = pYes, lty = 2, col = 2 )
    
    text(mean(par("usr")[1:2]), pYes, "TRUE Proportion of \"YES\"", pos = 3, font = 2, col = 4)
    
    # Display the first 10 response sequence:
    ResponseLetters = paste( c("N","Y")[ResponseSequence[1:10] + 1], collapse ="" )
    
    displayString = paste0("Response Sequence = ", ResponseLetters, ". . .")
    
    text( N , 1 , displayString , adj = c(1, .5), col = "red4", font = 2, cex = .8)
    
    # Display the relative frequency at the end of the sequence.
    text( N , .95, paste0("Proportion of Y in ", N, " responses = ", runProp[N]) , adj = c(1, .5), 
          col = "red4", font = 2, cex = .8 )
    
    if(responses)list(Responses = paste( c("N","Y")[ResponseSequence[1:N] + 1], collapse = "" ))
  
}
#Example of use:
biling.survey(N = 1e3, pYes = .6)
