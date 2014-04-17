tabmeans.svy <- function(x, y, svy, latex = FALSE, xlevels = NULL, yname = "Y variable", 
                         test = "Wald", decimals = 1, p.decimals = c(2,3), p.cuts = 0.01, 
                         p.lowerbound = 0.001, p.leading0 = TRUE, p.avoid1 = FALSE, n = FALSE) {
  
  # If any inputs are not correct class, return error
  if (!is.logical(latex)) {
    stop("For latex input, please enter TRUE or FALSE")
  }
  if (! test %in% c("Wald", "LRT")) {
    stop("For test input, please enter 'Wald' or 'LRT'")
  }
  if (!is.numeric(decimals)) {
    stop("For decimals input, please enter numeric value")
  }
  if (!is.numeric(p.decimals)) {
    stop("For p.decimals input, please enter numeric value or vector")
  }
  if (!is.numeric(p.cuts)) {  
    stop("For p.cuts input, please enter numeric value or vector")
  }
  if (!is.numeric(p.lowerbound)) {
    stop("For p.lowerbound input, please enter numeric value")
  }
  if (!is.logical(p.leading0)) {
    stop("For p.leading0 input, please enter TRUE or FALSE")
  }
  if (!is.logical(p.avoid1)) {
    stop("For p.avoid1 input, please enter TRUE or FALSE")
  }
  if (!is.logical(n)) {
    stop("For n input, please enter TRUE or FALSE")
  }
  
  # Convert decimals to variable for sprintf
  spf <- paste("%0.", decimals, "f", sep = "")
  
  # Extract vectors x and y
  x <- svy$variables[,x]
  y <- svy$variables[,y]
  
  # Update survey object to include y and x explicitly
  svy2 <- update(svy, y = y, x = x)
  
  # Get unique values of x
  xvals <- sort(unique(x))
  
  # If xlevels unspecified, set to actual values
  if (is.null(xlevels)) {
    xlevels <- xvals
  }
  
  # Initialize table
  tbl <- matrix("", nrow = 1, ncol = 3+length(xlevels))
  
  # Get means and SE's overall and by levels of x, and get sample size in each x
  totmean <- svymean(y, design = svy2)
  means <- svyby(~y, by = ~x, FUN = svymean, design = svy2)
  ns <- tapply(X = y, INDEX = x, FUN = length)
  
  # Add mean (SE) values to table
  tbl[1,1] <- paste(yname, ", M (SE)", sep = "")
  tbl[1,2] <- paste(sprintf(spf, totmean), " (", sprintf(spf, sqrt(attr(totmean, "var"))), ")", sep = "")
  tbl[1,3:(ncol(tbl)-1)] <- paste(sprintf(spf, means$"y"), " (", sprintf(spf, means$"se"), ")", sep = "")
  
  # ANOVA
  fit1 <- svyglm(y ~ 1, design = svy2)
  fit2 <- svyglm(y ~ as.factor(x), design = svy2)
  pval <- anova(fit1, fit2, method = test)$p
  tbl[1,ncol(tbl)] <- formatp(p = pval, cuts = p.cuts, decimals = p.decimals, lowerbound = p.lowerbound, leading0 = p.leading0, avoid1 = p.avoid1)

  # If requested, include n's in xlevels labels
  if (n == TRUE) {
    xlevels <- paste(xlevels, " (n = ", tapply(X = y, INDEX = x, FUN = length), ")", sep = "")
    overall <- paste("Overall (n = ", sum(complete.cases(x) & complete.cases(y)), ")", sep = "")
  } else {
    overall <- "Overall"
  }
  
  # Add column names
  colnames(tbl) <- c("Variable", overall, xlevels, "P")
  
  # If latex is TRUE, do some re-formatting
  if (latex == TRUE) {
    plocs <- which(substr(tbl[,"P"], 1, 1) == "<")
    if (length(plocs) > 0) {
      tbl[plocs,"P"] <- paste("$<$", substring(tbl[plocs,"P"], 2), sep = "")
    }
  }
  
  # Return table
  return(tbl)
  
}