tabmeans <- function(x, y, latex = FALSE, xname = NULL, xlevels = NULL, yname = "Y variable", 
                     quantiles = NULL, quantile.vals = FALSE, decimals = 1, p.include = TRUE,
                     p.decimals = c(2, 3), p.cuts = 0.01, p.lowerbound = 0.001, p.leading0 = TRUE, 
                     p.avoid1 = FALSE, n = FALSE, se = FALSE, fig = FALSE) {
  
  # If any inputs are not correct class, return error
  if (!is.logical(latex)) {
    stop("For latex input, please enter TRUE or FALSE")
  }
  if (!is.null(xlevels) && !is.character(xlevels)) {
    stop("For xlevels input, please enter vector of character strings.")
  }
  if (!is.character(yname)) {
    stop("For yname input, please enter character string")
  }
  if (!is.null(quantiles) && ! (is.numeric(quantiles) & length(quantiles) == 1 && quantiles > 1 & quantiles == round(quantiles))) {
    stop("For quantiles input, please enter a whole number greater than 1.")
  } 
  if (!is.numeric(decimals)) {
    stop("For decimals input, please enter numeric value")
  }
  if (!is.logical(p.include)) {
    stop("For p.include input, please enter TRUE or FALSE")
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
  if (!is.logical(se)) {
    stop("For se input, please enter TRUE or FALSE")
  }
  if (!is.logical(fig)) {
    stop("For fig input, please enter TRUE or FALSE")
  }
  
  # Drop missing values
  locs.complete <- which(!is.na(x) & !is.na(y))
  x <- x[locs.complete]
  y <- y[locs.complete]
  
  # Create quantiles if necessary
  if (!is.null(quantiles)) {
    x <- cut(x = x, breaks = quantile(x, probs = seq(0, 1, 1/quantiles)), include.lowest = TRUE, right = TRUE, dig.lab = decimals)
  }
  
  # Get means and SD's or SE's
  means <- tapply(X = y, INDEX = x, FUN = mean)
  ns <- tapply(X = y, INDEX = x, FUN = length)
  
  # Convert decimals to variable for sprintf
  spf <- paste("%0.", decimals, "f", sep = "")
  
  # Get unique values of x
  xvals <- sort(unique(x))
  
  # If xlevels unspecified, set to actual values
  if (is.null(xlevels)) {
    if (!is.null(quantiles)) {
      if (quantile.vals == TRUE) {
        xlevels <- paste("Q", 1:length(xvals), " ", as.character(xvals), sep = "")
      } else {
        xlevels <- paste("Q", 1:length(xvals), sep = "")
      }
    } else {
      xlevels <- xvals
    }
  }
  
  # Calculate p-value based on ANOVA or t-test depending on number of levels of x
  if (p.include == TRUE) {
    
    if (length(xlevels) == 2) {
      
      # F test for equal variances then appropriate t-test
      f <- var.test(x = y[x == xvals[1]], y = y[x == xvals[2]])
      if (f$p.value < 0.05) {
        p <- t.test(x = y[x == xvals[1]], y = y[x == xvals[2]], var.equal = FALSE)$p.value
        message(paste("Unequal variance t-test was used to compare mean ", yname, " in the two groups.", sep = ""))
      } else {
        p <- t.test(x = y[x == xvals[1]], y = y[x == xvals[2]], var.equal = TRUE)$p.value
        message(paste("Equal variance t-test was used to compare mean ", yname, " in the two groups.", sep = ""))
      }
      
    } else {
      
      # ANOVA
      p <- anova(lm(y ~ as.factor(x)))$"Pr(>F)"[1]
      message(paste("ANOVA was used to compare means for ", yname, sep = ""))
      
    }
    
  } else {
    p = NA
  }
  
  if (fig == FALSE) {
    
    # Initialize table
    tbl <- matrix("", nrow = 1, ncol = length(xlevels)+4)
    
    # Get means and SD's or SE's and add variable name and 1st cell entry to table
    if (se == FALSE) {
      vars <- tapply(X = y, INDEX = x, FUN = sd)
      tbl[1, 1] <- paste(yname, ", M (SD)", sep = "")
      tbl[1, 2] <- sprintf("%.0f", length(locs.complete))
      tbl[1, 3] <- paste(sprintf(spf, mean(y)), " (", sprintf(spf, sd(y)), ")", sep = "")
    } else {
      vars <- tapply(X = y, INDEX = x, FUN = function(x) sd(x)/sqrt(length(x)))
      tbl[1, 1] <- paste(yname, ", M (SE)", sep = "")
      tbl[1, 2] <- sprintf("%.0f", length(locs.complete))
      tbl[1, 3] <- paste(sprintf(spf, mean(y)), " (", sprintf(spf, sd(y)/sqrt(length(y))), ")", sep = "")
    }
    
    # Add mean (SD/SE) values to table
    tbl[1, 4:(ncol(tbl)-1)] <- paste(sprintf(spf, means), " (", sprintf(spf, vars), ")", sep = "")
    
    # Add p-value from t-test
    if (p.include == TRUE) {
      tbl[1, ncol(tbl)] <- formatp(p = p, cuts = p.cuts, decimals = p.decimals, lowerbound = p.lowerbound, leading0 = p.leading0, avoid1 = p.avoid1)
    }
    
    # Add column names
    colnames(tbl) <- c("Variable", "N", "Overall", xlevels, "P")
    
    # Drop N column if requested
    if (n == FALSE) {
      tbl <- tbl[, -which(colnames(tbl) == "N"), drop = FALSE]
    }
    
    # Drop p column if requested
    if (p.include == FALSE) {
      tbl <- tbl[, -which(colnames(tbl) == "P")]
    }
    
    # If latex is TRUE, do some re-formatting
    if (latex == TRUE) {
      plocs <- which(substr(tbl[, "P"], 1, 1) == "<")
      if (length(plocs) > 0) {
        tbl[plocs, "P"] <- paste("$<$", substring(tbl[plocs, "P"], 2), sep = "")
      }
    }
  
  } else {
    
    cis <- tapply(X = y, INDEX = x, FUN = function(x) t.test(x)$conf.int)
    unlisted <- unlist(cis)
    ci.range <- max(unlisted) - min(unlisted)
    ylim1 <- min(unlisted) - 0.1*ci.range
    ylim2 <- max(unlisted) + 0.1*ci.range
    cis2 <- unlist(cis)
    tbl <- plot(x = NULL, y = NULL, main = paste("Mean ", yname, "by ", ifelse(!is.null(xname), xname, "Group")), 
                xlim = c(0.5, (length(xlevels)+0.5)), ylim = c(ylim1, ylim2), 
                ylab = paste(yname, " (Mean +/- 95% CI)", sep = ""), xlab = ifelse(!is.null(xname), xname, NA), 
                xaxt = "n", cex.lab = 1.1)
    for (ii in 1:length(cis)) {
      ci <- c(cis[[ii]][1], cis[[ii]][2])
      points(x = ii, y = means[ii], pch = 19)
      lines(x = rep(ii, 2), y = ci)
      lines(x = c((ii - 0.03), (ii + 0.03)), y = rep(ci[1], 2))
      lines(x = c((ii - 0.03), (ii + 0.03)), y = rep(ci[2], 2))
    }
    axis(side = 1, at = 1:length(xlevels), labels = xlevels)
    
    tbl <- recordPlot()
    
  }
  
  # Return table
  return(tbl)
  
}