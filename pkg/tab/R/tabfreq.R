tabfreq <- function(x, y, latex = FALSE, xlevels = NULL, yname = "Y variable", ylevels = NULL, 
                    cell.percent = FALSE, parenth = NULL, text.label = NULL, parenth.sep = "-",
                    test = "chi", decimals = 1, p.include = TRUE, p.decimals = c(2, 3), p.cuts = 0.01,
                    p.lowerbound = 0.001, p.leading0 = TRUE, p.avoid1 = FALSE, n.column = FALSE,
                    n.headings = TRUE, compress = FALSE, compress.val = NULL, bold.colnames = TRUE, 
                    bold.varnames = FALSE, bold.varlevels = FALSE, variable.colname = "Variable") {
  
  # Drop missing values
  locs.complete <- which(!is.na(x) & !is.na(y))
  x <- x[locs.complete]
  y <- y[locs.complete]
  
  # Get cell counts and proportions
  counts <- table(y, x)
  props <- 100*prop.table(counts)
  colprops <- 100*prop.table(counts, 2)
  
  # If any inputs are not correct class, return error
  if (!is.logical(latex)) {
    stop("For latex input, please enter TRUE or FALSE")
  }
  if (!is.null(xlevels) && !is.character(xlevels)) {
    stop("For xlevels input, please enter vector of character strings")
  }
  if (!is.character(yname)) {
    stop("For yname input, please enter character string")
  }
  if (!is.null(ylevels) && !is.character(ylevels)) {
    stop("For ylevels input, please enter vector of character strings")
  }
  if (!is.logical(cell.percent)) {
    stop("For cell.percent input, please enter TRUE or FALSE")
  }
  if (!is.null(parenth) && ! parenth %in% c("none", "percent", "ci", "se")) {
    stop("For parenth input, please enter 'none', 'percent', 'ci', or 'se'")
  }
  if (!is.null(text.label) && !is.character(text.label)) {
    stop("For text.label input, please enter a character string or just leave it unspecified. Use 'none' to request no label")
  }
  if (!is.character(parenth.sep)) {
    stop("For parenth.sep input, please enter a character string (only used if parenth = 'ci'; usually '-' or ', ')")
  }
  if (! test %in% c("chi", "fisher", "z", "z.continuity")) {
    stop("For test input, please enter 'chi', 'fisher', 'z', or 'z.continuity'")
  }
  if (test %in% c("z", "z.continuity") & ! all(dim(counts) == 2)) {
    stop("For test input, 'z' and 'z.continuity' can only be used if both x and y are binary")
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
  if (!is.logical(n.column)) {
    stop("For n.column input, please enter TRUE or FALSE")
  }
  if (!is.logical(n.headings)) {
    stop("For n.headings input, please enter TRUE or FALSE")
  }
  if (!is.logical(compress)) {
    stop("For compress input, please enter TRUE or FALSE")
  }
  if (!is.null(compress) && !is.null(compress.val) && ! compress.val %in% unique(y)) {
    stop("For compress.val input, please ensure that you enter one of the values that y takes on")
  }
  if (!is.logical(bold.colnames)) {
    stop("For bold.colnames input, please enter TRUE or FALSE")
  }
  if (!is.logical(bold.varnames)) {
    stop("For bold.varnames input, please enter TRUE or FALSE")
  }
  if (!is.logical(bold.varlevels)) {
    stop("For bold.varlevels input, please enter TRUE or FALSE")
  }
  if (!is.character(variable.colname)) {
    stop("For variable.colname input, please enter a character string")
  }
  
  # Convert decimals to variable for sprintf
  spf <- paste("%0.", decimals, "f", sep = "")
  
  # If ylevels unspecified, set to actual values
  if (is.null(ylevels)) {
    ylevels <- rownames(counts)
  }
  
  # If parenth NULL, set default value based on cell.percent
  if (is.null(parenth)) {
    if (cell.percent == TRUE) {
      parenth = "ci"
    } else if (cell.percent == FALSE) {
      parenth = "percent"
    }
  }
  
  # Initialize table
  tbl <- matrix("", nrow = nrow(counts)+1, ncol = ncol(counts)+4) 
  
  # Figure out text.label
  if (is.null(text.label)) {
    if (cell.percent == FALSE) {
      part1 <- "n"
    } else if (cell.percent == TRUE) {
      part1 <- "%"
    }
    if (parenth == "none") {
      text.label <- paste(", ", part1, sep = "")
    } else if (parenth == "percent") {
      text.label <- paste(", ", part1, " (%)", sep = "")
    } else if (parenth == "se") {
      text.label <- paste(", ", part1, " (SE)", sep = "")
    } else if (parenth == "ci") {
      text.label <- paste(", ", part1, " (95% CI)", sep = "")
    }
  } else if (text.label == "none") {
    text.label <- NULL
  } else {
    text.label <- paste(", ", text.label, sep = "")
  }
  
  # Add variable name and levels of Y to first row
  tbl[, 1] <- c(paste(yname, text.label, sep = ""), paste("  ", ylevels, sep = ""))
  
  # Add N column
  tbl[1, 2] <- sprintf("%.0f", sum(counts))
  
  # Cell values and parentheses for overall y column
  if (cell.percent == FALSE) {
    cells <- sprintf("%.0f", rowSums(counts))
  } else if (cell.percent == TRUE) {
    cells <- sprintf(spf, rowSums(props))
  }
  
  if (parenth == "none") {
    tbl[2:nrow(tbl), 3] <- cells
  } else if (parenth == "percent") {
    parentheses <- rowSums(props)
    tbl[2:nrow(tbl), 3] <- paste(cells, " (", sprintf(spf, parentheses), ")", sep = "")
  } else if (parenth == "se") {
    parentheses <- sqrt(rowSums(props) / 100 * (1 - rowSums(props) / 100) / sum(counts)) * 100
    tbl[2:nrow(tbl), 3] <- paste(cells, " (", sprintf(spf, parentheses), ")", sep = "")
  } else if (parenth == "ci") {
    conf95 <- matrix(NA, nrow = nrow(counts), ncol = 2)
    for (ii in 1:nrow(counts)) {
      conf95[ii, 1:2] <- binom.test(x = sum(counts[ii, ]), n = sum(counts))$conf.int*100
    }
    tbl[2:nrow(tbl), 3] <- paste(cells, " (", sprintf(spf, conf95[, 1]), parenth.sep, sprintf(spf, conf95[, 2]), ")", sep = "")
  }
  
#   n (%) for overall y
#   tbl[2:nrow(tbl), 3] <- paste(sprintf("%.0f", rowSums(counts)), " (", sprintf(spf, rowSums(props)), ")", sep = "")
  

  # Cell values and parentheses for each cell
  for (ii in 1:ncol(counts)) {
    
    if (cell.percent == FALSE) {
      cells <- sprintf("%.0f", counts[, ii])
    } else if (cell.percent == TRUE) {
      cells <- sprintf(spf, counts[, ii] / sum(counts[, ii]) * 100)
    }
    
    if (parenth == "none") {
      tbl[2:nrow(tbl), (3 + ii)] <- cells
    } else if (parenth == "percent") {
      parentheses <- counts[, ii] / sum(counts[, ii]) * 100
      tbl[2:nrow(tbl), (3 + ii)] <- paste(cells, " (", sprintf(spf, parentheses), ")", sep = "")
    } else if (parenth == "se") {
      parentheses <- sqrt(counts[, ii] / sum(counts[, ii]) * (1 - counts[, ii] / sum(counts[, ii])) / sum(counts[, ii])) * 100
      tbl[2:nrow(tbl), (3 + ii)] <- paste(cells, " (", sprintf(spf, parentheses), ")", sep = "")
    } else if (parenth == "ci") {
      conf95 <- matrix(NA, nrow = nrow(counts), ncol = 2)
      for (jj in 1:nrow(counts)) {
        conf95[jj, 1:2] <- binom.test(x = counts[jj, ii], n = sum(counts[, ii]))$conf.int*100
      }
      tbl[2:nrow(tbl), (3 + ii)] <- paste(cells, " (", sprintf(spf, conf95[, 1]), parenth.sep, sprintf(spf, conf95[, 2]), ")", sep = "")
    }
    
  }
  
  # Statistical test
  if (p.include == FALSE | nrow(counts) == 1) {
    pval = "-"
  } else {
    if (test == "chi") {
      pval <- chisq.test(x = x, y = y)$p.value
      message(paste("Pearson's chi-square test was used to test whether the distribution of ", yname, " differed across groups.", sep = ""))
    } else if (test == "fisher") {
      pval <- fisher.test(x = x, y = y)$p.value
      message(paste("Fisher's exact test was used to test whether the distribution of ", yname, " differed across groups.", sep = ""))
    } else if (test == "z") {
      pval <- prop.test(x = counts, correct = FALSE)$p.value
      message(paste("A z-test (without continuity correction) was used to test whether proportions of ", yname, " differed in the two groups.", sep = ""))
    } else if (test == "z.continuity") {
      pval <- prop.test(x = counts)$p.value
      message(paste("A z-test (with continuity correction) was used to test whether proportions of ", yname, " differed in the two groups.", sep = ""))
    }
    tbl[1, ncol(tbl)] <- formatp(p = pval, cuts = p.cuts, decimals = p.decimals, lowerbound = p.lowerbound, leading0 = p.leading0, avoid1 = p.avoid1)
    
  }
  
  # If xlevels unspecified, set to actual values
  if (is.null(xlevels)) {
    xlevels <- colnames(counts)
  }
  
  # If y binary and compress is TRUE, compress table to a single row
  if (nrow(counts) <= 2 & compress == TRUE) {
    if (is.null(compress.val)) {
      tbl <- matrix(c(tbl[1, 1:2], tbl[nrow(tbl), 3:(ncol(tbl)-1)], tbl[1, ncol(tbl)]), nrow = 1)
    } else {
      whichrow <- which(rownames(counts) == as.character(compress.val)) + 1
      tbl <- matrix(c(tbl[1, 1:2], tbl[whichrow, 3:(ncol(tbl)-1)], tbl[1, ncol(tbl)]), nrow = 1)
    }
  }
  
  # Add column names, with sample sizes for each group if requested
  if (n.headings == FALSE) {
    colnames(tbl) <- c(variable.colname, "N", "Overall", xlevels, "P")
  } else {
    colnames(tbl) <- c(variable.colname, "N", paste(c("Overall", xlevels), " (n = ", c(sum(counts), apply(counts, 2, sum)), ")", sep = ""), "P")
  }
  
  # Drop N column if requested
  if (n.column == FALSE) {
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
    spacelocs <- which(substr(tbl[, variable.colname], 1, 2) == "  ")
    if (length(spacelocs) > 0) {
      tbl[spacelocs, variable.colname] <- paste("\\hskip .4cm ", substring(tbl[spacelocs, variable.colname], 3), sep = "")
    }
    chars <- strsplit(tbl[, variable.colname], "")
    for (ii in 1:length(chars)) {
      percentlocs <- which(chars[[ii]] == "%")
      if (length(percentlocs) > 0) {
        chars[[ii]][percentlocs] <- "\\%"
      }
    }
    tbl[, variable.colname] <- sapply(chars, function(x) paste(x, sep = "", collapse = ""))
    if (bold.colnames == TRUE) {
      colnames(tbl) <- paste("$\\textbf{", colnames(tbl), "}$", sep = "")
    }
    if (bold.varnames == TRUE) {
      tbl[1, 1] <- paste("$\\textbf{", tbl[1, 1], "}$")
    }
    if (bold.varlevels == TRUE) {
      tbl[2:nrow(tbl), 1] <- paste("$\\textbf{", tbl[2:nrow(tbl), 1], "}$", sep = "")
    }
  }
  
  # Return table
  return(tbl)
  
}