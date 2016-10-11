\name{tabfreq.svy}
\alias{tabfreq.svy}
\title{
Generate Frequency Tables for Statistical Reports (Survey Data)
}
\description{
This function creates an I-by-J frequency table and summarizes the results in a clean table for a statistical report. Similar to tabfreq, but for survey data. Relies heavily on the 'survey' package [1, 2].
}
\usage{
tabfreq.svy(x, y, svy, latex = FALSE, xlevels = NULL, yname = "Y variable",
            ylevels = NULL, test = "F", decimals = 1, p.decimals = c(2, 3), p.cuts = 0.01,
            p.lowerbound = 0.001, p.leading0 = TRUE, p.avoid1 = FALSE, n.column = FALSE,
            n.headings = TRUE, compress = FALSE, compress.val = NULL, 
            bold.colnames = TRUE, bold.varnames = FALSE, bold.varlevels = FALSE, 
            variable.colname = "Variable", print.html = FALSE, 
            html.filename = "table1.html")
}
\arguments{
  \item{svy}{
Survey design object created by a call to svydesign [1, 2]. 
}
  \item{x}{
Character string specifying column variable name. Must match one of names(svy$variables).
}
  \item{y}{
Character string specifying row variable name. Must match one of names(svy$variables).
}
  \item{latex}{
If TRUE, object returned is formatted for printing in LaTeX using xtable [3]; if FALSE, formatted for copy-and-pasting from RStudio into a word processor.
}
  \item{xlevels}{
Optional character vector to label the levels of x. If unspecified, the function uses the values that x takes on.
}
  \item{yname}{
Optional label for the y (row) variable.
}
  \item{ylevels}{
Optional character vector to label the levels of y. If unspecified, the function uses the values that y takes on. Note that levels of y will be listed in the order that they appear when you run table(y, x).
}
  \item{test}{
Controls test for association between x and y. Must be a possible value for the 'statistic' input of the svychisq function in the survey package [1,2]: 'F', 'Chisq', 'Wald', 'adjWald', 'lincom', or 'saddlepoint'.
}
  \item{decimals}{
Number of decimal places for percentages.
}
  \item{p.decimals}{
Number of decimal places for p-values. If a vector is provided rather than a single value, number of decimal places will depend on what range the p-value lies in. See p.cuts input.
}
  \item{p.cuts}{
Cut-point(s) to control number of decimal places used for p-values. For example, by default p.cuts is 0.1 and p.decimals is c(2, 3). This means that p-values in the range [0.1, 1] will be printed to two decimal places, while p-values in the range [0, 0.1) will be printed to three decimal places.
}
  \item{p.lowerbound}{
Controls cut-point at which p-values are no longer printed as their value, but rather <lowerbound. For example, by default p.lowerbound is 0.001. Under this setting, p-values less than 0.001 are printed as <0.001.
}
  \item{p.leading0}{
If TRUE, p-values are printed with 0 before decimal place; if FALSE, the leading 0 is omitted.
}
  \item{p.avoid1}{
If TRUE, p-values rounded to 1 are not printed as 1, but as >0.99 (or similarly depending on values for p.decimals and p.cuts). 
}
  \item{n.column}{
If TRUE, the table will have a column for (unweighted) sample size.
}
  \item{n.headings}{
If TRUE, the table will indicate the (unweighted) sample size overall and in each group in parentheses after the column headings.
}
  \item{compress}{
If y has only two levels, setting compress to TRUE will produce a single row for n (percent) for the higher level. For example, if y is gender with 0 for female, 1 for male, setting compress = TRUE will return a table with n (percent) for males only.
}
  \item{compress.val}{
When x and y are both binary, and compress is TRUE, compress.val can be used to specify which level of the y variable should be shown. For example, if x is sex and y is obesity status with levels "Obese" and "Not Obese", setting compress to TRUE and compress.val to "Not Obese" would result in the table comparing the proportions of subjects that are not obese by sex.
}
  \item{bold.colnames}{
If TRUE, column headings are printed in bold font. Only applies if latex = TRUE. 
}
  \item{bold.varnames}{
If TRUE, variable name in the first column of the table is printed in bold font. Only applies if latex = TRUE.
}
  \item{bold.varlevels}{
If TRUE, levels of the y variable are printed in bold font. Only applies if latex = TRUE.
}
  \item{variable.colname}{
Character string with desired heading for first column of table, which shows the y variable name and levels.
}
  \item{print.html}{
If TRUE, function prints a .html file to the current working directory.
}
  \item{html.filename}{
Character string indicating the name of the .html file that gets printed if print.html is set to TRUE.
}
}
\details{
NA
}
\value{
A character matrix with the requested frequency table. If latex is set to TRUE, the character matrix will be formatted for inserting into a Markdown/Sweave/knitr report using the xtable package [3].
}
\references{
1. Lumley T (2012). survey: analysis of complex survey samples. R package version 3.28-2, \url{https://cran.r-project.org/package=survey}.

2. Lumley T (2014). Analysis of complex survey samples. Journal of Statistical Software 9(1): 1-19.

3. Dahl DB (2013). xtable: Export tables to LaTeX or HTML. R package version 1.7-1, \url{https://cran.r-project.org/package=xtable}.

4. Kevin Ushey (2013). Kmisc: Kevin Miscellaneous. R package version 0.5.0. \url{https://CRAN.R-project.org/package=Kmisc}.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
If you wish to paste your tables into Word, you can use either of these approaches:

1. Use the write.cb function in the Kmisc package [4]. If your table is stored in a character matrix named table1, use write.cb(table1) to copy the table to your clipboard. Paste the result into Word, then highlight the text and go to Insert - Table - Convert Text to Table... OK. 

2. Set the print.html input to TRUE. This will result in a .html file writing to your current working directory. When you open this file, you will see a nice looking table that you can copy and paste into Word. You can control the name of this file with the html.filename input. 

If you wish to use LaTeX, R Markdown, knitr, Sweave, etc., please see the package vignette for examples. In most cases, you have to set the latex input to TRUE and then use the xtable package [1]. 

If you have suggestions for additional options or features, or if you would like some help using any function in the package tab, please e-mail me at vandomed@gmail.com. Thanks!
}
\seealso{
\code{\link{svydesign}},
\code{\link{svychisq}},
\code{\link{tabfreq}},
\code{\link{tabmeans}},
\code{\link{tabmedians}},
\code{\link{tabmulti}},
\code{\link{tabglm}},
\code{\link{tabcox}},
\code{\link{tabgee}},
\code{\link{tabmeans.svy}},
\code{\link{tabmedians.svy}},
\code{\link{tabmulti.svy}},
\code{\link{tabglm.svy}}
}
\examples{
NA
}
\keyword{ table }
\keyword{ frequency }
\keyword{ crosstab }
\keyword{ survey }