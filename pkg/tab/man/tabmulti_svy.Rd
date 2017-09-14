\name{tabmulti.svy}
\alias{tabmulti.svy}
\title{
Generate Multi-row Tables Comparing Means/Medians/Frequencies of Multiple
Variables Across Levels of One Categorical Variable (Survey Data)
}
\description{
This function basically provides an alternative to making multiple calls to
\code{\link{tabmeans.svy}, \code{\link{tabmedians.svy}}, and
\code{\link{tabfreq.svy}}, then using \code{\link{rbind}} to combine the results
into a single table. Similar to \code{\link{tabmulti}}, but for survey data.
Relies heavily on the \bold{survey} package [1, 2].
}
\usage{
tabmulti.svy(svy, xvarname, yvarnames, ymeasures = NULL,
             listwise.deletion = FALSE, latex = FALSE, xlevels = NULL,
             ynames = yvarnames, ylevels = NULL, mean.tests = "Wald",
             median.tests = "wilcoxon", freq.tests = "F", parenth = "iqr",
             text.label = NULL, parenth.sep = "-", decimals = 1,
             p.include = TRUE, p.decimals = c(2, 3), p.cuts = 0.01,
             p.lowerbound = 0.001, p.leading0 = TRUE, p.avoid1 = FALSE,
             n.column = FALSE, n.headings = TRUE, se = FALSE, compress = FALSE,
             bold.colnames = TRUE, bold.varnames = FALSE,
             bold.varlevels = FALSE, variable.colname = "Variable",
             print.html = FALSE, html.filename = "table1.html")
}
\arguments{
  \item{svy}{
Survey design object created by a call to \code{\link{svydesign}} [1, 2].
}
  \item{xvarname}{
Character string with name of column variable. Should be one of
\code{colnames(dataset)}.
}
  \item{yvarnames}{
Character string or vector of character strings with names of row variables.
Each element should be one of \code{colnames(dataset)}.
}
  \item{ymeasures}{
Character string or vector of character strings indicating whether each \code{y}
variable should be summarized by mean, median, or frequency. For example, if
\code{yvarnames} has length three and you wish to display frequencies for the
first variable, means for the second, and medians for the third, you would set
\code{ymeasures = c("freq", "mean", "median")}. If unspecified, function
displays frequencies for any factor variable or numeric variable with five or
fewer unique values, and means for numeric variables with more than five levels.
}
  \item{listwise.deletion}{
If \code{TRUE}, observations with missing values for any \code{y} variable are
excluded entirely; if \code{FALSE}, all available data is used for each
comparison.
}
  \item{latex}{
If \code{TRUE}, object returned is formatted for printing in LaTeX using
\code{\link{xtable}} [3]; if \code{FALSE}, formatted for copy-and-pasting from
RStudio into a word processor.
}
  \item{xlevels}{
Optional character vector to label the levels of \code{x}, used in the column
headings. If unspecified, the function uses the values that \code{x} takes on.
}
  \item{ynames}{
Optional labels for the \code{y} variables. If unspecified, \code{yvarnames} is
used.
}
  \item{ylevels}{
Character vector or list of character vectors to label the levels of the
categorical \code{y} variables.
}
  \item{mean.tests}{
Character string or vector of character strings indicating what statistical
tests should be used to compare means for each \code{y} variable for which a
comparison of means is requested. Elements should be \code{"Wald"} for Wald test
or \code{"LRT"} for likelihood ratio test.
}
  \item{median.tests}{
Character string or vector of character strings indicating what statistical
tests should be used to compare medians for each \code{y} variable for which a
comparison of medians is requested. Elements should be possible value for the
\code{test} input of the \code{\link{svyranktest}} function in the \bold{survey}
package [1, 2]: \code{"wilcoxon"} for Mann-Whitney U/Wilcoxon test of whether
one group is from distribution that is stochastically greater than the other;
\code{"vanderWaerden"} for Van der Waerden test of whether the population
distribution functions are equal; \code{"median"} for Mood's test for whether
the population medians are equal; and \code{"KruskalWallis"} for Kruskal-Wallis
test, which is Mann-Whitney U/Wilcoxon generalized to three or more groups.
}
  \item{freq.tests}{
Character string or vector of character strings indicating what statistical
tests should be used to compare distributions of each categorical \code{y}
variable across levels of the \code{x} variable. Elements should be possible
values for the \code{statistic} input of the \code{\link{svychisq}} function in
the \bold{survey} package [1, 2]: \code{"F"}, \code{"Chisq"}, \code{"Wald"},
\code{"adjWald"}, \code{"lincom"}, or \code{"saddlepoint"}.
}
  \item{parenth}{
For median comparisons, controls what values (if any) are placed in parentheses
after the medians in each cell. Possible values are \code{"none"},
\code{"minmax"} for minimum and maximum; \code{"range"} for difference between
minimum and maximum, \code{"q1q3"} for first and third quartiles, and
\code{"iqr"} for difference between first and third quartiles.
}
  \item{text.label}{
For median comparisons, optional text to put after the \code{y} variable name,
identifying what cell values and parentheses indicate in the table. If
unspecified, function uses default labels based on \code{parenth}, e.g. Median
(IQR) if \code{parenth = "iqr"}. Set to \code{"none"} for no text labels.
}
  \item{parenth.sep}{
For median comparisons, optional character specifying the separator for the two
numbers in parentheses when \code{parenth} is set to \code{"minmax"} or
\code{"q1q3"}. The default is a dash, so values in the table are formatted as
Median (Lower-Upper). If you set \code{parenth.sep = ", "} the values in the
table will instead be formatted as Median (Lower, Upper).
}
  \item{decimals}{
Numeric value or vector of numeric values indicating how many decimal places
should be used in reporting statistics for each \code{y} variable.
}
  \item{p.include}{
If \code{FALSE}, statistical tests are not performed and p-values are not
returned.
}
  \item{p.decimals}{
Number of decimal places for p-values. If a vector is provided rather than a
single value, number of decimal places will depend on what range the p-value l
ies in. See \code{p.cuts}.
}
  \item{p.cuts}{
Cut-point(s) to control number of decimal places used for p-values. For example,
by default \code{p.cuts = 0.1} and \code{p.decimals = c(2, 3)}. This means that
p-values in the range [0.1, 1] will be printed to two decimal places, while
p-values in the range [0, 0.1) will be printed to three decimal places.
}
  \item{p.lowerbound}{
Controls cut-point at which p-values are no longer printed as their value, but
rather <lowerbound. For example, by default \code{p.lowerbound = 0.001}. Under
this setting, p-values less than 0.001 are printed as \code{<0.001}.
}
  \item{p.leading0}{
If \code{TRUE}, p-values are printed with 0 before decimal place; if
\code{FALSE}, the leading 0 is omitted.
}
  \item{p.avoid1}{
If \code{TRUE}, p-values rounded to 1 are not printed as 1, but as \code{>0.99}
(or similarly depending on \code{p.decimals} and \code{p.cuts}).
}
  \item{n.column}{
If \code{TRUE}, the table will have a column for (unweighted) sample size.
}
  \item{n.headings}{
If \code{TRUE}, the table will indicate the (unweighted) sample size overall and
in each group in parentheses after the column headings.
}
  \item{se}{
If \code{TRUE}, the table will present mean (standard error) rather than mean
(standard deviation) for continuous \code{y} variables.
}
  \item{compress}{
If \code{TRUE}, categorical \code{y} variables with two levels are compressed
into a single row.
}
  \item{bold.colnames}{
If \code{TRUE}, column headings are printed in bold font. Only applies if
\code{latex = TRUE}.
}
  \item{bold.varnames}{
If \code{TRUE}, variable name in the first column of the table is printed in
bold font. Only applies if \code{latex = TRUE}.
}
  \item{bold.varlevels}{
If \code{TRUE}, levels of categorical \code{y} variables are printed in bold
font. Only applies if \code{latex = TRUE}.
}
  \item{variable.colname}{
Character string with desired heading for first column of table, which shows the
\code{y} variable name and levels.
}
  \item{print.html}{
If \code{TRUE}, function prints a .html file to the current working directory.
}
  \item{html.filename}{
Character string indicating the name of the .html file that gets printed if
\code{print.html = TRUE}.
}
}
\details{
Please see help files for \code{\link{tabmeans.svy}},
\code{\link{tabmedians.svy}}, and \code{\link{tabfreq.svy}} for details on
statistical tests.
}
\value{
A character matrix comparing means/medians/frequencies of \code{y} variables
across levels of the \code{x} variable. If \code{latex = TRUE}, the character
matrix will be formatted for inserting into a Markdown/Sweave/knitr report using
the \code{\link{xtable}} package [3].
}
\references{
1. Lumley T (2012). survey: analysis of complex survey samples. R package
version 3.28-2, \url{https://cran.r-project.org/package=survey}.

2. Lumley T (2014). Analysis of complex survey samples. Journal of Statistical
Software 9(1): 1-19.

3. Dahl DB (2013). xtable: Export tables to LaTeX or HTML. R package version
1.7-1, \url{https://cran.r-project.org/package=xtable}.

4. Kevin Ushey (2013). Kmisc: Kevin Miscellaneous. R package version 0.5.0.
\url{https://CRAN.R-project.org/package=Kmisc}.

Acknowledgment: This material is based upon work supported by the National
Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
If you wish to paste your tables into Word, you can use either of these
approaches:

1. Use the \code{\link{write.cb}} function in the \code{\link{Kmisc}} package
[4]. If your table is stored in a character matrix named \code{table1}, use
\code{write.cb(table1)} to copy the table to your clipboard. Paste the result
into Word, then highlight the text and go to \code{Insert - Table - Convert Text
to Table... OK}.

2. Set \code{print.html = TRUE}. This will result in a .html file writing to
your current working directory. When you open this file, you will see a nice
looking table that you can copy and paste into Word. You can control the name of
this file with \code{html.filename}.

If you wish to use LaTeX, R Markdown, knitr, Sweave, etc., set
\code{latex = TRUE} and then use \code{\link{xtable}} [3]. You may have to set
\code{sanitize.text.function = identity} when calling
\code{\link{print.xtable}}.

If you have suggestions for additional options or features, or if you would like
some help using any function in \bold{tab}, please e-mail me at
vandomed@gmail.com. Thanks!
}
\seealso{
\code{\link{svydesign}}\cr
\code{\link{svyglm}}\cr
\code{\link{svychisq}}\cr
\code{\link{svyquantile}}\cr
\code{\link{svyranktest}}\cr
\code{\link{tabfreq}}\cr
\code{\link{tabmeans}}\cr
\code{\link{tabmedians}}\cr
\code{\link{tabglm}}\cr
\code{\link{tabcox}}\cr
\code{\link{tabgee}}\cr
\code{\link{tabfreq.svy}}\cr
\code{\link{tabmeans.svy}}\cr
\code{\link{tabmedians.svy}}\cr
\code{\link{tabglm.svy}}
}
\examples{
NA
}
\keyword{ table }
\keyword{ means }
\keyword{ t-test }
\keyword{ anova }
