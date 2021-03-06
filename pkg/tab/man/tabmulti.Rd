\name{tabmulti}
\alias{tabmulti}
\title{
Generate Multi-row Tables Comparing Means/Medians/Frequencies of Multiple
Variables Across Levels of One Categorical Variable
}
\description{
This function basically provides an alternative to making multiple calls to
\code{\link{tabmeans}}, \code{\link{tabmedians}}, and \code{\link{tabfreq}},
then using \code{\link{rbind}} to combine the results into a single table.
}
\usage{
tabmulti(dataset, xvarname, yvarnames, ymeasures = NULL,
         listwise.deletion = TRUE, latex = FALSE, xlevels = NULL,
         ynames = yvarnames, ylevels = NULL, quantiles = NULL,
         quantile.vals = FALSE, parenth.sep = "-", cell = "n",
         freq.parenth = NULL, freq.text.label = NULL, freq.tests = "chi",
         means.parenth = "sd", means.text.label = NULL, variance = "unequal",
         medians.parenth = "iqr", medians.text.label = NULL, decimals = NULL,
         p.include = TRUE, p.decimals = c(2, 3), p.cuts = 0.01,
         p.lowerbound = 0.001, p.leading0 = TRUE, p.avoid1 = FALSE,
         overall.column = TRUE, n.column = FALSE, n.headings = TRUE,
         compress = FALSE, bold.colnames = TRUE, bold.varnames = FALSE,
         bold.varlevels = FALSE, variable.colname = "Variable",
         print.html = FALSE, html.filename = "table1.html")
}
\arguments{
  \item{dataset}{
Data frame or matrix containing variables of interest.
}
  \item{xvarname}{
Character string with name of column variable. Should be one of
\code{colnames(dataset)}.
}
  \item{yvarnames}{
Character vector with names of row variables. Each element should be one of
\code{colnames(dataset)}.
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
\code{\link{xtable}} [1]; if \code{FALSE}, formatted for copy-and-pasting from
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
  \item{quantiles}{
If specified, function compares \code{y} variables across quantiles of the
\code{x} variable. For example, if \code{x} contains continuous BMI values and
\code{y} contains continuous HDL and race, setting \code{quantiles = 3} would
result in mean HDL and distribution of race being compared across tertiles of
BMI.
}
  \item{quantile.vals}{
If \code{TRUE}, labels for \code{x} show quantile number and corresponding range
of the \code{x} variable, e.g. Q1 [0.00, 0.25). If \code{FALSE}, labels for
quantiles just show quantile number, e.g. Q1. Only used if \code{xlevels} is not
specified.
}
  \item{parenth.sep}{
Optional character specifying the separator between first and second numbers in
parentheses (e.g. lower and upper bound of confidence intervals, when
requested). Usually either \code{"-"} or \code{", "} depending on user
preference.
}
  \item{cell}{
Controls what values are placed in cells for frequency comparisons. Possible
values are \code{"n"} for counts, \code{"tot.percent"} for table percentage,
\code{"col.percent"} for column percentage, \code{"row.percent"} for row
percentage, \code{"tot.prop"} for table proportion, \code{"col.prop"} for column
proportion, \code{"row.prop"} for row proportion, \code{"n/totn"} for
count/total counts, \code{"n/coln"} for count/column count, and \code{"n/rown"}
for count/row count.
}
  \item{freq.parenth}{
Controls what values (if any) are placed in parentheses after the values in each
cell for frequency comparisons. By default, if \code{cell} is \code{"n"},
\code{"n/totn"}, \code{"n/coln"}, or \code{"n/rown"} then the corresponding
percentage is shown in parentheses; if \code{cell} is \code{"tot.percent"},
\code{"col.percent"}, \code{"row.percent"}, \code{"tot.prop"},
\code{"col.prop"}, or \code{"row.prop"} then a 95\% confidence interval for the
requested percentage of proportion is shown in parentheses. Possible values are
\code{"none"}, \code{"se"} for standard error of requested percentage or
proportion, \code{"ci"} for 95\% confidence interval for requested percentage or
proportion, and \code{"tot.percent"}, \code{"col.percent"}, \code{"row.percent"},
\code{"tot.prop"}, \code{"col.prop"}, or \code{"row.prop"} for various
percentages or proportions.
}
  \item{freq.text.label}{
Optional text to put after the \code{y} variable name for frequency comparisons,
identifying what cell values and parentheses indicate in the table. If
unspecified, function uses default labels based on \code{cell} and
\code{freq.parenth} settings. Set to \code{"none"} for no text labels.
}
  \item{freq.tests}{
Character string or vector of character strings indicating what statistical
tests should be used to compare distributions of each categorical \code{y}
variable across levels of the \code{x} variable. Elements can be \code{"chi"}
for Pearson's chi-squared test, \code{"fisher"} for Fisher's exact test,
\code{"z"} for z test without continuity correction, or \code{"z.continuity"}
for z test with continuity correction. \code{"z"} and \code{"z.continuity"} can
only be used for binary \code{x} and \code{y} variables.
}
  \item{means.parenth}{
Controls what values (if any) are placed in parentheses after the means in each
cell for mean comparisons. Possible values are \code{"none"}, \code{"sd"} for
standard deviation, \code{"se"} for standard error, \code{"t.ci"} for 95\%
confidence interval for population mean based on t distribution, and
\code{"z.ci"} for 95\% confidence interval for population mean based on z
distribution.
}
  \item{means.text.label}{
Optional text to put after the \code{y} variable name, identifying what cell
values and parentheses indicate in the table. If unspecified, function uses
default labels based on \code{means.parenth}, e.g. M (SD) if
\code{means.parenth = "sd"}. Set to \code{"none"} for no text labels.
}
  \item{variance}{
Controls whether equal variance t-test or unequal variance t-test is used for
mean comparisons when \code{x} has two levels. Possible values are
\code{"equal"} for equal variance, \code{"unequal"} for unequal variance, and
\code{"ftest"} for F test to determine which version of the t-test to use. Note
that unequal variance t-test is less restrictive than equal variance t-test, and
the F test is only valid when y is normally distributed in both \code{x} groups.
}
  \item{medians.parenth}{
Controls what values (if any) are placed in parentheses after the medians in
each cell for median comparisons. Possible values are \code{"none"},
\code{"iqr"} for difference between first and third quartiles, \code{"range"}
for difference between minimum and maximum, \code{"minmax"} for minimum and
maximum, and \code{"q1q3"} for first and third quartiles.
}
  \item{medians.text.label}{
Optional text to put after the \code{y} variable name for median comparisons,
identifying what cell values and parentheses indicate in the table. If
unspecified, function uses default labels based on \code{medians.parenth}, e.g.
Median (IQR) if \code{medians.parenth = "iqr"}. Set to \code{"none"} for no text
labels.
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
single value, number of decimal places will depend on what range the p-value
lies in. See \code{p.cuts}.
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
  \item{overall.column}{
If \code{FALSE}, column showing frequencies/means/medians for \code{y} in full
sample is suppressed.
}
  \item{n.column}{
If \code{TRUE}, the table will have a column for sample size.
}
  \item{n.headings}{
If \code{TRUE}, the table will indicate the sample size overall and in each
group in parentheses after the column headings.
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
See help files for \code{\link{tabmeans}}, \code{\link{tabmedians}}, and
\code{\link{tabfreq}} for details on statistical tests.
}
\value{
A character matrix comparing means/medians/frequencies of \code{y} variables
across levels of the \code{x} variable. If \code{latex = TRUE}, the character
matrix will be formatted for inserting into a Markdown/Sweave/knitr report using
the \code{\link{xtable}} package [1].
}
\references{
1. Dahl DB (2013). xtable: Export tables to LaTeX or HTML. R package version
1.7-1, \url{https://cran.r-project.org/package=xtable}.

2. Kevin Ushey (2013). Kmisc: Kevin Miscellaneous. R package version 0.5.0.
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
[2]. If your table is stored in a character matrix named \code{table1}, use
\code{write.cb(table1)} to copy the table to your clipboard. Paste the result
into Word, then highlight the text and go to \code{Insert - Table - Convert Text
to Table... OK}.

2. Set \code{print.html = TRUE}. This will result in a .html file writing to
your current working directory. When you open this file, you will see a nice
looking table that you can copy and paste into Word. You can control the name of
this file with \code{html.filename}.

If you wish to use LaTeX, R Markdown, knitr, Sweave, etc., set
\code{latex = TRUE} and then use \code{\link{xtable}} [1]. You may have to set
\code{sanitize.text.function = identity} when calling
\code{\link{print.xtable}}.

If you have suggestions for additional options or features, or if you would like
some help using any function in \bold{tab}, please e-mail me at
vandomed@gmail.com. Thanks!
}
\seealso{
\code{\link{tabfreq}}\cr
\code{\link{tabmeans}}\cr
\code{\link{tabmedians}}\cr
\code{\link{tabglm}}\cr
\code{\link{tabcox}}\cr
\code{\link{tabgee}}\cr
\code{\link{tabfreq.svy}}\cr
\code{\link{tabmeans.svy}}\cr
\code{\link{tabmedians.svy}}\cr
\code{\link{tabmulti.svy}}\cr
\code{\link{tabglm.svy}}
}
\examples{
# Load in sample dataset d
data(d)

# Compare age, sex, race, and BMI in control vs. treatment group
# data for each comparison
table1 <- tabmulti(dataset = d, xvarname = "Group",
                   yvarnames = c("Age", "Sex", "Race", "BMI"))

# Repeat, but use all available data for each comparison (as opposed to listwise
# deletion)
table2 <- tabmulti(dataset = d, xvarname = "Group", n.column = TRUE,
                   n.headings = FALSE,
                   yvarnames = c("Age", "Sex", "Race", "BMI"),
                   listwise.deletion = FALSE)

# Same as table1, but compare medians rather than means for BMI
table3 <- tabmulti(dataset = d, xvarname = "Group",
                   yvarnames = c("Age", "Sex", "Race", "BMI"),
                   ymeasures = c("mean", "freq", "freq", "median"))
}
\keyword{ table }
\keyword{ means }
\keyword{ t-test }
\keyword{ anova }
