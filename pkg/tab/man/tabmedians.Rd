\name{tabmedians}
\alias{tabmedians}
\title{
Generate Summary Tables of Median Comparisons for Statistical Reports
}
\description{
This function compares the median of a continuous variable across levels of a
categorical variable and summarizes the results in a clean table for a
statistical report.
}
\usage{
tabmedians(x, y, latex = FALSE, xlevels = NULL, yname = NULL, quantiles = NULL,
           quantile.vals = FALSE, parenth = "iqr", text.label = NULL,
           parenth.sep = "-", decimals = NULL, p.include = TRUE,
           p.decimals = c(2, 3), p.cuts = 0.01, p.lowerbound = 0.001,
           p.leading0 = TRUE, p.avoid1 = FALSE, overall.column = TRUE,
           n.column = FALSE, n.headings = TRUE, bold.colnames = TRUE,
           bold.varnames = FALSE, variable.colname = "Variable",
           print.html = FALSE, html.filename = "table1.html")
}
\arguments{
  \item{x}{
Vector of values for the categorical variable.
}
  \item{y}{
Vector of values for the continuous variable.
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
  \item{yname}{
Optional label for the \code{y} (row) variable. If unspecified, variable name of
\code{y} is used.
}
  \item{quantiles}{
If specified, function compares medians of the \code{y} variable across
quantiles of the \code{x} variable. For example, if \code{x} contains continuous
BMI values and \code{y} contains continuous HDL cholesterol levels, setting
\code{quantiles = 3} would result in median HDL being compared across tertiles
of BMI.
}
  \item{quantile.vals}{
If \code{TRUE}, labels for \code{x} show quantile number and corresponding range
of the \code{x} variable, e.g. Q1 [0.00, 0.25). If \code{FALSE}, labels for
quantiles just show quantile number, e.g. Q1. Only used if \code{xlevels} is not
specified.
}
  \item{parenth}{
Controls what values (if any) are placed in parentheses after the medians in
each cell. Possible values are \code{"none"}, \code{"iqr"} for difference
between first and third quartiles, \code{"range"} for difference between minimum
and maximum, \code{"minmax"} for minimum and maximum, \code{"q1q3"} for first
and third quartiles, and \code{"ci.90"}, \code{"ci.95"}, or \code{"ci.99"} for
confidence intervals for the medians (based on binomial probabilities if one or
more groups have n less than 10, otherwise based on normal approximation to
binomial).
}
  \item{text.label}{
Optional text to put after the \code{y} variable name, identifying what cell
values and parentheses indicate in the table. If unspecified, function uses
default labels based on \code{parenth}, e.g. Median (IQR) if
\code{parenth = "iqr"}. Set to \code{"none"} for no text labels.
}
  \item{parenth.sep}{
Optional character specifying the separator for the two numbers in parentheses
when \code{parenth} is set to \code{"minmax"} or \code{"q1q3"}. The default is a
dash, so values in the table are formatted as Median (Lower-Upper). If you set
\code{parenth.sep = ", "} the values in the table will instead be formatted as
Median (Lower, Upper).
}
  \item{decimals}{
Number of decimal places for values in table. If unspecified, function uses 0
decimal places if the largest median (in magnitude) is in [1,000, Inf), 1
decimal place if [10, 1,000), 2 decimal places if [0.1, 10), 3 decimal places if
[0.01, 0.1), 4 decimal places if [0.001, 0.01), 5 decimal places if
[0.0001, 0.001), and 6 decimal places if [0, 0.0001).
}
  \item{p.include}{
If \code{FALSE}, statistical test is not performed and p-value is not returned.
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
If \code{FALSE}, column showing median of \code{y} in full sample is suppressed.
}
  \item{n.column}{
If \code{TRUE}, the table will have a column for sample size.
}
  \item{n.headings}{
If \code{TRUE}, the table will indicate the sample size overall and in each
group in parentheses after the column headings.
}
  \item{bold.colnames}{
If \code{TRUE}, column headings are printed in bold font. Only applies if
\code{latex = TRUE}.
}
  \item{bold.varnames}{
If \code{TRUE}, variable name in the first column of the table is printed in
bold font. Only applies if \code{latex = TRUE}.
}
  \item{variable.colname}{
Character string with desired heading for first column of table, which shows the
\code{y} variable name.
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
If \code{x} has two levels, a Mann-Whitney U (also known as Wilcoxon rank-sum)
test is used to test whether the distribution of the continuous variable
(\code{y}) differs in the two groups (\code{x}). If \code{x} has more than two
levels, a Kruskal-Wallis test is used to test whether the distribution of
\code{y} differs across at least two of the \code{x} groups.

Both \code{x} and \code{y} can have missing values. The function drops
observations with missing \code{x} or \code{y}.
}
\value{
A character matrix with the requested table comparing median \code{y} across
levels of \code{x}. If \code{latex = TRUE}, the character matrix will be
formatted for inserting into a Markdown/Sweave/knitr report using the
\code{\link{xtable}} package [1].
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
\code{\link{tabmulti}}\cr
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
# Load in sample dataset d and drop rows with missing values
data(d)
d <- d[complete.cases(d), ]

# Create labels for group and race
groups <- c("Control", "Treatment")
races <- c("White", "Black", "Mexican American", "Other")

# Compare median BMI in control group vs. treatment group
medtable1 <- tabmedians(x = d$Group, y = d$BMI)

# Repeat, but show first and third quartile rather than IQR in parentheses
medtable2 <- tabmedians(x = d$Group, y = d$BMI, parenth = "q1q3")

# Compare median BMI by race, suppressing overall column and (n = ) part of
# headings
medtable3 <- tabmedians(x = d$Race, y = d$BMI, overall.column = FALSE,
                        n.headings = FALSE)

# Compare median BMI by quartile of age
medtable4 <- tabmedians(x = d$Age, y = d$BMI, quantiles = 4)

# Create single table comparing median BMI and median age in control vs.
# treatment group
medtable5 <- rbind(tabmedians(x = d$Group, y = d$BMI),
                   tabmedians(x = d$Group, y = d$Age))

# A (usually) faster way to make the above table is to call the tabmulti
# function
medtable6 <- tabmulti(dataset = d, xvarname = "Group",
                      yvarnames = c("BMI", "Age"), ymeasures = "median")

# medtable5 and medtable6 are equivalent
all(medtable5 == medtable6)
}
\keyword{ table }
\keyword{ median }
