\name{tabmeans}
\alias{tabmeans}
\title{
Generate Summary Tables of Mean Comparisons for Statistical Reports
}
\description{
This function compares the mean of a continuous variable across levels of a
categorical variable and summarizes the results in a clean table (or figure) for
a statistical report.
}
\usage{
tabmeans(x, y, latex = FALSE, variance = "unequal", xname = NULL,
         xlevels = NULL, yname = NULL, quantiles = NULL, quantile.vals = FALSE,
         parenth = "sd", text.label = NULL, parenth.sep = "-", decimals = NULL,
         p.include = TRUE, p.decimals = c(2, 3), p.cuts = 0.01,
         p.lowerbound = 0.001, p.leading0 = TRUE, p.avoid1 = FALSE,
         overall.column = TRUE, n.column = FALSE, n.headings = TRUE,
         bold.colnames = TRUE, bold.varnames = FALSE,
         variable.colname = "Variable", fig = FALSE, fig.errorbars = "z.ci",
         fig.title = NULL, print.html = FALSE, html.filename = "table1.html")
}
\arguments{
  \item{x}{
Vector of values for the categorical \code{x} variable.
}
  \item{y}{
Vector of values for the continuous \code{y} variable.
}
  \item{latex}{
If \code{TRUE}, object returned is formatted for printing in LaTeX using
\code{\link{xtable}} [1]; if \code{FALSE}, formatted for copy-and-pasting from
RStudio into a word processor.
}
  \item{variance}{
Controls whether equal variance t-test or unequal variance t-test is used when
\code{x} has two levels. Possible values are \code{"equal"} for equal variance,
\code{"unequal"} for unequal variance, and \code{"ftest"} for F test to
determine which version of the t-test to use. Note that unequal variance t-test
is less restrictive than equal variance t-test, and the F test is only valid
when \code{y} is normally distributed in both \code{x} groups.
}
  \item{xname}{
Label for the categorical variable. Only used if \code{fig = TRUE}.
}
  \item{xlevels}{
Optional character vector to label the levels of \code{x}, used in the column
headings. If unspecified, the function uses the values that \code{x} takes on.
}
  \item{yname}{
Optional label for the continuous \code{y} variable. If unspecified, variable
name of \code{y} is used.
}
  \item{quantiles}{
If specified, function compares means of the \code{y} variable across quantiles
of the \code{x} variable. For example, if \code{x} contains continuous BMI
values and \code{y} contains continuous HDL cholesterol levels, setting
\code{quantiles = 3} would result in mean HDL being compared across tertiles of
BMI.
}
  \item{quantile.vals}{
If \code{TRUE}, labels for \code{x} show quantile number and corresponding range
of the \code{x} variable, e.g. Q1 [0.00, 0.25). If \code{FALSE}, labels for
quantiles just show quantile number, e.g. Q1. Only used if \code{xlevels} is not
specified.
}
  \item{parenth}{
Controls what values (if any) are placed in parentheses after the means in each
cell. Possible values are \code{"none"}, \code{"sd"} for standard deviation,
\code{"se"} for standard error, \code{"t.ci"} for 95\% confidence interval for
population mean based on t distribution, and \code{"z.ci"} for 95\% confidence
interval for population mean based on z distribution.
}
  \item{text.label}{
Optional text to put after the \code{y} variable name, identifying what cell
values and parentheses indicate in the table. If unspecified, function uses
default labels based on \code{parenth}, e.g. M (SD) if \code{parenth = "sd"}.
Set to \code{"none"} for no text labels.
}
  \item{parenth.sep}{
Optional character specifying the separator between lower and upper bound of
confidence interval (when requested). Usually either \code{"-"} or \code{", "}
depending on user preference.
}
  \item{decimals}{
Number of decimal places for numeric values in the table (except p-values). If
unspecified, function uses 0 decimal places if the largest mean (in magnitude)
is in [1,000, Inf), 1 decimal place if [10, 1,000), 2 decimal places if
[0.1, 10), 3 decimal places if [0.01, 0.1), 4 decimal places if [0.001, 0.01), 5
decimal places if [0.0001, 0.001), and 6 decimal places if [0, 0.0001).
}
  \item{p.include}{
If \code{FALSE}, t-test is not performed and p-value is not returned.
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
If \code{FALSE}, column showing mean of \code{y} in full sample is suppressed.
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
  \item{fig}{
If \code{TRUE}, a figure is returned rather than a table. The figure shows mean
(95\% confidence interval) for each level of \code{x}.
}
  \item{fig.errorbars}{
Controls error bars around mean when \code{fig = TRUE}. Possible values are
\code{"sd"} for +/- 1 standard deviation, \code{"se"} for +/- 1 standard error,
\code{"t.ci"} for 95\% confidence interval based on t distribution,
\code{"z.ci"} for 95\% confidence interval based on z distribution, and
\code{"none"} for no error bars.
}
  \item{fig.title}{
Title of figure. If unspecified, title is set to \code{"Mean yname by xname"}.
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
If \code{x} has two levels, a t-test is used to test for a difference in means.
If \code{x} has more than two levels, a one-way analysis of variance is used to
test for a difference in means across the groups.

Both \code{x} and \code{y} can have missing values. The function drops
observations with missing \code{x} or \code{y}.
}
\value{
A character matrix with the requested table comparing mean \code{y} across
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
\code{\link{tabmedians}}\cr
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

# Compare mean BMI in control group vs. treatment group - table and figure
meanstable1 <- tabmeans(x = d$Group, y = d$BMI)
meansfig1 <- tabmeans(x = d$Group, y = d$BMI, fig = TRUE)

# Compare mean BMI by race - table and figure
meanstable2 <- tabmeans(x = d$Race, y = d$BMI)
meansfig2 <- tabmeans(x = d$Race, y = d$BMI, fig = TRUE)

# Compare mean baseline systolic BP across tertiles of BMI - table and figure
meanstable3 <- tabmeans(x = d$BMI, y = d$bp.1, yname = "Systolic BP",
                        quantiles = 3)
meansfig3 <- tabmeans(x = d$BMI, y = d$bp.1, quantiles = 3, fig = TRUE,
                      yname = "Systolic BP", xname = "BMI Tertile")

# Create single table comparing mean BMI and mean age in control vs. treatment
# group
meanstable4 <- rbind(tabmeans(x = d$Group, y = d$BMI),
                     tabmeans(x = d$Group, y = d$Age))

# An easier way to make the above table is to call the tabmulti function
meanstable5 <- tabmulti(dataset = d, xvarname = "Group",
                        yvarnames = c("BMI", "Age"))

# meanstable4 and meanstable5 are equivalent
all(meanstable4 == meanstable5)
}
\keyword{ table }
\keyword{ means }
\keyword{ t-test }
\keyword{ anova }
