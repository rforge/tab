\name{tabfreq}
\alias{tabfreq}
\title{
Generate Frequency Tables for Statistical Reports
}
\description{
This function creates an I-by-J frequency table and summarizes the results in a
clean table for a statistical report.
}
\usage{
tabfreq(x, y, latex = FALSE, xlevels = NULL, yname = NULL, ylevels = NULL,
        quantiles = NULL, quantile.vals = FALSE, cell = "n", parenth = NULL,
        text.label = NULL, parenth.sep = "-", test = "chi", decimals = NULL,
        p.include = TRUE, p.decimals = c(2, 3), p.cuts = 0.01,
        p.lowerbound = 0.001, p.leading0 = TRUE, p.avoid1 = FALSE,
        overall.column = TRUE, n.column = FALSE, n.headings = TRUE,
        compress = FALSE, compress.val = NULL, bold.colnames = TRUE,
        bold.varnames = FALSE, bold.varlevels = FALSE,
        variable.colname = "Variable", print.html = FALSE,
        html.filename = "table1.html")
}
\arguments{
  \item{x}{
Vector of values indicating group membership for columns of IxJ table.
}
  \item{y}{
Vector of values indicating group membership for rows of IxJ table.
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
  \item{ylevels}{
Optional character vector to label the levels of \code{y}. If unspecified, the
function uses the values that \code{y} takes on. Note that levels of \code{y}
will be listed in the order that they appear when you run \code{table(y, x)}.
}
  \item{quantiles}{
If specified, function compares distribution of the \code{y} variable across
quantiles of the \code{x} variable. For example, if \code{x} contains continuous
BMI values and \code{y} is race, setting \code{quantiles = 3} would result in
the distribution of race being compared across tertiles of BMI.
}
  \item{quantile.vals}{
If \code{TRUE}, labels for \code{x} show quantile number and corresponding range
of the \code{x} variable, e.g. Q1 [0.00, 0.25). If \code{FALSE}, labels for
quantiles just show quantile number, e.g. Q1. Only used if \code{xlevels} is not
specified.
}
  \item{cell}{
Controls what value is placed in each cell of the table. Possible choices are
\code{"n"} for counts, \code{"tot.percent"} for table percentage,
\code{"col.percent"} for column percentage, \code{"row.percent"} for row
percentage, \code{"tot.prop"} for table proportion, \code{"col.prop"} for column
proportion, \code{"row.prop"} for row proportion, \code{"n/totn"} for count/total
counts, \code{"n/coln"} for count/column count, and \code{"n/rown"} for
count/row count.
}
  \item{parenth}{
Controls what values (if any) are placed in parentheses after the values in each
cell. By default, if \code{cell} is \code{"n"}, \code{"n/totn"},
\code{"n/coln"}, or \code{"n/rown"} then the corresponding percentage is shown
in parentheses; if \code{cell} is \code{"tot.percent"}, \code{"col.percent"},
\code{"row.percent"}, \code{"tot.prop"}, \code{"col.prop"}, or \code{"row.prop"}
then a 95\% confidence interval for the requested percentage of proportion is
shown in parentheses. Possible values are \code{"none"}, \code{"se"} for
standard error of requested percentage or proportion, \code{"ci"} for 95\%
confidence interval for requested percentage of proportion, and
\code{"tot.percent"}, \code{"col.percent"}, \code{"row.percent"},
\code{"tot.prop"}, \code{"col.prop"}, and \code{"row.prop"} for various
percentages and proportions.
}
  \item{text.label}{
Optional text to put after the \code{y} variable name, identifying what cell
values and parentheses indicate in the table. If unspecified, function uses
default labels based on \code{cell} and \code{parenth}. Set to \code{"none"} for
no text labels.
}
  \item{parenth.sep}{
Optional character specifying the separator between lower and upper bound of
confidence interval (when requested). Usually either \code{"-"} or \code{", "}
depending on user preference.
}
  \item{test}{
Controls test for association between \code{x} and \code{y}. Use \code{"chi"}
for Pearson's chi-squared test, which is valid only in large samples;
\code{"fisher"} for Fisher's exact test, which is valid in small or large
samples; \code{"z"} for z test without continuity correction; or
\code{"z.continuity"} for z test with continuity correction. \code{"z"} and
\code{"z.continuity"} can only be used if \code{x} and \code{y} are binary.
}
  \item{decimals}{
Number of decimal places for values in table (no decimals are used for counts).
If unspecified, function uses 1 decimal for percentages and 3 decimals for
proportions.
}
  \item{p.include}{
If \code{FALSE}, statistical test is not performed and p-value is not returned.
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
  \item{overall.column}{
If \code{FALSE}, column showing distribution of \code{y} in full sample is
suppressed.
}
  \item{n.column}{
If \code{TRUE}, the table will have a column for sample size.
}
  \item{n.headings}{
If \code{TRUE}, the table will indicate the sample size overall and in each
group in parentheses after the column headings.
}
  \item{compress}{
If \code{y} has only two levels, setting compress to \code{TRUE} will produce a
single row rather than two rows. For example, if \code{y} is sex with 0 for
female, 1 for male, and \code{cell = "n"} and \code{parenth = "col.pecent"},
setting \code{compress = TRUE} will return a table with \code{n (percent)} for
males only. If \code{FALSE}, the table would show \code{n (percent)} for both
males and females, which is somewhat redundant.
}
  \item{compress.val}{
When \code{x} and \code{y} are both binary and \code{compress = TRUE},
\code{compress.val} can be used to specify which level of the \code{y} variable
should be shown. For example, if \code{x} is sex and \code{y} is obesity status
with levels \code{"Obese"} and \code{"Not Obese"}, setting
\code{compress = TRUE} and \code{compress.val = "Not Obese"} would result in the
table comparing the proportions of subjects that are not obese by sex.
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
If \code{TRUE}, levels of the y variable are printed in bold font. Only applies
if \code{latex = TRUE}.
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
NA
}
\value{
A character matrix with the requested frequency table. If \code{latex = TRUE},
the character matrix will be formatted for inserting into a
Markdown/Sweave/knitr report using \code{\link{xtable}} [1].
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
\code{\link{tabmeans}}\cr
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

# Compare sex distribution by group, with group as column variable
freqtable1 <- tabfreq(x = d$Group, y = d$Sex)

# Same comparison, but compress table to show Female row only, show percent (SE)
# rather than n (percent), and suppress (n = ) from column headings
freqtable2 <- tabfreq(x = d$Group, y = d$Sex, compress = TRUE,
                      compress.val = "Female", cell = "col.percent",
                      parenth = "se", n.headings = FALSE)

# Compare sex distribution by race, suppressing (n = ) from column headings and
# showing percent (95\% CI) rather than n (percent)
freqtable3 <- tabfreq(x = d$Race, y = d$Sex, n.headings = FALSE,
                      cell = "col.percent")

# Use rbind to create single table comparing sex and race in control vs.
# treatment group
freqtable4 <- rbind(tabfreq(x = d$Group, y = d$Sex),
                    tabfreq(x = d$Group, y = d$Race))

# A (usually) faster way to make the above table is to call the the tabmulti
# function
freqtable5 <- tabmulti(dataset = d, xvarname = "Group",
                       yvarnames = c("Sex", "Race"))

# freqtable4 and freqtable5 are equivalent
all(freqtable4 == freqtable5)
}
\keyword{ table }
\keyword{ frequency }
\keyword{ crosstab }
