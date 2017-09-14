\name{tabmeans.svy}
\alias{tabmeans.svy}
\title{
Generate Summary Tables of Mean Comparisons for Statistical Reports (Survey
Data)
}
\description{
This function compares the mean of a continuous variable across levels of a
categorical variable and summarizes the results in a clean table for a
statistical report. Similar to \code{\link{tabmeans}}, but for survey data.
Relies heavily on the \bold{survey} package [1, 2].
}
\usage{
tabmeans.svy(x, y, svy, latex = FALSE, xlevels = NULL, yname = "Y variable",
             test = "Wald", decimals = 1, p.decimals = c(2, 3), p.cuts = 0.01,
             p.lowerbound = 0.001, p.leading0 = TRUE, p.avoid1 = FALSE,
             n.column = FALSE, n.headings = TRUE, bold.colnames = TRUE,
             bold.varnames = FALSE, variable.colname = "Variable",
             print.html = FALSE, html.filename = "table1.html")
}
\arguments{
  \item{svy}{
Survey design object created by a call to \code{\link{svydesign}} [1, 2].
}
  \item{x}{
Character string specifying categorical \code{x} variable name. Must match one
of \code{names(svy$variables)}.
}
  \item{y}{
Character string specifying continuous \code{y} variable name. Must match one of
\code{names(svy$variables)}.
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
Optional label for the continuous variable.
}
  \item{test}{
Either \code{"Wald"} for Wald test or \code{"LRT"} for likelihood ratio test to
test for equivalent mean \code{y} across levels of \code{x}.
}
  \item{decimals}{
Number of decimal places for means and standard deviations or standard errors.
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
  \item{n.column}{
If \code{TRUE}, the table will have a column for (unweighted) sample size.
}
  \item{n.headings}{
If \code{TRUE}, the table will indicate the (unweighted) sample size overall and
in each group in parentheses after the column headings.
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
NA
}
\value{
A character matrix with the requested table comparing mean \code{y} across
levels of \code{x}. If \code{latex = TRUE}, the character matrix will be
formatted for inserting into a Markdown/Sweave/knitr report using the
\code{\link{xtable}} package [3].
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
\code{\link{tabfreq}}\cr
\code{\link{tabmeans}}\cr
\code{\link{tabmedians}}\cr
\code{\link{tabmulti}}\cr
\code{\link{tabglm}}\cr
\code{\link{tabcox}}\cr
\code{\link{tabgee}}\cr
\code{\link{tabfreq.svy}}\cr
\code{\link{tabmedians.svy}}\cr
\code{\link{tabmulti.svy}}\cr
\code{\link{tabglm.svy}}
}
\examples{
NA
}
\keyword{ table }
\keyword{ means }
\keyword{ anova }
\keyword{ survey }
