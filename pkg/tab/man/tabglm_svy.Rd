\name{tabglm.svy}
\alias{tabglm.svy}
\title{
Generate Summary Tables of Fitted Generalized Linear Models for Statistical
Reports (Survey Data)
}
\description{
This function takes an object returned from the \code{\link{svyglm}} function
and generates a clean summary table for a statistical report. Similar to
\code{\link{tabglm}}, but for survey data. Relies heavily on the \bold{survey}
package [1, 2].
}
\usage{
tabglm.svy(svyglmfit, latex = FALSE, xlabels = NULL, ci.beta = TRUE,
           inference = "wald.t", decimals = 2, p.decimals = c(2, 3),
           p.cuts = 0.01, p.lowerbound = 0.001, p.leading0 = TRUE,
           p.avoid1 = FALSE, basic.form = FALSE, intercept = TRUE, n = FALSE,
           events = FALSE, greek.beta = FALSE, binary.compress = TRUE,
           bold.colnames = TRUE, bold.varnames = FALSE, bold.varlevels = FALSE,
           predictor.colname = "Variable", print.html = FALSE,
           html.filename = "table1.html")
}
\arguments{
  \item{svyglmfit}{
An object returned from \code{\link{svyglm}} function call [1,2].
}
  \item{latex}{
If \code{TRUE}, object returned is formatted for printing in LaTeX using
\code{\link{xtable}} [3]; if \code{FALSE}, formatted for copy-and-pasting from
RStudio into a word processor.
}
  \item{xlabels}{
Optional character vector to label the \code{x} variables and their levels. If
unspecified, generic labels are used.
}
  \item{ci.beta}{
If \code{TRUE}, the table returned will include a column for Wald 95\%
confidence interval for the estimated regression coefficients.
}
 \item{inference}{
If \code{"wald.t"}, confidence intervals and p-values are based on t
distributions with degrees of freedom given by \code{svyglmfit$df.residual}; if
\code{"wald.z"}, confidence intervals and p-values are based on z distributions.
}
  \item{decimals}{
Number of decimal places for numeric values in the table (except p-values).
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
  \item{basic.form}{
If \code{TRUE}, there is no attempt to neatly format factor variables and their
levels, and the table returned is very similar to what you see when you run
\code{summary(svyglmfit)}.
}
  \item{intercept}{
If \code{FALSE}, the table returned will not include a row for the intercept.
}
  \item{n}{
If \code{TRUE}, the table returned will include a column for sample size.
}
  \item{events}{
If \code{TRUE}, the table returned will include a column for number of events
observed. Only meaningful when the outcome variable is binary.
}
  \item{greek.beta}{
If \code{TRUE}, column headings refer to regression parameters as Greek letter
beta rather than Beta. Only used when \code{latex = TRUE}.
}
  \item{binary.compress}{
If \code{TRUE}, only one row of the table is dedicated to parameter estimates
for each binary factor predictor. If \code{FALSE}, the table displays separate
rows for the variable name and the two levels for each binary factor predictor,
much like the presentation for factor variables with more than two levels.
}
  \item{bold.colnames}{
If \code{TRUE}, column headings are printed in bold font. Only applies if
\code{latex = TRUE}.
}
  \item{bold.varnames}{
If \code{TRUE}, variable names in the first column of the table are printed in
bold font. Only applies if \code{latex = TRUE}.
}
  \item{bold.varlevels}{
If \code{TRUE}, levels of each factor variable are printed in bold font. Only
applies if \code{latex = TRUE} and there is at least one factor variable
included as a predictor.
}
  \item{predictor.colname}{
Character string with desired column heading for the column of predictors.
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
The function should work well with categorical predictors (factors), provided
they are not ordered. For ordered factors, just convert to unordered before
creating the \code{\link{svyglm}} object to pass to \code{tabglm.svy}. Note that
you can define the levels of an unordered factor to control, which dictates
which level is used as the reference group in regression models. For example,
suppose a factor variable \code{x} takes values \code{"low"}, \code{"medium"},
and \code{"high"}. If you write \code{x = factor(x = x, levels = c("low",
"medium", "high"))}, then you can run \code{levels(x)} to see that the levels
are now arranged \code{"low"}, \code{"medium"}, \code{"high"}. It is still a
regular factor, but now if you use \code{x} as a predictor in a call to
\code{\link{svyglm}}, \code{"low"} will be the reference group.

Interaction and polynomial terms are compatible with \code{tabglm.svy}. If
interaction terms are included, the table will be formatted a little
differently. Basically including an interaction is equivalent to setting
\code{basic.form = TRUE}. All variable names and levels will be exactly as they
appear when you run \code{summary(svyglmfit)}, where \code{svyglmfit} is the
object returned from a call to \code{\link{svyglm}}.
}
\value{
A character matrix that summarizes the fitted generalized linear model. If
\code{latex = TRUE}, the character matrix will be formatted for inserting into a
Markdown/Sweave/knitr report using the \code{\link{xtable}} package [3].
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
Dane R. Van domelen
}
\note{
While \code{tabglm.svy} should work with any object generated by a call to
\code{\link{svyglm}}, not all possibilities have been tested. Therefore in
general I recommend always doing a quick check that the table created by
\code{tabglm.svy} matches the information in the \code{\link{svyglm}} object
itself.

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
\code{\link{tabmeans.svy}}\cr
\code{\link{tabmedians.svy}}\cr
\code{\link{tabmulti.svy}}
}
\examples{
NA
}
\keyword{ table }
\keyword{ generalized linear model }
