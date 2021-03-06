\name{tabcox}
\alias{tabcox}
\title{
Generate Summary Tables of Fitted Cox Proportional Hazards Regression Models for
Statistical Reports
}
\description{
This function performs Cox proportional hazards regression using the R package
\code{\link{survival}} [1, 2] and summarizes the results in a clean table for a
statistical report.
}
\usage{
tabcox(x, time, delta, latex = FALSE, xlabels = NULL, cluster = NULL,
       robust.se = TRUE, decimals = 2, p.decimals = c(2, 3), p.cuts = 0.01,
       p.lowerbound = 0.001, p.leading0 = TRUE, p.avoid1 = FALSE, n = FALSE,
       events = FALSE, coef = "n", greek.beta = FALSE, binary.compress = TRUE,
       bold.colnames = TRUE, bold.varnames = FALSE, bold.varlevels = FALSE,
       predictor.colname = "Variable", suppress.beta = FALSE,
       print.html = FALSE, html.filename = "table1.html")
}
\arguments{
  \item{x}{
For single predictor, vector of values; for multiple predictors, data frame or
matrix with one column per predictor. Categorical variables should be of class
"factor."
}
  \item{time}{
Numeric values for time to event or censoring.
}
  \item{delta}{
Indicator variable where 1 = event observed, 0 = censored.
}
  \item{latex}{
If \code{TRUE}, object returned is formatted for printing in LaTeX using
\bold{xtable} [3]; if \code{FALSE}, formatted for copy-and-pasting from RStudio
into a word processor.
}
  \item{xlabels}{
Optional character vector to label the \code{x} variables and their levels. If
unspecified, generic labels are used.
}
  \item{cluster}{
Optional vector indicating clusters of subjects.
}
  \item{robust.se}{
Only a valid option if \code{cluster} is specified. In that case, setting to
\code{TRUE} requests robust sandwich method standard errors, while setting to
\code{FALSE} requests normal standard errors.
  }
  \item{decimals}{
Number of decimal places for the regression coefficients, standard errors,
hazard ratios, and confidence intervals.
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
  \item{n}{
If \code{TRUE}, the table returned will include a column for sample size.
}
  \item{events}{
If \code{TRUE}, the table returned will include a column for number of events
observed (i.e. uncensored observations).
}
  \item{coef}{
If set to \code{"x"}, function will standardize all variables in \code{x} that
are continuous, providing standardized regression coefficients. Then, the
interpretation of each hazard ratio is the hazard ratio associated with a one
standard deviation increase in the predictor.
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
\item{suppress.beta}{
If \code{FALSE}, the Beta (SE) column is not included. May often be preferred
since the point and interval estimate for the hazard ratio contains the same
information, but are easier to interpret.
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
A character matrix that summarizes the fitted Cox PH regression model. If
\code{latex = TRUE}, the character matrix will be formatted for inserting into
a Markdown/Sweave/knitr report using the \code{\link{xtable}} package [3].
}
\references{
1. Therneau T (2013). A Package for Survival Analysis in S. R package version
2.37-4, \url{https://cran.r-project.org/package=survival}.

2. Terry M. Therneau and Patricia M. Grambsch (2000). Modeling Survival Data:
Extending the Cox Model. Springer, New York. ISBN 0-387-98784-3.

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
I may eventually write a function to create a summary table based on an object
from \code{coxph} rather than the data vectors themselves (like how
\code{tabglm} works).

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
\code{\link{coxph}}\cr
\code{\link{tabfreq}}\cr
\code{\link{tabmeans}}\cr
\code{\link{tabmedians}}\cr
\code{\link{tabmulti}}\cr
\code{\link{tabglm}}\cr
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

# Create labels for race levels
races <- c("White", "Black", "Mexican American", "Other")

# Test whether race is associated with survival
coxtable1 <- tabcox(x = d$Race, time = d$time, delta = d$delta,
                    xlabels = c("Race", races))

# Test whether age, sex, race, and treatment group are associated with survival
coxtable2 <- tabcox(x = d[,c("Age", "Sex", "Race", "Group")], time = d$time,
                    delta = d$delta,
                    xlabels = c("Age", "Male", "Race", races, "Treatment"))
}
\keyword{ table }
\keyword{ regression }
\keyword{ Cox proportional hazards}
