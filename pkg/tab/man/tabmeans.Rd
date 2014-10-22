\name{tabmeans}
\alias{tabmeans}
\title{
Generate Summary Tables of Mean Comparisons for Statistical Reports 
}
\description{
This function compares the mean of a continuous variable across levels of a categorical variable and summarizes the results in a clean table (or figure) for a statistical report.
}
\usage{
tabmeans(x, y, latex = FALSE, xname = NULL, xlevels = NULL, yname = "Y variable", 
         quantiles = NULL, quantile.vals = FALSE, decimals = 1, p.include = TRUE, 
         p.decimals = c(2, 3), p.cuts = 0.01, p.lowerbound = 0.001, p.leading0 = TRUE, 
         p.avoid1 = FALSE, n = FALSE, se = FALSE, fig = FALSE)
}
\arguments{
  \item{x}{
Vector of values for the categorical x variable.
}
  \item{y}{
Vector of values for the continuous y variable.
}
  \item{latex}{
If TRUE, object returned is be formatted for printing in LaTeX using xtable [1]; if FALSE, it is formatted for copy-and-pasting from RStudio into a word processor.
}
  \item{xname}{
Label for the categorical variable. Only used if fig is TRUE.  
}
  \item{xlevels}{
Character vector of labels for levels of the x variable. If unspecified, the function uses the values that x takes on.
}
  \item{yname}{
Label for the continuous y variable.
}
  \item{quantiles}{
If specified, function compares means of the y variable across quantiles of the x variable. For example, if x contains continuous BMI values and y contains continuous HDL cholesterol levels, setting quantiles to 3 would result in mean HDL being compared across tertiles of BMI.
}
  \item{quantile.vals}{
If TRUE, labels for x show quantile number and corresponding range of the x variable. For example, Q1 [0.00, 0.25). If FALSE, labels for quantiles just show quantile number (e.g. Q1). Only used if xlevels is not specified.
}
  \item{decimals}{
Number of decimal places for means and standard deviations or standard errors.
}
  \item{p.include}{
If FALSE, t-test is not performed and p-value is not returned. 
}
  \item{p.decimals}{
Number of decimal places for p-values. If a vector is provided rather than a single value, number of decimal places will depend on what range the p-value lies in. See p.cuts.
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
  \item{n}{
If TRUE, the table will have a column for sample size.
}
  \item{se}{
If TRUE, the table will present mean (standard error) rather than mean (standard deviation).
}
  \item{fig}{
If TRUE, a figure is returned rather than a table. The figure shows mean (95 percent confidence interval) for each level of x.
}
}
\details{
If x has two levels, a t-test is used to test for a difference in means. An F test is first used to determine whether the equal variance or unequal variance t-test is appropriate. If x has more than two levels, a one-way analysis of variance is used to test for a difference in means across the groups.

Both x and y can have missing values. The function drops observations with missing x or y. 
}
\value{
A character matrix with the requested table comparing mean y across levels of x. If you click on the matrix name under "Data" in the RStudio Workspace tab, you will see a clean table that you can copy and paste into a statistical report or manuscript. If latex is set to TRUE, the character matrix will be formatted for inserting into an Sweave or Knitr report using the xtable package [1].
}
\references{
1. Dahl DB (2013). xtable: Export tables to LaTeX or HTML. R package version 1.7-1, \url{http://CRAN.R-project.org/package=xtable}.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
If you have suggestions for additional options or features, or if you would like some help using any function in the package tab, please e-mail me at vandomed@gmail.com. Thanks!
}
\seealso{
\code{\link{tabfreq}},
\code{\link{tabmedians}},
\code{\link{tabmulti}},
\code{\link{tabglm}},
\code{\link{tabcox}},
\code{\link{tabgee}},
\code{\link{tabfreq.svy}},
\code{\link{tabmeans.svy}},
\code{\link{tabglm.svy}},
}
\examples{
# Load in sample dataset d and drop rows with missing values
data(d)
d <- d[complete.cases(d), ]

# Create labels for group and race
groups <- c("Control", "Treatment")
races <- c("White", "Black", "Mexican American", "Other")

# Compare mean BMI in control group vs. treatment group - table and figure
meanstable1 <- tabmeans(x = d$group, y = d$bmi, xlevels = groups, yname = "BMI")
meansfig1 <- tabmeans(x = d$group, y = d$bmi, xlevels = groups, yname = "BMI", fig = TRUE)

# Compare mean BMI by race - table with sample size and figure
meanstable2 <- tabmeans(x = d$race, y = d$bmi, xlevels = races, yname = "BMI", n = TRUE)
meansfig2 <- tabmeans(x = d$race, y = d$bmi, xname = "Race", xlevels = races, 
                      yname = "BMI", fig = TRUE)

# Compare mean baseline systolic BP across tertiles of BMI - table and figure
meanstable3 <- tabmeans(x = d$bmi, y = d$bp.1, yname = "Systolic BP", quantiles = 3)
meansfig3 <- tabmeans(x = d$bmi, y = d$bp.1, xname = "BMI Tertile", yname = "Systolic BP",
                      quantiles = 3, fig = TRUE)

# Create single table comparing mean BMI and mean age in control vs. treatment group
meanstable4 <- rbind(tabmeans(x = d$group, y = d$bmi, xlevels = groups, yname = "BMI"),
                     tabmeans(x = d$group, y = d$age, xlevels = groups, yname = "Age"))
                     
# An easier way to make the above table is to call the tabmulti function
meanstable5 <- tabmulti(dataset = d, xvarname = "group", yvarnames = c("bmi", "age"),
                        xlevels = groups, ynames = c("BMI", "Age"))
                        
# meanstable4 and meanstable5 are equivalent
all(meanstable4 == meanstable5)

# Click on meanstable 1, ... , meanstable5 in the Workspace tab of RStudio to see the 
# tables that could be copied and pasted into a report. Alternatively, setting the latex 
# input to TRUE produces tables that can be inserted into LaTeX using the xtable package.
}
\keyword{ table }
\keyword{ means }
\keyword{ t-test }
\keyword{ anova }