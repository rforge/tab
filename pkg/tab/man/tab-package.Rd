\name{tab-package}
\alias{tab-package}
\alias{tab}
\docType{package}
\title{
Functions for Creating Summary Tables for Statistical Reports
}
\description{
Contains functions for generating tables for statistical reports written in Microsoft Word or LaTeX. There are functions for I-by-J frequency tables, comparison of means or medians across levels of a categorical variable, and summarizing fitted generalized linear models, generalized estimating equations, and Cox proportional hazards regression. Functions are available to handle data simple random samples or survey data. The package is intended to make it easier for researchers to translate results from statistical analyses in R to their reports or manuscripts.
}
\details{
\tabular{ll}{
Package: \tab tab\cr
Type: \tab Package\cr
Version: \tab 3.1.3\cr
Date: \tab 2016-10-10\cr
License: \tab GPL-2\cr
}
The following functions are included:

\code{\link{tabfreq}},
\code{\link{tabmeans}},
\code{\link{tabmedians}},
\code{\link{tabmulti}},
\code{\link{tabglm}},
\code{\link{tabcox}},
\code{\link{tabgee}},
\code{\link{tabfreq.svy}},
\code{\link{tabmeans.svy}},
\code{\link{tabmedians.svy}},
\code{\link{tabmulti.svy}},
\code{\link{tabglm.svy}},
\code{\link{formatp}}
}
\author{
Dane R. Van Domelen

Maintainer: Dane R. Van Domelen <vandomed@gmail.com>
}
\references{
1. Therneau T (2013). A Package for Survival Analysis in S. R package version 2.37-4, \url{https://cran.r-project.org/package=survival}.

2. Terry M. Therneau and Patricia M. Grambsch (2000). Modeling Survival Data: Extending the Cox Model.
Springer, New York. ISBN 0-387-98784-3.

3. Dahl DB (2013). xtable: Export tables to LaTeX or HTML. R package version 1.7-1, \url{https://cran.r-project.org/package=xtable}.

4. Kevin Ushey (2013). Kmisc: Kevin Miscellaneous. R package version 0.5.0. \url{https://CRAN.R-project.org/package=Kmisc}.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\keyword{ package }
\seealso{
NA
}
\examples{
# Load in sample dataset d and drop rows with missing values
data(d)
d <- d[complete.cases(d), ]

# Compare race distribution by group, with group as column variable
freqtable <- tabfreq(x = d$Group, y = d$Race)

# Compare mean BMI in control group vs. treatment group
meanstable <- tabmeans(x = d$Group, y = d$BMI)

# Generate plot comparing mean BMI in control group vs. treatment group
meansfig <- tabmeans(x = d$Group, y = d$BMI, fig = TRUE)

# Compare median BMI in control group vs. treatment group
medianstable <- tabmedians(x = d$Group, y = d$BMI)

# Create a typical Table 1 for statistical report or manuscript
table1 <- tabmulti(dataset = d, xvarname = "Group",
                   yvarnames = c("Age", "Sex", "Race", "BMI"))
                   
# Create vector of race labels for use in regression tables
races <- c("White", "Black", "Mexican American", "other")

# Test whether age, sex, race, and treatment group are associated with BMI
glmfit1 <- glm(BMI ~ Age + Sex + Race + Group, data = d)
lintable <- tabglm(glmfit = glmfit1, 
                   xlabels = c("Intercept", "Age", "Male", "Race", races, "Treatment"))

# Test whether age, sex, race, and treatment group are associated with 1-year mortality
glmfit2 <- glm(death_1yr ~ Age + Sex + Race + Group, data = d, family = binomial)
logtable <- tabglm(glmfit = glmfit2, ci.beta = FALSE,
                   xlabels = c("Intercept", "Age", "Male", "Race", races, "Treatment"))

# Test whether age, sex, race, and treatment group are associated with survival
coxtable <- tabcox(x = d[,c("Age", "Sex", "Race", "Group")], time = d$time, 
                   delta = d$delta, 
                   xlabels = c("Age", "Male", "Race", races, "Treatment"))
                   
# To move freqtable into Word, run write.cb(freqtable) to copy the table onto your
# clipboard. Paste into Word, highlight the table and go to Insert - Table - Convert Text 
# to Table... OK. Alternatively, if you set print.html to TRUE, the function will write 
# a html file named html.filename to your current working directory. You can open this 
# file, copy the table, and paste it into Word.

# If you wish to use LaTeX, R Markdown, knitr, Sweave, etc., please see the package 
# vignette for examples. In most cases, you have to set the latex input to TRUE and then 
# use the xtable package [3].
}