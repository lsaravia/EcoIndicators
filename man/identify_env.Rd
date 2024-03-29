% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/identify_env.R
\name{identify_env}
\alias{identify_env}
\title{Identify the environment from new samples of community species}
\usage{
identify_env(com.to.identify, indicator.species, group, alfa = 0.05)
}
\arguments{
\item{com.to.identify}{A new sample or group of samples to identify the 
environment that they belong to.}

\item{indicator.species}{The indicator species such as that returned by
[select_indicator_species()]}

\item{group}{vector of the sample grouping of the original community.}

\item{alfa}{Significance level used for the test.}
}
\value{
A list with two components: a data.frame with the environment estimation 
value for each group and a vector with the belonging environment estimated. 
If two environments coincides in the max value then returns NA.
}
\description{
This function uses the result of [select_indicator_species()] to identify a new 
set of samples from the community
}
\examples{

data(soilandfauna)
 
# Select community (species) data

com <- soilandfauna[,18:60]

# Select grouping factor

group <- soilandfauna[,1]

# Select a subset of samples belonging to the same environment

subcom <- com[3:10,]

# Estimate the indicator species present

indicator.species <- select_indicator_species(com, group)

# Estimate the environment or group the samples belongs

identify_env(subcom, indicator.species, group)

}
