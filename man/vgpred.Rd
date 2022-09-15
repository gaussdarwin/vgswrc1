\name{vg_pred}
\alias{vg_pred}
\title{
Fit and Van Genuchten's Soil Water Retention Curve on observed values with the least square method
}
\description{
The function helps you to fit a soil water retention curve with Van Genuchten equation on your observed values with the least square method.
}
\usage{
vg_pred(psi_obs, theta_obs, psi_unit, theta_r, theta_s, alpha, n, plot)
}

\arguments{
  \item{psi_obs}{ A vector of values for water pressure head (in cmH2O or in KPa)
}

  \item{theta_obs}{ A vector of the volumetric water retention values (in ratio, not percentage)
}

  \item{psi_unit}{ The unit for water pressure head values, choose between 'cmh2o' or 'kpa'
}

  \item{theta_r}{ The starting iteration value for the residual volumetric water retention fo the material (default = 0.001)
}

  \item{theta_s}{ The starting iteration value for the saturated volumetric water retention fo the material (default = 0.4)
}

  \item{alpha}{ The starting iteration value for the 'alpha' parameter in the Van Genuchten Equation (default is 1)
}

  \item{n}{ The starting iteration value for the 'n' parameter in the Van Genuchten Equation (default is 2)
}

  \item{plot}{ Logical argument to return a plot of the soil water retention curve or only the predicted data (default is 'TRUE')
}
}

\author{
Simon Taurines,

PhD student in mineral engineering at Universite du Quebec en Abitibi Temiscamingue.

mail : simon.taurines@uqat.ca
}
\examples{
## NOT RUN
## vg_pred(psi_obs, theta_obs, ...)
}
