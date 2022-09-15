\name{vg_eq}
\alias{vg_eq}
\title{
Van Genuchten equation function
}
\description{ Predict volumetric water content and pressure head values from Van Genuchten's non linear equation parameters
}
\usage{
vg_eq(theta_r, theta_s, alpha, n)
}
\arguments{
  \item{theta_r}{ The residual volumetric water retention fo the material (default is 0.001)
}

  \item{theta_s}{ The saturated volumetric water retention fo the material (default is 0.5)
}

  \item{alpha}{ The 'alpha' parameter in the Van Genuchten Equation (default is 1)
}

  \item{n}{ The 'n' parameter in the Van Genuchten Equation (default is 2)
}

}
\author{
Simon Taurines, UQAT
}
\examples{

vg_eq()

}
