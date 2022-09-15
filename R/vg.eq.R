

vg_eq <- function(theta_r = .001,
                  theta_s = 0.5,
                  alpha = 1,
                  n = 2) {
  pred.fits <- expand.grid(x = seq(0.01, 3000, length = 100000))

  colnames(pred.fits)[1] <- "psihcmh2o"

  attach(pred.fits)

  pred.fits$theta <-
    (theta_r + ((theta_s - theta_r) / ((
      1 + (alpha * psihcmh2o) ^ n
    ) ^ (1 - (
      1 / n
    )))))

  pred.fits$psikpa <- psihcmh2o * 0.0977

  detach(pred.fits)

  return(pred.fits)

}
