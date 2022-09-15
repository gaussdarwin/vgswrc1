vg_pred <- function(psi_obs,
                    theta_obs,
                    psi_unit = "cmh2o",
                    theta_r = 0.001,
                    theta_s = 0.4,
                    alpha = 1,
                    n = 2,
                    plot = T) {

  if(!require(scales)){
    install.packages("scales")
    library(scales)
  }

  if(!require(tidyverse)){
    install.packages("tidyverse")
    library(tidyverse)
  }

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


  df <- data.frame(theta_obs, psi_obs)
  df2 <- data.frame(theta_obs = 0.01, psi_obs = 1500)

  df <- rbind(df2, df)

  if (psi_unit == "cmh2o") {
    df$psihcmh2o <- df$psi_obs
  }
  else{
    if (psi_unit == "kpa") {
      df$psihcmh2o <- df$psi_obs * 10.2
    }
    else{
      print(" /!\ Message : for 'psi_unit =' parameter, choose 'cmh2o' or 'kpa'. Default is 'cmh2o'.")
    }
  }


  fit_vg <-
    nls(
      formula = theta_obs ~ theta_r + ((theta_s - theta_r) / ((
        1 + (alpha * psihcmh2o) ^ n
      ) ^ (1 - (
        1 / n
      )))),
      data = df,
      start = c(
        theta_r = theta_r,
        theta_s = theta_s,
        alpha = alpha,
        n = n
      )
    )

  param <- coefficients(fit_vg)

  pred <- vg_eq(
    theta_r =  param[1],
    theta_s = param[2],
    alpha = param[3],
    n = param[4]
  )


  if (plot == T) {
    theme_srcw <- theme(
      axis.title = element_text(family = "sans", size = 15),
      axis.text = element_text(
        family = "sans",
        colour = "black",
        size = 12
      ),
      axis.ticks.x.bottom = element_line(size = .8),
      axis.ticks.y = element_line(size = .8),
      axis.line.x.bottom = element_line(colour = "black", size = .8),
      axis.line.y.left = element_line(colour = "black", size = .8),
      panel.background = element_rect(fill = "white"),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(size = 18),
      legend.key.width = unit(1.2, 'cm'),
      legend.key.height = unit(1.2, "cm"),
      legend.position = c(.8, .7),
      legend.key = element_rect(fill = "white"),
      legend.title = element_text(family = "sans", size = 13),
      legend.text = element_text(
        family = "sans",
        colour = "black",
        size = 11
      )
    )


    p <- ggplot(data = pred, aes(x = psikpa)) +
      geom_line(size = 1.3, aes(y = theta * 100), color = "blue") +
      geom_point(
        data = df[-1,],
        aes(x = psi_obs, y = theta_obs * 100),
        size = 3,
        color = "red"
      ) +
      scale_x_log10(labels = trans_format("log10", math_format(10 ^ .x))) +
      ggtitle("Van Genuchten SRCW") +
      ylab("\u03B8 (%)") +
      xlab("\u03A8 (KPa)") +
      annotation_logticks(base = 10,
                          sides = "b") +
      theme_srcw


    return(list(plot = p, data = pred))

  }
  else{
    return(pred)
  }

}



