#' Calculate the probability of sagebrush establishment in response to
#' environmental variables in the year following seeding based on the
#' Shriver-2018 model
#'
#' @param Temp_mean A numerical vector; mean temperature from day 1 to 250
#'   in degree Celsius.
#' @param VWC_spring A numerical vector; mean soil moisture from day 70 to
#'   100 in the top soil layer (0–5 cm) in units of var{m3  m3}.
#'
#' @references Shriver, R. K., C. M. Andrews, D. S. Pilliod, R. S. Arkle,
#'   J. L. Welty, M. J. Germino, M. C. Duniway, D. A. Pyke, and
#'   J. B. Bradford. 2018. Adapting management to a changing world Warm
#'   temperatures, dry soil, and interannual variability limit restoration
#'   success of a dominant woody shrub in temperate drylands.
#'   Global Change Biology 244972–4982.
p_Shriver2018 <- function(Temp_mean, VWC_spring) {
  logit_p <- 3.306 + 2.499 * VWC_spring - 0.289 * Temp_mean
  stats::plogis(logit_p) # inverse logit
}

