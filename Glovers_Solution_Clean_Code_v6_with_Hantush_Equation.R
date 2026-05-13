
library(tidyverse)
library(plotly)
library(statmod)
library(EstimationTools)
library(mosaicCalc)

bgs <-        15 # placeholder to allow for starting depth of water table in ft below ground surface

rechargeR <-  2 # ft/day
storageC <-   0.2 # unitless
transM <-     2500 # sqft/day
bWidth <-     200 # ft
bLength <-    200 # ft
distance <-   seq(-500, 500, by = 50) # ft
xvar <-       sqrt((distance^2)/2)
yvar <-       xvar
time <-       30 # days
e <-          exp(1) # Euler's number

uvar1 <- (xvar - (bWidth / 2)) / ((4 * transM) / (storageC * time))
uvar2 <- (xvar + (bWidth / 2)) / ((4 * transM) / (storageC * time))
uvar3 <- (yvar - (bLength / 2)) / ((4 * transM) / (storageC * time))
uvar4 <- (yvar + (bLength / 2)) / ((4 * transM) / (storageC * time))

uvar <- cbind(uvar1,uvar2,uvar3,uvar4) # matrix of uvar vectors
uvar_pairs <- list(uvar[,c(2,4)],uvar[,c(2,3)],uvar[,c(1,4)],uvar[,c(1,3)]) # list of paired vectors for each integral in equation (2)

ints <- list() # Empty list for M* integrals
resultH <- c()
final <- data.frame(distance) # Empty vector for solution

for(z in 1:nrow(uvar_pairs[[1]])){ # Evaluates Hantush's whole solution z times based on number of distance points

  for(q in 1:length(uvar_pairs)){  # Evaluates Hantush's general integral 4 times, once for each ui,uj pair

    uvari <- uvar_pairs[[q]][[z,1]] # ui = row z, first column, pair q
    uvarj <- uvar_pairs[[q]][[z,2]] # uj = row z, second column, pair q

    #print(c("uvari" = uvari, "uvarj" = uvarj))

    # M* Evaluation #
    nodes <- c(0.238619186, 0.661209386, 0.932469514, -0.932469514, -0.661209386, -0.238619186)
    weights <- c(0.467913935, 0.360761573, 0.171324492, 0.171324492, 0.360761573, 0.467913935)

    Mstar = function(ui,uj){ # Mstar function will be evaluated as M*(ui,uj) and as M*(uj,ui) in Hantush's general integral each time
      if (ui == 0) return(0) # Prevent division by 0
      # Mstar = uj / (pi * ui)
      Mstar <- ui / pi
      resultM <- 0

      for (k in 1:length(nodes)) {
        Ak <- nodes[k]
        Vk <- weights[k]

        r_k <- (Ak + 1) * (uj / (2 * ui))

        term_k <- (exp((-uj^2) * (1 + r_k^2)) / (1 + r_k^2)) * Vk

        resultM <- resultM + term_k
      }
      #print(Mstar * resultM)
      return(Mstar * resultM)
    }

    # W(u) Evaluation #
    wellf <- function(u){
      if(u<=1){ # when u <=1, otherwise when 1 <= u <= inf
        if (u <= 0) return(0)  # Avoid log of non-positive numbers
        a0 <- -0.57721566;
        a1 <- 0.99999193;
        a2 <- -0.24991055;
        a3 <- 0.05519968;
        a4 <- -0.00976004;
        a5 <- 0.00107857;
        return(a0 - log(u) + (a1 * u) + (a2 * u^2) + (a3 * u^3) + (a4 * u^4) + (a5 * u^5))
      } else {
        b1 <- 8.57332874;
        b2 <- 18.0590170;
        b3 <- 8.63476089;
        b4 <- 0.267773734;
        c1 <- 9.57332235;
        c2 <- 25.6329561;
        c3 <- 21.0996531;
        c4 <- 3.95849692;
        return((1/(u * exp(u))) * (((u^4) + (b1 * u^3) + (b2 * u^2) + (b3 * u) + b4) / ((u^4) + (c1 * u^3) + (c2 * u^2) + (c3 * u) + c4)))
      }
    }

    # erf() Evaluation #
    erf <- function(uraw){
      sign <- ifelse(uraw < 0, -1, 1)
      u <- abs(uraw)
      pvar <- 0.3275911;
      bvar <- 1/(1 + (pvar * u));
      e1 <- 0.254829592;
      e2 <- -0.284496736;
      e3 <- 1.421413741;
      e4 <- -1.453152027;
      e5 <- 1.06140543;
      return(sign * (1 - ((e1 * bvar) + (e2 * bvar^2) + (e3 * bvar^3) + (e4 * bvar^4) + (e5 * bvar^5)) * e^(-u^2))) # Order of 10^-7
    }

    # Perform Hantush's integration by parts #

    ints[[q]] <- erf(uvari) * erf(uvarj) + (4 / pi) * uvari * uvarj * wellf((uvari^2) + (uvarj^2)) + (2 / sqrt(pi)) * (uvari * e^(-uvari^2) * erf(uvarj) + uvarj * e^(-uvarj^2) * erf(uvari)) - 2 * ((uvari^2) * Mstar(uvarj / uvari, uvari^2) + (uvarj^2) * Mstar(uvari / uvarj, uvarj^2))
  }
  temp <- ((rechargeR * time) / (4 * storageC)) * (ints[[1]] - ints[[2]] - ints[[3]] + ints[[4]])
  # Print-outs of data used in calculations for debugging
  cat("Distance (ft):",xvar[z],"\n")
  cat(" u1 =", uvar1[z], "u2 =", uvar2[z], "u3 =", uvar3[z], "u4 =", uvar4[z], "\n")
  cat("erf(u) =",erf(uvar1[z]),erf(uvar2[z]),erf(uvar3[z]),erf(uvar4[z]),"\n")
  cat(" M*(ui,uj) =",Mstar(uvar2[z],uvar4[z]),Mstar(uvar2[z],uvar3[z]),Mstar(uvar1[z],uvar4[z]),Mstar(uvar1[z],uvar3[z]),"\n")
  cat(" M*(uj,ui) =",Mstar(uvar4[z],uvar2[z]),Mstar(uvar3[z],uvar2[z]),Mstar(uvar4[z],uvar1[z]),Mstar(uvar3[z],uvar1[z]),"\n")
  cat("ui^2 + uj^2 =",uvar2[z]^2 + uvar4[z]^2,uvar2[z]^2 + uvar3[z]^2,uvar1[z]^2 + uvar4[z]^2,uvar1[z]^2 + uvar3[z]^2,"\n")
  cat("W(ui^2 + uj^2) =",wellf(uvar2[z]^2 + uvar4[z]^2),wellf(uvar2[z]^2 + uvar3[z]^2),wellf(uvar1[z]^2 + uvar4[z]^2),wellf(uvar1[z]^2 + uvar3[z]^2),"\n")
  cat(" Integrals:",ints[[1]],ints[[2]],ints[[3]],ints[[4]],"\n")
  cat(" Height (ft):",temp,"\n","\n")
  resultH <- append(resultH, temp)
}

final <- cbind(final, resultH)
final

# final %>% # Plot mound relative to water table and ground surface
#   ggplot() +
#   geom_line(aes(distance, resultH), linewidth = 1) +
#   labs(
#     x = "Distance (ft)",
#     y = "Mounding (ft)"
#   ) +
#   geom_hline(yintercept = bgs, color = "#0e7446", linewidth = 1) + # Ground surface
#   geom_hline(yintercept = bgs - 4, color = "#ee3b33", linetype = "dashed", linewidth = 1) + # Limit of mounding
#   geom_hline(yintercept = 0, color = "#0f75bc", linewidth = 1) + # Existing water table
#   xlim(min(distance), max(distance)) +
#   ylim(0, bgs) +
#   theme_bw()

library(plotly)

final %>%
  plot_ly(x = ~distance, y = ~resultH, type = "scatter", mode = "lines",
          line = list(width = 2, color = "black"), name = "Mounding",
          hovertemplate = "Distance: %{x} ft<br>Mounding: %{y:.2f} ft<extra></extra>") %>%
  layout(
    title = list(
      text = paste0("<b>Groundwater Mounding after ", time," days</b><br>")
    ),
    xaxis = list(
      title = "Distance (ft)",
      range = c(min(final$distance), max(final$distance))
    ),
    yaxis = list(
      title = "Mounding (ft)",
      range = c(0, bgs)
    ),
    shapes = list(
      # Ground surface line
      list(type = "line", x0 = 0, x1 = 1, xref = "paper",
           y0 = bgs, y1 = bgs, line = list(color = "#0e7446", width = 2)),
      # Limit of mounding line
      list(type = "line", x0 = 0, x1 = 1, xref = "paper",
           y0 = bgs - 4, y1 = bgs - 4, line = list(color = "#ee3b33", width = 2, dash = "dash")),
      # Existing water table line
      list(type = "line", x0 = 0, x1 = 1, xref = "paper",
           y0 = 0, y1 = 0, line = list(color = "#0f75bc", width = 2))
    )
  )


################################################################################
# Use direct integration instead of approximations #

# dummyTau = 1
# height = c()
#
# for(z in 1:nrow(uvar_pairs[[1]])){
#   integrand1 = (erf(uvar2[z])/sqrt(dummyTau))*(erf(uvar4[z])/sqrt(dummyTau))*deriv(dummyTau, c("dummyTau"))
#   int1 = integrate(integrand1, lower = 0, upper = 1)$value
#
#   integrand2 = (erf(uvar2[z])/sqrt(dummyTau))*(erf(uvar3[z])/sqrt(dummyTau))*deriv(dummyTau, c("dummyTau"))
#   int2 = integrate(integrand2, lower = 0, upper = 1)$value
#
#   integrand3 = (erf(uvar1[z])/sqrt(dummyTau))*(erf(uvar4[z])/sqrt(dummyTau))*deriv(dummyTau, c("dummyTau"))
#   int3 = integrate(integrand3, lower = 0, upper = 1)$value
#
#   integrand4 = (erf(uvar1[z])/sqrt(dummyTau))*(erf(uvar3[z])/sqrt(dummyTau))*deriv(dummyTau, c("dummyTau"))
#   int4 = integrate(integrand4, lower = 0, upper = 1)$value
#
#   height[z] = ((rechargeR*time)/(4*storageC))*(int1 - int2 - int3 + int4)
# }
# height







