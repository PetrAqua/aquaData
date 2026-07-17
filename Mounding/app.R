# Groundwater Mounding Program as Adapted from Sunada's BASIC Microcomputer Program for the R Shiny Application.
# Version -> May 27, 2026 by Luke Hudson
# WORK IN PROGRESS TO ADD SUBUNITS TO BASIN AND FRACTION OF UNIT AREA AS TRENCH AREA

library(shiny)
library(bslib)
library(bsicons)
library(devtools)
library(htmlwidgets)
library(bslib)
library(tidyverse)
library(plotly)
library(statmod)
library(EstimationTools)
library(mosaicCalc)

ui <- page_sidebar(
  title = "Groundwater Mounding Model",
  sidebar = sidebar(width = "40%",
                    card(
                      card_header("Recharge Information"),
                      layout_columns(
                        card(numericInput("rechargeRate", label = tagList(
                          "Recharge Rate (ft/day):",
                          popover(
                            bs_icon("question-circle"),
                            title = "Calculate the Recharge Rate",
                            "The recharge or hydraulic loading rate is a function of the discharge volume and the discharge area.\n
        Provide the flow in gpd to calculate the recharge rate in ft/day for the current recharge area:",
        numericInput("dischargeRate", "Discharge Rate (gpd):", FALSE),
        actionButton("calculateRate", "Calculate")
                          )), 0.0334)
                        ),
        card(numericInput("rechargePeriod", label = tagList(
          "Recharge Period (days):",
          popover(
            bs_icon("question-circle"),
            title = "Calculate the Recharge Period",
            "The recharge period is the amount of time for which the aquifer recharge will be simulated.\n
        You may provide the recharge period in years to convert to days:",
        numericInput("rechargeYears", "Recharge Period (years):", FALSE),
        actionButton("calculatePeriod", "Calculate")
          )), 7305))
                      ),
        layout_columns(
          card(numericInput("basinX", label = tagList(
            "Basin Width (ft):",
            popover(
              bs_icon("question-circle"),
              title = "Basin Width",
              "The width of the rectangular recharge basin."
            )), 200)),
          card(numericInput("basinY", label = tagList(
            "Basin Length (ft):",
            popover(
              bs_icon("question-circle"),
              title = "Basin Length",
              "The length of the rectangular recharge basin."
            )), 200))),
        layout_columns(
          card(checkboxInput("useStream", label = tagList(
            "Stream in Vicinity",
            popover(
              bs_icon("question-circle"),
              title = "Stream in Vicinity",
              "Calculate groundwater mounding with a nearby stream functioning as a constant-head boundary. Assumes stream is perpendicular to basin width (x-axis)."
            )), FALSE),
          conditionalPanel(
            condition = "input.useStream == true",
            numericInput(
              inputId = "streamDistance",
              label = "Distance from Basin Center to Stream (ft):",
              value = 150,
              min = 10
            )
          )),
          card(checkboxInput("useSubunits", label = tagList(
            "Basin Subunits",
            popover(
              bs_icon("question-circle"),
              title = "Basin Subunits",
              "Calculate groundwater mounding for a basin with separate subunits. 
              Assumes subunits are parallel with optional separation along the width of the basin. 
              Define the spacing between each subunit and the total fraction of each subunit serving as the trench area."
            )), FALSE),
            conditionalPanel(
              condition = "input.useSubunits == true",
              numericInput(
                inputId = "subUnits",
                label = "Number of Subunits in Basin:",
                value = 1
              ),
              numericInput(
                inputId = "subSpace",
                label = "Distance Between Each Subunit (ft):",
                value = 0
              ),
              numericInput(
                inputId = "fTrench",
                label = "Fraction of Subunits as Trench Area:",
                value = 1.0,
                max = 1.0
              )
            ))
          )
                    ),
        card(
          card_header("Aquifer Information"),
          layout_columns(
            card(numericInput("transmissivity", label = tagList(
              "Transmissivity (sqft/day):",
              popover(
                bs_icon("question-circle"),
                title = "Calculate the Transmissivity",
                "The transmissivity is a function of the hydraulic conductivity (K) and the saturated aquifer thickness.\n
        Provide the conductivity in ft/day and the saturated aquifer thickness in ft to calculate the transmissivity in sqft/day:",
        numericInput("conductivity", "Conductivity (ft/day):", FALSE),
        numericInput("thickness", "Aquifer Thickness (ft):", FALSE),
        actionButton("calculateTrans", "Calculate")
              )), 2500)),
        card(numericInput("specificYield", label = tagList(
          "Specific Yield:",
          popover(
            bs_icon("question-circle"),
            title = "Specific Yield",
            "The specific yield, or storage coefficient, is the volume of water released from a known volume of saturated soil under the force of gravity and inherent soil tension.\n
        It is expressed as a decimal between 0 and 1."
          )), 0.2))),
        card(numericInput("waterDepth", label = tagList(
          "Depth to Water (ft):",
          popover(
            bs_icon("question-circle"),
            title = "Depth to Water",
            "The depth of the existing water table below the ground surface."
          )), 15))
        ),
        card(
          card_header("Mounding Information"),
          layout_columns(
            card(numericInput("moundRadius", label = tagList(
              "Mound Radius (ft):",
              popover(
                bs_icon("question-circle"),
                title = "Mound Radius",
                "The radius of groundwater mounding that will be simulated."
              )), 500)),
            card(numericInput("radiusIncrement", label = tagList(
              "Radius Increment (ft):",
              popover(
                bs_icon("question-circle"),
                title = "Radius Increment",
                "The horizontal distance between each data point of the simulated mound."
              )), 50)))
        )

  ),
  navset_card_underline(
    id = "my_tabs",
    nav_panel("Mounding Profile",
              value = "moundingProfile",
              plotlyOutput("moundPlot2D")),
    nav_panel("3D Mounding",
              value = "mounding3D",
              plotlyOutput("moundPlot3D")),
    nav_panel("Model Info",
              value = "moundingInfo",
              card(
                tags$div("This R Shiny application is based on Molden, Sunada, and Warner's 1984 BASIC program for use on the APPLE II+ 48K microcomputer,
                        which evaluates Glover's (1960) Solution for Constant Recharge from a Rectangular Basin.
                        The integral in Glover's Solution is evaluated using Hantush's (1967) Method of integration by parts.
                        Expressions for the error and well functions are used.
                        Improvements have been made to allow for three-dimensional modeling.",
                        tags$blockquote("Molden, D., D. K. Sunada, and J. W. Warner.
                                        “Microcomputer Model of Artificial Recharge Using Glover's Solution.” ",
                                        tags$em("Ground Water. Vol. 22,")," 1984.")),
                tags$h5("Glover's Solution for Constant Recharge from a Rectangular Basin"),
                tags$div(withMathJax("$$
                                    H = \\frac{Rt}{4S}~\\int_{0}^1 (erf\\frac{u_2}{\\sqrt{}\\tau}-erf\\frac{u_1}{\\sqrt{}\\tau})(erf\\frac{u_4}{\\sqrt{}\\tau}-erf\\frac{u_3}{\\sqrt{}\\tau})~d\\tau~~~~~(1)
                                    $$")),
                tags$h5("where"),
                tags$div(withMathJax("$$
                                    u_1 = (\\frac{x - W/2}{4T/St})~~~~~~~~~~u_2 = (\\frac{x + W/2}{4T/St})\\\\
                                    u_3 = (\\frac{y - W/2}{4T/St})~~~~~~~~~~u_4 = (\\frac{y + W/2}{4T/St})
                                    $$")),
                tags$h5("and"),
                tags$div("H = mound height (L),", tags$br(),
                         "R = recharge rate (L/T),", tags$br(),
                         "S = storage coefficient (dimensionless),", tags$br(),
                         "T = transmissivity (L2/T),", tags$br(),
                         "W = basin width (L),", tags$br(),
                         "L = basin length (L),", tags$br(),
                         "x,y = Cartesian coordinates (L),", tags$br(),
                         "t = time (T),", tags$br(),
                         HTML("&#120591; = dummy variable of integration,"), tags$br(), # Mathematical italic small tau
                         "erf(u) = error function."),
                tags$h5("After performing the multiplication indicated in (1), Glover's Solution is written as"),
                tags$div(withMathJax("$$
                                    H = \\frac{Rt}{4S} [
                                    ~\\int_{0}^1 erf\\frac{u_2}{\\sqrt\\tau} ~ erf\\frac{u_4}{\\sqrt\\tau} -
                                    \\int_{0}^1 erf\\frac{u_2}{\\sqrt\\tau} ~ erf\\frac{u_3}{\\sqrt\\tau}\\\\ -
                                    \\int_{0}^1 erf\\frac{u_1}{\\sqrt\\tau} ~ erf\\frac{u_4}{\\sqrt\\tau} -
                                    \\int_{0}^1 erf\\frac{u_1}{\\sqrt\\tau} ~ erf\\frac{u_3}{\\sqrt\\tau}~]
                                    ~~~~~(2)
                                    $$")),
                tags$h5("Hantush shows that the integrals in (2) can be evaluated as"),
                tags$div(withMathJax("$$
                                    \\int_{0}^1 erf\\frac{u_i}{\\sqrt\\tau}erf\\frac{u_j}{\\sqrt\\tau}d\\tau = \\\\
                                    erf(u_i)erf(u_j) + (4/\\pi)~u_i~u_j~W(u_i^2~u_j^2) \\\\
                                    + (2/\\sqrt\\pi)~[~u_i~e^{-u_i^2} erf(u_j)+u_j~e^{-u_j^2} erf(u_i)~] \\\\
                                    - 2~[~u_i^2M^*(u_i,u_j)+u_j^2M^*(u_j,u_i)~]~~~~~(3)
                                    $$")),
                tags$h5("where"),
                tags$div(withMathJax("$$
                                    M^*(u_i,u_j) = \\frac{u_j}{\\pi u_i}~\\int_{-1}^1 \\frac{exp[-u_i^2(1+r^2)]}{1+r^2}dv
                                    ~~~~~(4)
                                    $$
                                    $$
                                    r = (v+1)\\frac{u_j}{2u_i}~~~~~(5)
                                    $$")),
                tags$h5("and W(u) = well function."),
                tags$h5(HTML("For u &ge; 0, the error function is given by")),
                tags$div(withMathJax("$$
                                     erf(u) = 1-(e_1b+e_2b^2+e_3b^3+e_4b^4+e_5b^5)e^{-u^2}~~~~~(6)
                                     $$")),
                tags$h5("where"),
                tags$div(withMathJax("$$
                                    b = 1/(1+pu)~~~~~e_4 = -1.453152027\\\\
                                    e_1 = 0.254829592~~~~~e_5 = 1.06140543\\\\
                                    e_2 = -0.284496736~~~~~p = 0.3275911\\\\
                                    e_3 = 1.421413741
                                    $$")),
                tags$h5(HTML("and erf(-u) = -erf(u). The error in equation (6) is in the order of 10<sup>-7</sup>.")),
                tags$h5(HTML("For values of u &le; 1, the well function is given by")),
                tags$div(withMathJax("$$
                                     W(u) = a_0-\\ln(u)+a_1u+a_2u^2+a_3u^3+a_4u^4+a_5u^5~~~~~(7)
                                     $$")),
                tags$h5("where"),
                tags$div(withMathJax("$$
                                    a_0 = -0.57721566~~~~~a_3 = 0.05519968\\\\
                                    a_1 = 0.99999193~~~~~a_4 = -0.00976004\\\\
                                    a_2 = -0.24991055~~~~~a_5 = 0.00107857
                                    $$")),
                tags$h5(HTML("For values of 1 &le; u &le; &infin;, the well function is given by")),
                tags$div(withMathJax("$$
                                     W(u) = \\frac{1}{u e^u} \\frac{u^4+b_1u^3+b_2u^2+b_3u+b_4}{u_4+c_1u^3+c_2u^2+c_3u+c_4}~~~~~(8)
                                     $$")),
                tags$h5("where"),
                tags$div(withMathJax("$$
                                    b_1 = 8.57332874~~~~~c_1 = 9.57332235\\\\
                                    b_2 = 18.0590170~~~~~c_2 = 25.6329561\\\\
                                    b_3 = 8.63476089~~~~~c_3 = 21.0996531\\\\
                                    b_4 = 0.267773734~~~~~c_4 = 3.95849692
                                    $$")),
                tags$h5(HTML("The integral in M* is evaluated using six-point Gaussian Quadrature given by")),
                tags$div(withMathJax("$$
                                     M^*(u_i,u_j) = \\frac{u_j}{\\pi u_i} \\sum_{k=1}^{6} \\frac{exp[-u_i^2(1+r^2)]}{1+r^2} V_k~~~~~(9)
                                     $$")),
                tags$h5("where"),
                tags$div(withMathJax("$$
                                    r = (A_k+1)\\frac{u_j}{2u_i},~~~~~(10)\\\\
                                    A_k = \\text{abscissas of Gaussian Quadrature,}\\\\
                                    V_k = \\text{weights of Gaussian Quadrature.}
                                    $$")),
                tags$h5("The abscissas and weights are"),
                tags$div(withMathJax("$$
                                    A_1 = -A_6 = 0.238619186~~~~~V_1 = V_6 = 0.467913935\\\\
                                    A_2 = -A_5 = 0.661209386~~~~~V_2 = V_5 = 0.360761573\\\\
                                    A_3 = -A_4 = 0.932469514~~~~~V_3 = V_4 = 0.171324492
                                    $$")),
                tags$div("With a stream in vicinity, the principle of superposition in space is used to calculate the mound profile.
                         The drawdown from the image basin is superimposed onto the mound height contribution from the real basin to give
                         the actual mound height."),
                tags$div(withMathJax("$$
                                    H = H_r + H_{is}
                                    $$")),
                tags$h5("where"),
                tags$div(withMathJax("$$
                                    H_r = \\text{mound height contribution from the real basin,}\\\\
                                    H_{is} = \\text{drawdown contribution from the image basin superimposed in space.}
                                    $$"))
              )
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  # Recharge rate calc
  calculatedVal <- reactiveVal(0.0334)
  observeEvent(input$calculateRate, {
    req(input$dischargeRate, input$basinX, input$basinY)
    gal_to_cuft <- 231 / 1728
    cuft_d <- input$dischargeRate * gal_to_cuft
    sqft <- input$basinX * input$basinY
    ft_d <- cuft_d / sqft
    ft_d <- round(ft_d, 4)
    calculatedVal(ft_d)
  })
  output$rechargeSolution <- renderText({calculatedVal()})
  observe({
    updateNumericInput(
      session = session,
      inputId = "rechargeRate",
      value = calculatedVal()
    )
  })
  # Transmissivity calc
  calculatedVal2 <- reactiveVal(2500)
  observeEvent(input$calculateTrans, {
    req(input$conductivity, input$thickness)
    sqft_d <- input$conductivity * input$thickness
    sqft_d <- round(sqft_d, 2)
    calculatedVal2(sqft_d)
  })
  output$transSolution <- renderText({calculatedVal2()})
  observe({
    updateNumericInput(
      session = session,
      inputId = "transmissivity",
      value = calculatedVal2()
    )
  })
  # Recharge Period calc
  calculatedVal3 <- reactiveVal(7305)
  observeEvent(input$calculatePeriod, {
    req(input$rechargeYears)
    rDays <- input$rechargeYears * 365.25
    rDays <- round(rDays, 0)
    calculatedVal3(rDays)
  })
  output$transSolution <- renderText({calculatedVal3()})
  observe({
    updateNumericInput(
      session = session,
      inputId = "rechargePeriod",
      value = calculatedVal3()
    )
  })

  ### MOUND CALCULATED DATA ###

  # mound_data <- reactive({
  #   bgs <- input$waterDepth
  #   rechargeR <- input$rechargeRate
  #   storageC <- input$specificYield
  #   transM <- input$transmissivity
  #   bWidth <- input$basinX
  #   bLength <- input$basinY
  #   radius <- input$moundRadius
  #   increment <- input$radiusIncrement
  #   time <- input$rechargePeriod
  #   e <- exp(1)
  # 
  #   # Checkbox toggle from the UI
  #   use_stream <- input$useStream
  # 
  #   # Generate the spatial grid sequences
  #   x_seq <- seq(-1 * radius, radius, by = increment)
  #   y_seq <- seq(-1 * radius, radius, by = increment)
  #   grid <- expand.grid(xvar = x_seq, yvar = y_seq)
  # 
  #   solve_hantush <- function(x_coords, y_coords) {
  #     denom <- sqrt((4 * transM * time) / storageC)
  #     uvar1 <- (x_coords - (bWidth / 2)) / denom
  #     uvar2 <- (x_coords + (bWidth / 2)) / denom
  #     uvar3 <- (y_coords - (bLength / 2)) / denom
  #     uvar4 <- (y_coords + (bLength / 2)) / denom
  # 
  #     uvar <- cbind(uvar1, uvar2, uvar3, uvar4)
  #     uvar_pairs <- list(uvar[,c(2,4)], uvar[,c(2,3)], uvar[,c(1,4)], uvar[,c(1,3)])
  # 
  #     ints <- list()
  #     out_vector <- numeric(length(x_coords))
  # 
  #     # Loop Math Helpers
  #     nodes <- c(0.238619186, 0.661209386, 0.932469514, -0.932469514, -0.661209386, -0.238619186)
  #     weights <- c(0.467913935, 0.360761573, 0.171324492, 0.171324492, 0.360761573, 0.467913935)
  # 
  #     Mstar = function(ui, uj){
  #       if (is.na(ui) || is.nan(ui) || ui == 0) return(0)
  #       Mstar <- ui / pi
  #       resultM <- 0
  #       for (k in 1:length(nodes)) {
  #         r_k <- (nodes[k] + 1) * (uj / (2 * ui))
  #         resultM <- resultM + (exp((-uj^2) * (1 + r_k^2)) / (1 + r_k^2)) * weights[k]
  #       }
  #       return(Mstar * resultM)
  #     }
  # 
  #     wellf <- function(u){
  #       if (u <= 1) {
  #         if (u <= 0) return(0)
  #         return(-0.57721566 - log(u) + (0.99999193 * u) + (-0.24991055 * u^2) + (0.05519968 * u^3) + (-0.00976004 * u^4) + (0.00107857 * u^5))
  #       } else {
  #         return((1/(u * exp(u))) * (((u^4) + (8.57332874 * u^3) + (18.0590170 * u^2) + (8.63476089 * u) + 0.267773734) / ((u^4) + (9.57332235 * u^3) + (25.6329561 * u^2) + (21.0996531 * u) + 3.95849692)))
  #       }
  #     }
  # 
  #     erf <- function(uraw){
  #       sign <- ifelse(uraw < 0, -1, 1)
  #       u <- abs(uraw)
  #       bvar <- 1/(1 + (0.3275911 * u))
  #       return(sign * (1 - ((0.254829592 * bvar) + (-0.284496736 * bvar^2) + (1.421413741 * bvar^3) + (-1.453152027 * bvar^4) + (1.06140543 * bvar^5)) * e^(-u^2)))
  #     }
  # 
  #     for (z in 1:length(x_coords)) {
  #       for (q in 1:length(uvar_pairs)) {
  #         uvari <- uvar_pairs[[q]][z, 1]
  #         uvarj <- uvar_pairs[[q]][z, 2]
  # 
  #         mstar_term1 <- if (uvari == 0) 0 else (uvari^2) * Mstar(uvarj / uvari, uvari^2)
  #         mstar_term2 <- if (uvarj == 0) 0 else (uvarj^2) * Mstar(uvari / uvarj, uvarj^2)
  # 
  #         ints[[q]] <- erf(uvari) * erf(uvarj) + (4 / pi) * uvari * uvarj * wellf((uvari^2) + (uvarj^2)) +
  #           (2 / sqrt(pi)) * (uvari * e^(-uvari^2) * erf(uvarj) + uvarj * e^(-uvarj^2) * erf(uvari)) -
  #           2 * (mstar_term1 + mstar_term2)
  #       }
  #       out_vector[z] <- ((rechargeR * time) / (4 * storageC)) * (ints[[1]] - ints[[2]] - ints[[3]] + ints[[4]])
  #     }
  #     return(out_vector)
  #   }
  #   # -----------------------------------------------------------------
  # 
  #   # ALWAYS calculate the mound from the REAL basin
  #   z_real <- solve_hantush(grid$xvar, grid$yvar)
  # 
  #   # Conditional Spatial Superposition
  #   if (isTRUE(use_stream)) {
  #     # Only read the distance input and calculate image well if checked
  #     d_stream <- input$streamDistance
  #     x_image_coords <- grid$xvar - (2 * d_stream)
  #     z_image <- solve_hantush(x_image_coords, grid$yvar)
  # 
  #     # Superposition: Real minus Image
  #     resultH <- z_real - z_image
  #   } else {
  #     # Default behavior: No stream, no image basin math
  #     resultH <- z_real
  #     d_stream <- NA
  #   }
  # 
  #   # Force any negative values due to boundary truncation back to 0
  #   resultH[resultH < 0] <- 0
  # 
  #   z_matrix <- matrix(resultH, nrow = length(x_seq), ncol = length(y_seq))
  # 
  #   list(
  #     x_seq = x_seq, y_seq = y_seq, z_matrix = z_matrix, resultH = resultH,
  #     bgs = bgs, radius = radius, bWidth = bWidth, bLength = bLength,
  #     use_stream = use_stream, d_stream = d_stream
  #   )
  # })
  
  ### MOUND DATA CHUNK ACCOUNTING FOR SEPARATE BASIN SUBUNITS AND PORTION OF BASIN THAT ARE TRENCHES ###
  
  mound_data <- reactive({
    bgs <- input$waterDepth
    storageC <- input$specificYield
    transM <- input$transmissivity
    radius <- input$moundRadius
    increment <- input$radiusIncrement
    time <- input$rechargePeriod
    use_stream <- input$useStream 
    
    # --- NEW SUBUNIT VARIABLES (Hardcoded from your prompt or linked to UI inputs) ---
    bLength  <- input$basinY   # Subunit length matches total length
    bWidth <- input$basinX # Total basin width including all subunits and spacing
    subUnits <- input$subUnits # Number of subunits in basin
    subSpace <- input$subSpace # Space between subunits
    subWidth <- (bWidth-(subSpace*(subUnits-1)))/subUnits   # Calculated subunit widths
    trench_f <- input$fTrench   # Disposal trench area fraction
    
    # Scale the recharge rate by the trench fraction
    rechargeR <- input$rechargeRate * trench_f 
    
    # Define the center points (X offsets) for n subunits
    first_center <- (-1*bWidth / 2) + (subWidth / 2)
    center_increment <- subWidth + subSpace
    subunit_centers <- seq(from = first_center, by = center_increment, length.out = subUnits)
    # --------------------------------------------------------------------------------
    
    x_seq <- seq(-1 * radius, radius, by = increment)
    y_seq <- seq(-1 * radius, radius, by = increment)
    grid <- expand.grid(xvar = x_seq, yvar = y_seq)
    
    # Local Hantush Solver (Modified to use the localized subWidth and bLength variables)
    solve_hantush <- function(x_coords, y_coords) {
      denom <- sqrt((4 * transM * time) / storageC)
      uvar1 <- (x_coords - (subWidth / 2)) / denom  # Updated to subWidth
      uvar2 <- (x_coords + (subWidth / 2)) / denom  # Updated to subWidth
      uvar3 <- (y_coords - (bLength / 2)) / denom   # Updated to bLength
      uvar4 <- (y_coords + (bLength / 2)) / denom   # Updated to bLength
      
      uvar <- cbind(uvar1, uvar2, uvar3, uvar4)
      uvar_pairs <- list(
        uvar[, c(2,4), drop = FALSE], 
        uvar[, c(2,3), drop = FALSE], 
        uvar[, c(1,4), drop = FALSE], 
        uvar[, c(1,3), drop = FALSE]
      )
      
      ints <- list()
      out_vector <- numeric(length(x_coords))
      e <- exp(1)
      nodes <- c(0.238619186, 0.661209386, 0.932469514, -0.932469514, -0.661209386, -0.238619186)
      weights <- c(0.467913935, 0.360761573, 0.171324492, 0.171324492, 0.360761573, 0.467913935)
      
      Mstar = function(ui, uj){
        if (is.na(ui) || is.nan(ui) || ui == 0) return(0) 
        Mstar_prefactor <- ui / pi 
        resultM <- 0
        for (k in 1:length(nodes)) {
          r_k <- (nodes[k] + 1) * (ui / 2)
          resultM <- resultM + (exp((-uj) * (1 + r_k^2)) / (1 + r_k^2)) * weights[k]
        }
        return(Mstar_prefactor * resultM)
      }
      wellf <- function(u){
        if (u <= 1) { if (u <= 0) return(0) 
          return(-0.57721566 - log(u) + (0.99999193 * u) + (-0.24991055 * u^2) + (0.05519968 * u^3) + (-0.00976004 * u^4) + (0.00107857 * u^5))
        } else {
          return((1/(u * exp(u))) * (((u^4) + (8.57332874 * u^3) + (18.0590170 * u^2) + (8.63476089 * u) + 0.267773734) / ((u^4) + (9.57332235 * u^3) + (25.6329561 * u^2) + (21.0996531 * u) + 3.95849692)))
        }
      }
      erf <- function(uraw){
        sign <- ifelse(uraw < 0, -1, 1); u <- abs(uraw); bvar <- 1/(1 + (0.3275911 * u))
        return(sign * (1 - ((0.254829592 * bvar) + (-0.284496736 * bvar^2) + (1.421413741 * bvar^3) + (-1.453152027 * bvar^4) + (1.06140543 * bvar^5)) * e^(-u^2))) 
      }
      
      for (z in 1:length(x_coords)) { 
        for (q in 1:length(uvar_pairs)) {  
          uvari <- uvar_pairs[[q]][z, 1]; uvarj <- uvar_pairs[[q]][z, 2] 
          mstar_term1 <- if (uvari == 0) 0 else (uvari^2) * Mstar(uvarj / uvari, uvari^2)
          mstar_term2 <- if (uvarj == 0) 0 else (uvarj^2) * Mstar(uvari / uvarj, uvarj^2)
          ints[[q]] <- erf(uvari) * erf(uvarj) + 
            (4 / pi) * uvari * uvarj * wellf((uvari^2) + (uvarj^2)) + 
            (2 / sqrt(pi)) * (uvari * exp(-uvari^2) * erf(uvarj) + uvarj * exp(-uvarj^2) * erf(uvari)) - 
            2 * (mstar_term1 + mstar_term2)
        }
        out_vector[z] <- ((rechargeR * time) / (4 * storageC)) * (ints[[1]] - ints[[2]] - ints[[3]] + ints[[4]])
      }
      return(out_vector)
    }
    
    # -----------------------------------------------------------------
    # EXECUTE SPATIAL SUPERPOSITION FOR SUBUNITS
    # -----------------------------------------------------------------
    # Initialize blank vectors for tracking cumulative totals
    z_real  <- numeric(nrow(grid))
    z_image <- numeric(nrow(grid))
    
    # Loop through each of the 3 subunits
    for (center in subunit_centers) {
      # Shift coordinates by the subunit's center and add to total real mound
      z_real <- z_real + solve_hantush(grid$xvar - center, grid$yvar)
      
      # If stream is active, shift image coordinates based on each subunit center
      if (isTRUE(use_stream)) {
        d_stream <- input$streamDistance
        x_image_coords <- grid$xvar - (2 * d_stream - center)
        z_image <- z_image + solve_hantush(x_image_coords, grid$yvar)
      }
    }
    
    # Final Superposition Combining Subunits and Streams
    if (isTRUE(use_stream)) {
      resultH <- z_real - z_image
    } else {
      resultH <- z_real
      d_stream <- NA
    }
    
    resultH[resultH < 0] <- 0
    z_matrix <- matrix(resultH, nrow = length(x_seq), ncol = length(y_seq))
    
    # Return definitions (Using total combined dimensions for bounding box layouts)
    list(
      x_seq = x_seq, y_seq = y_seq, z_matrix = z_matrix, resultH = resultH,
      bgs = bgs, radius = radius, bWidth = bWidth, bLength = bLength, 
      use_stream = use_stream, d_stream = d_stream
    )
  })

  ### 2D Plot ###

  output$moundPlot2D <- renderPlotly({
    # Pull data from the reactive calculation
    data <- mound_data()

    bgs <- input$waterDepth
    rechargeR <- input$rechargeRate
    storageC <- input$specificYield
    transM <- input$transmissivity
    bWidth <- input$basinX
    bLength <- input$basinY
    radius <- input$moundRadius
    increment <- input$radiusIncrement
    time <- input$rechargePeriod

    # Extract the 0-degree cross section (along the X-axis through the center where Y = 0)
    center_y_idx <- which(data$y_seq == 0)
    # If increment prevents hitting absolute 0, default to exact middle column
    if (length(center_y_idx) == 0) center_y_idx <- round(length(data$y_seq) / 2)

    profile_z <- data$z_matrix[, center_y_idx]

    final2D <- data.frame(distance = data$x_seq, resultH = profile_z)

    # Render 2D Plot
    fig <- final2D %>%
      plot_ly(x = ~distance, y = ~resultH, type = "scatter", mode = "lines",
              line = list(width = 2, color = "black"), name = "Mounding",
              hovertemplate = "Distance: %{x} ft<br>Mounding: %{y:.2f} ft<extra></extra>")
    stream_annotation <- list()
    if (isTRUE(data$use_stream)) {
      stream_annotation <- list(
        list(
          x = data$d_stream,
          y = 0,
          text = "&#9660;",             # Solid downward triangle
          font = list(color = "#0f75bc", size = 22),
          showarrow = FALSE,
          yanchor = "bottom"
        ),
        list(x = 0, y = max(data$resultH), xref = "x", yref = "y", text = paste0(round(max(data$resultH), 2), " ft"),
             showarrow = FALSE, xanchor = "center", yanchor = "bottom", font = list(color = "#000000")),
        list(x = 0, y = bgs, xref = "paper", yref = "y", text = "Ground Surface",
             showarrow = FALSE, xanchor = "left", yanchor = "bottom", font = list(color = "#0e7446")),
        list(x = 0, y = bgs - 4, xref = "x", yref = "y", text = "Basin",
             showarrow = FALSE, xanchor = "center", yanchor = "bottom", font = list(color = "#000000")),
        list(x = 0, y = 0, xref = "paper", yref = "y", text = "Water Table",
             showarrow = FALSE, xanchor = "left", yanchor = "top", font = list(color = "#0f75bc"))
      )
    }
      fig %>% layout(
        title = "Groundwater Mounding Profile",
        xaxis = list(title = "Distance from Recharge (ft)", range = c(-data$radius, data$radius), zeroline = FALSE),
        yaxis = list(title = "Mounding (ft)", range = c(-1, data$bgs + 1)),
        shapes = list(
          list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = data$bgs, y1 = data$bgs, line = list(color = "#0e7446", width = 2)),
          list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 0, y1 = 0, line = list(color = "#0f75bc", width = 2)),
          list(type = "line", x0 = -data$bWidth/2, x1 = -data$bWidth/2, y0 = data$bgs, y1 = data$bgs - 4, line = list(color = "gray", width = 2)),
          list(type = "line", x0 = data$bWidth/2, x1 = data$bWidth/2, y0 = data$bgs, y1 = data$bgs - 4, line = list(color = "gray", width = 2)),
          list(type = "line", x0 = -data$bWidth/2, x1 = data$bWidth/2, y0 = data$bgs - 4, y1 = data$bgs - 4, line = list(color = "gray", width = 2))
        ),
        annotations = stream_annotation,
        annotations = list(
          list(x = 0, y = max(data$resultH), xref = "x", yref = "y", text = paste0(round(max(data$resultH), 2), " ft"),
               showarrow = FALSE, xanchor = "center", yanchor = "bottom", font = list(color = "#000000")),
          list(x = 0, y = bgs, xref = "paper", yref = "y", text = "Ground Surface",
               showarrow = FALSE, xanchor = "left", yanchor = "bottom", font = list(color = "#0e7446")),
          list(x = 0, y = bgs - 4, xref = "x", yref = "y", text = "Basin",
               showarrow = FALSE, xanchor = "center", yanchor = "bottom", font = list(color = "#000000")),
          list(x = 0, y = 0, xref = "paper", yref = "y", text = "Water Table",
               showarrow = FALSE, xanchor = "left", yanchor = "top", font = list(color = "#0f75bc"))
        )
      )
  })

  ### 3D Plot ###
  output$moundPlot3D <- renderPlotly({
    data <- mound_data()

    bgs <- input$waterDepth
    rechargeR <- input$rechargeRate
    storageC <- input$specificYield
    transM <- input$transmissivity
    bWidth <- input$basinX
    bLength <- input$basinY
    radius <- input$moundRadius
    increment <- input$radiusIncrement
    time <- input$rechargePeriod

    x_padded <- c(data$x_seq[1], data$x_seq, data$x_seq[length(data$x_seq)])
    y_padded <- c(data$y_seq[1], data$y_seq, data$y_seq[length(data$y_seq)])
    z_padded <- matrix(0, nrow = length(x_padded), ncol = length(y_padded))
    z_padded[2:(length(data$x_seq) + 1), 2:(length(data$y_seq) + 1)] <- data$z_matrix

    wt_matrix <- matrix(0, nrow = length(x_padded), ncol = length(y_padded))
    gs_matrix <- matrix(data$bgs, nrow = length(x_padded), ncol = length(y_padded))

    basin_x <- c(-data$bWidth / 2, data$bWidth / 2, data$bWidth / 2, -data$bWidth / 2, -data$bWidth / 2)
    basin_y <- c(-data$bLength / 2, -data$bLength / 2, data$bLength / 2, data$bLength / 2, -data$bLength / 2)
    basin_z <- rep(data$bgs - 4, 5)

    fig <- plot_ly() %>%

      # Primary Opaque Mound Surface
      add_trace(
        x = data$x_seq, y = data$y_seq, z = t(data$z_matrix),
        type = "surface",
        colorscale = "Blues",
        reversescale = TRUE,
        opacity = 1.0,
        name = "Groundwater Mound",
        showlegend = TRUE,
        colorbar = list(
          title = "Mound Height (ft)",
          len = 0.4,          # Shrinks color bar height
          yanchor = "top",
          x = 1.02,
          y = 0.75
        ),
        hovertemplate = "X: %{x} ft<br>Y: %{y} ft<br>Mounding: %{z:.2f} ft<extra></extra>",
        contours = list(
          x = list(show = TRUE, usecolormap = FALSE, color = "#444444", width = 1.5, start = -data$radius, end = data$radius, size = increment*2),
          y = list(show = TRUE, usecolormap = FALSE, color = "#444444", width = 1.5, start = -data$radius, end = data$radius, size = increment*2),
          z = list(show = TRUE, usecolormap = FALSE, color = "black", width = 2, highlightcolor = "#ee3b33", highlightwidth = 4,
                   start = min(data$resultH), end = max(data$resultH), size = (max(data$resultH) - min(data$resultH)) / 5)
        )
      ) %>%

      # Transparent Side Walls
      add_trace(x = x_padded, y = y_padded, z = t(z_padded), type = "surface", opacity = 0.4,
                colorscale = "Blues", reversescale = TRUE, showscale = FALSE,
                name = "Mound Side Walls", showlegend = FALSE, hoverinfo = "none",
                contours = list(x = list(highlight = FALSE), y = list(highlight = FALSE), z = list(highlight = FALSE))) %>%

      # Water Table
      add_trace(x = x_padded, y = y_padded, z = t(wt_matrix), type = "surface", opacity = 0.3,
                colorscale = list(c(0, 1), c("#0f75bc", "#0f75bc")), showscale = FALSE,
                name = "Original Water Table", showlegend = FALSE, hoverinfo = "none",
                contours = list(x = list(highlight = FALSE), y = list(highlight = FALSE), z = list(highlight = FALSE))) %>%

      # Basin Footprint
      add_trace(x = basin_x, y = basin_y, z = t(basin_z), type = "scatter3d", mode = "lines",
                line = list(color = "#ee3b33", width = 3, dash = "solid"),
                name = "Basin Footprint", showlegend = TRUE,
                hovertemplate = "Basin Boundary<extra></extra>")

        if (isTRUE(data$use_stream)) {
          stream_y <- c(-data$radius, data$radius)
          stream_x <- c(data$d_stream, data$d_stream)
          stream_z <- c(0, 0) # Sits right at the water table elevation

          fig <- fig %>% add_trace(
            x = stream_x, y = stream_y, z = stream_z,
            type = "scatter3d", mode = "lines",
            line = list(color = "#0f75bc", width = 8), # Thick blue line representing the river
            name = "Constant-Head Stream",
            showLegend = TRUE,
            hovertemplate = "Stream Boundary<extra></extra>"
          )
        }
      fig %>%
      layout(
        title = list(
          text = "3D Groundwater Mounding Surface",
          font = list(size = 18, face = "bold"),
          y = 0.95,
          x = 0.5,
          xanchor = "center"
        ),
        scene = list(
          xaxis = list(title = "Width (ft)", range = c(-data$radius, data$radius)),
          yaxis = list(title = "Length (ft)", range = c(-data$radius, data$radius)),
          zaxis = list(title = "Elevation (ft)", range = c(-1, data$bgs + 1)),
          aspectratio = list(x = 1, y = 1, z = 0.5)
        ),
        showlegend = TRUE,
        legend = list(
          x = 1.02,
          y = 0.9,
          xanchor = "left",
          yanchor = "top"
        )
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
