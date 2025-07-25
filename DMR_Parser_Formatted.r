##### Parsing Discharge Monitoring Report Raw Data #####

# This script takes Discharge Monitoring Report data from EPA's ECHO database
# and organizes it into effluent and groundwater data tables for Groundwater
# Discharge Permit fact sheets.

# The user only needs to provide the NPDES permit number and date range for the
# data they would like rendered into tables.

# v2:   (1) Removed year text from effluent table row names
#       (2) Added support for multiple outfall (EXO) tables
#       (3) Added additional Flow parameter_code to parameters.xlsx
# v2.1: (1) Fixed issue with outfalls not all being "EXO" type
#           (i.e. "LAS" for land application site)
# v2.2: (1) Fixed issue with wells labelled "EXO" being grouped with outfalls
#       (2) Fixed issue arising when there are multiple limits of the same
#           parameter & type (e.g. Multiple max TN load limits for grown crops)
# v2.3: (1) Fixed issue with "WEL" outfalls being grouped with MWs
# v2.4: (1) Fixed issue taking values from final row of effluent_limit_basic
#       (2) Added additional Chloride parameter_code to parameters.xlsx
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
# vF:   (1) Added support for indicating NODI codes and violations
#       (2) Added color formatting to match fact sheet style
#       (3) Added option to download parameters.xlsx from Github
#       (4) Added default working directory to querySetup function

################################################################################ Function to assign query values #####
querySetup = function() {
  message("Please provide the following information:")
  direct = as.character(readline(prompt = paste0("Local working directory (defaults to C://Users/",
                                                 Sys.getenv("USERNAME"),
                                                 "/Documents): ")))
  NPDESid = as.character(readline(prompt = "NPDES ID: "))
  sdate = as.character(readline(prompt = "Beginning of period (YYYY-MM-DD): "))
  edate = as.character(readline(prompt = "End of period (YYYY-MM-DD): "))
  list = list(direct, NPDESid, sdate, edate)
  if(direct==""){
    npdesID = NPDESid
    minDate = as.Date(sdate)
    maxDate = as.Date(edate)
    message("Facility parameters set.")
  } else {
    if(file.exists(direct)){
      setwd(direct)
      npdesID = NPDESid
      minDate = as.Date(sdate)
      maxDate = as.Date(edate)
      message("Working directory and facility parameters set.")
    } else {
      message("The provided directory does not exist. Please try again.")
      querySetup()
    }
  }
}
##### SUPPLY QUERY INFORMATION #####
param = querySetup()
##### WHERE FILES SHOULD BE SAVED #####
setwd(param[[1]])
################################################################################ Script setup, data gathering, and cleaning #####
library(conflicted) # Shows error if functions from multiple packages conflict
library(devtools) # Allows installation of packages from Github
library(tidyverse) # Main collection of packages for QoL
  conflicts_prefer(dplyr::filter, dplyr::mutate, dplyr::arrange)
library(readxl)
library(reshape2)
library(pivottabler); library(basictabler) # For making pivot tables
library(glue) # For building strings from variables
library(openxlsx) # Used to export tables to Excel
  
options(scipen=999)

# Function to take data from ECHO or a local csv downloaded from ICIS
ECHO_data = function(NPDES_ID, start_date, end_date) {
  tryCatch(
    # Function first tries to get data from ECHO
    expr = {
      start_date = format(as.Date(start_date), "%m/%d/%Y")
      end_date = format(as.Date(end_date), "%m/%d/%Y")
      url = paste0("https://echodata.epa.gov/echo/eff_rest_services.download_effluent_chart?",
                   "p_id=", NPDES_ID,
                   "&start_date=", start_date,
                   "&end_date=", end_date,
                   "&p_echo=Y")
      data = as.data.frame(read_csv(url))
      return(data)
    },
    # If ECHO does not work, function prompts user for a local file instead
    error = function(e) {
      cat("An error occurred when querying ECHO:", conditionMessage(e), "\n")
      message("EPA has temporarily blocked automated queries of ECHO from this computer.\n
              Please try again later.")
      # path = as.character(readline(prompt = "Please enter a local file path to the data in csv format: "))
      # data = as.data.frame(read_csv(path))
      # return(data)
    }
  )
}
##### NPDES ID, START DATE, AND END DATE #####
DMR_data = ECHO_data(param[[2]], param[[3]], param[[4]])
################################################################################
# Parameter key from github or file
# parametersQuery = function() {
#   response = as.character(readline(prompt = "Indicate whether you would like to download the parameter name key (yes/skip): "))
#   response = ifelse(response %in% c("Yes", "yes", "YES", "Y", "y"), TRUE, FALSE)
#   if(response == TRUE){
    url = "https://github.com/PetrAqua/aquaData/raw/refs/heads/main/parameters.xlsx"
    temp_file = tempfile(fileext = ".xlsx")
    download.file(url, destfile = temp_file, mode = "wb")
    parameters = as.data.frame(read_xlsx(temp_file))
#     message("parameters.xlsx imported to R via Github.")
#   } else {
#     parameters = as.data.frame(read_xlsx("parameters.xlsx"))
#     message("parameters.xlsx imported from local .csv file.")
#   }
# }

data = DMR_data %>%
  select(perm_feature_id, perm_feature_nmbr, perm_feature_type_code, monitoring_period_end_date,
         parameter_code, parameter_desc, limit_value_id, limit_value_type_desc, limit_value_nmbr,
         limit_unit_desc, statistical_base_short_desc, dmr_value_nmbr, nodi_code, nodi_desc, violation_code, violation_desc) %>%
  mutate(monitoring_period_end_date = as_date(monitoring_period_end_date, format = "%m/%d/%Y")) %>%
  inner_join(parameters[,c(1,3)], join_by(parameter_code), relationship = "many-to-many") %>%
  mutate(parameter_name = glue("{parameter_alias} ({limit_unit_desc})"),
         limit_table_name = "Limit")

# Add code that automatically exports the ECHO data for every query to a folder
# and edit ECHO_data function to ask if the user would like it to search the
# folder for matching data if ECHO blocks queries or an error occurs?

################################################################################ Data Subsets for QA/QC Checks #####

effluent = data %>%
  filter(!grepl("MW", perm_feature_nmbr))
# Split effluent data.frame into list of grouped_dfs
outfalls = split(effluent, effluent$perm_feature_nmbr)
# Create empty list to use later for recalling outfall tables
outfalls_list = list()
# Creates separate grouped_dfs named by outfall
for (i in 1:length(outfalls)) {
  assign(paste0(
    unique(outfalls[[i]]$perm_feature_type_code), "_",
    unique(outfalls[[i]]$perm_feature_nmbr)),
    outfalls[[i]])
}

wells = data %>%
  filter(grepl("MW", perm_feature_nmbr)) %>%
  # Extracts numeric well numbers from alphanumeric permit_feature_nmbr column
  mutate(strcapture("(.*?)([[:digit:]]+)", perm_feature_nmbr,
                    proto = list(MW_prefix = "", wellNo = as.numeric()))) %>%
  arrange(wellNo) %>%
  # Orders column by numeric instead of alphabetical order
  mutate(perm_feature_nmbr = factor(perm_feature_nmbr,
                                    unique(perm_feature_nmbr)))

effluent_limits = effluent %>%
  select(perm_feature_nmbr, parameter_code, parameter_desc, statistical_base_short_desc,
         limit_value_id, limit_value_nmbr, limit_unit_desc) %>%
  distinct()
# Test if there are multiple limits for the same statistic type and parameter (e.g. DP-2801)
multiTest = nrow(distinct(effluent_limits[,c("parameter_desc","statistical_base_short_desc")])) == nrow(distinct(effluent_limits[,c("parameter_desc","limit_value_id")]))
# Split effluent_limits data.frame into list of grouped_dfs
outfall_limits = split(effluent_limits, effluent_limits$perm_feature_nmbr)
# Creates separate grouped_dfs named by outfall
for (i in 1:length(outfall_limits)) {
  assign(paste0(
    unique(outfalls[[i]]$perm_feature_type_code), "_",
    unique(outfall_limits[[i]]$perm_feature_nmbr), "_limits"),
    outfall_limits[[i]])
}

gw_limits = wells %>%
  select(parameter_code, parameter_desc, statistical_base_short_desc,
         limit_value_nmbr, limit_unit_desc) %>%
  distinct()
  
################################################################################ Table for Effluent Values and Limits #####

nodiLogic <- function(nodi_code, dmr_value_nmbr, violation_code) { # Allows pivot table to display NODI code when available instead of NaN
  if (all(is.na(nodi_code))) {
    return(paste0(mean(dmr_value_nmbr, na.rm = TRUE),ifelse(is.na(violation_code)==TRUE,"","*")))  # Adds asterisk to violation values
  } else {
    return(nodi_code[!is.na(nodi_code)][1])  # Return first non-NA value of nodi_code
  }
}
tableTheme <- list(
  fontName="Times New Roman",
  fontSize="0.75em",
  headerBackgroundColor = "#92d050",
  headerColor = "#000000",
  cellBackgroundColor = "#FFFFFF",
  cellColor = "#000000",
  outlineCellBackgroundColor = "#000000",
  outlineCellColor = "#000000",
  totalBackgroundColor = "#FFFFFF",
  totalColor = "#000000",
  borderColor = "#000000"
)

fx <- function(v, cell) { # Highlights violations detected in NetDMR
  if(isTRUE(str_detect(v,"[*]"))) return("#f1a983")
  if(isTRUE(str_detect(v,"E"))) return("#f1a984")
  return("#a6c9ec")
}

# Pivot table(s) of effluent values
for (i in seq_along(outfalls)){
  effluent_pivot = PivotTable$new()
  effluent_pivot$addData(outfalls[[i]])
  effluent_pivot$addRowDataGroups("monitoring_period_end_date",
                                  dataFormat = list(format = "%B"),
                                  addTotal = FALSE)
  effluent_pivot$addColumnDataGroups("parameter_name",
                                     addTotal = FALSE)
  effluent_pivot$addColumnDataGroups("statistical_base_short_desc",
                                     addTotal = FALSE)
  if(multiTest == FALSE) {effluent_pivot$addColumnDataGroups("limit_value_id",
                                     addTotal = FALSE)}
  effluent_pivot$defineCalculation(calculationName = paste0("meanValue_", i),
                                   summariseExpression = "as.character(nodiLogic(nodi_code, dmr_value_nmbr, violation_code))")
  effluent_pivot$evaluatePivot()
  effluent_pivot$theme <- tableTheme
  
  effluent_pivot$mapStyling(cells=effluent_pivot$allCells, styleProperty="background-color", mappings=fx)
  effluent_basic = effluent_pivot$asBasicTable()
  # Pivot table of effluent limits
  effluent_limit_pivot = PivotTable$new()
  effluent_limit_pivot$addData(outfalls[[i]])
  effluent_limit_pivot$addRowDataGroups("limit_table_name",
                                        addTotal = FALSE)
  effluent_limit_pivot$addColumnDataGroups("parameter_name",
                                           addTotal = FALSE)
  effluent_limit_pivot$addColumnDataGroups("statistical_base_short_desc",
                                           addTotal = FALSE)
  if(multiTest == FALSE) {effluent_limit_pivot$addColumnDataGroups("limit_value_id",
                                                             addTotal = FALSE)}
  effluent_limit_pivot$defineCalculation(calculationName = paste0("meanLimit_", i),
                                         summariseExpression = "as.character(unique(na.omit(limit_value_nmbr)))")
  effluent_limit_pivot$evaluatePivot()
  effluent_limit_pivot$theme <- tableTheme
  effluent_limit_basic = effluent_limit_pivot$asBasicTable()
  # Combining effluent tables
  effluent_basic$cells$insertRow(effluent_basic$rowCount + 1)
  effluent_basic$cells$setRow(effluent_basic$rowCount,
                              startAtColumnNumber = 1,
                              cellTypes = c("rowHeader"),
                              effluent_limit_basic$cells$getRowValues(rowNumber = effluent_limit_basic$rowCount,
                                                                      columnNumbers = 1:effluent_limit_basic$columnCount,
                                                                      formattedValue = FALSE,
                                                                      asList = TRUE,
                                                                      rebase = TRUE))
  effluent_basic$renderTable()
  message("Effluent Table for Outfall ", i, ":\n")
  print(effluent_basic)
  outfalls_list[[paste0("Outfall_", i)]] <- effluent_basic
}
# Create separate basicTable objects for each outfall
for (i in seq_along(outfalls_list)) {
  assign(paste0(names(outfalls_list)[i], "_table"), outfalls_list[[i]])
}
################################################################################ Table for Groundwater Values and Limits #####

# Pivot table of GW values
gw_pivot = PivotTable$new()
gw_pivot$addData(wells)
gw_pivot$addRowDataGroups("perm_feature_nmbr",
                          addTotal = FALSE)
gw_pivot$addColumnDataGroups("parameter_name",
                             addTotal = FALSE)
gw_pivot$defineCalculation(calculationName = "meanValue",
                           summariseExpression = "nodiLogic(nodi_code, dmr_value_nmbr, violation_code)")
gw_pivot$evaluatePivot()
gw_pivot$theme <- tableTheme

gw_pivot$mapStyling(cells=gw_pivot$allCells, styleProperty="background-color", mappings=fx)
gw_pivot$renderPivot()

gw_basic = gw_pivot$asBasicTable()
# Pivot table of GW limits
gw_limit_pivot = PivotTable$new()
gw_limit_pivot$addData(wells)
gw_limit_pivot$addRowDataGroups("limit_table_name",
                                addTotal = FALSE)
gw_limit_pivot$addColumnDataGroups("parameter_name",
                                   addTotal = FALSE)
gw_limit_pivot$defineCalculation(calculationName = "meanLimit",
                                 summariseExpression = "as.character(mean(limit_value_nmbr, na.rm = T))")
gw_limit_pivot$evaluatePivot()
gw_limit_pivot$theme <- tableTheme

gw_limit_basic = gw_limit_pivot$asBasicTable()

gw_basic$cells$insertRow(gw_basic$rowCount + 1) # Combining GW tables
gw_basic$cells$setRow(gw_basic$rowCount,
                            startAtColumnNumber = 1,
                            cellTypes = c("rowHeader"),
                            gw_limit_basic$cells$getRowValues(rowNumber = 2,
                                                              columnNumbers = 1:gw_limit_basic$columnCount,
                                                              formattedValue = FALSE,
                                                              asList = TRUE,
                                                              rebase = TRUE))
gw_basic$renderTable()

################################################################################ Exporting Tables to Excel #####

# Function to export tables
DMR_export = function() {
  input = as.character(readline(prompt = "Would you like to export the tables to Excel? Yes/No: "))
  if (input == "Yes") {
    wb = createWorkbook()
    for (i in seq_along(outfalls_list)) {addWorksheet(wb, paste0(names(outfalls_list)[i], " Table"))}
    addWorksheet(wb, "Groundwater Table")
    gw_basic$writeToExcelWorksheet(wb = wb, wsName = "Groundwater Table",
                                   topRowNumber = 1, leftMostColumnNumber = 1,
                                   applyStyles = TRUE, outputValuesAs="formattedValueAsText")
    addWorksheet(wb, "Raw Data")
    writeDataTable(wb, "Raw Data", DMR_data)
    for (i in seq_along(outfalls_list)) {
      outfalls_list[[i]]$writeToExcelWorksheet(wb = wb, wsName = paste0(names(outfalls_list)[i], " Table"),
                                               topRowNumber = 1, leftMostColumnNumber = 1,
                                               applyStyles = TRUE, outputValuesAs="formattedValueAsText")
    }
    saveWorkbook(wb, file = paste0(glue("{getwd()}/{DMR_data$npdes_id[[1]]}_Data.xlsx")), overwrite = TRUE)
    message(paste0(glue("Tables exported to {getwd()}/{DMR_data$npdes_id[[1]]}_Data.xlsx")))
    shell.exec(paste0("file:", getwd()))
  } else {
    message("Tables not exported.")
  }
}
##### EXPORT TABLES TO EXCEL #####
DMR_export()
