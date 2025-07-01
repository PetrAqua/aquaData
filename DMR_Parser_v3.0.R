# DMR Parser v3.0 uses raw data from ECHO compiled by fiscal year (2023 & 2024)
# in csv files instead of querying ECHO for each facility. This version is able
# to work without an internet connection if the DMR data has already been
# downloaded, and is able to summarize many facilities in a row without the risk
# of the user being locked out of ECHO for making too many queries.

################################################################################
### LIBRARIES AND OPTIONS ###

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

setwd(paste0("C://Users/",Sys.getenv("USERNAME"),"/Documents"))

################################################################################
### QUERIES ###

# SPECIFIC FACILITY QUERY
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
# ECHO DATA DOWNLOAD QUERY
downloadQuery = function() {
  response = as.character(readline(prompt = "Indicate whether you would like to download the ECHO data for FYs 2023-2024 (yes/skip): "))
  response = ifelse(response %in% c("Yes", "yes", "YES", "Y", "y"), TRUE, FALSE)
  if (response == TRUE){
    temp = tempfile()
    download.file("https://echo.epa.gov/files/echodownloads/NPDES_by_state_year/MD_FY2023_NPDES_DMRS_LIMITS.zip",temp)
    dmrs23 = read.csv(unz(temp, "MD_FY2023_NPDES_DMRS.csv"))
    unlink(temp)
    download.file("https://echo.epa.gov/files/echodownloads/NPDES_by_state_year/MD_FY2024_NPDES_DMRS_LIMITS.zip",temp)
    dmrs24 = read.csv(unz(temp, "MD_FY2024_NPDES_DMRS.csv"))
    unlink(temp)
    message("ECHO data imported to R.")
  } else {
    dmrs23 = read.csv("MD_FY2023_NPDES_DMRS.csv")
    dmrs24 = read.csv("MD_FY2024_NPDES_DMRS.csv")
    message("ECHO data imported from local .csv files.")
  }
}
# PARAMETERS.XLSX FILE QUERY
parametersQuery = function() {
  response = as.character(readline(prompt = "Indicate whether you would like to download the parameter name key (yes/skip): "))
  response = ifelse(response %in% c("Yes", "yes", "YES", "Y", "y"), TRUE, FALSE)
  if(response == TRUE){
    url = "https://github.com/PetrAqua/aquaData/raw/refs/heads/main/parameters.xlsx"
    temp_file = tempfile(fileext = ".xlsx")
    download.file(url, destfile = temp_file, mode = "wb")
    parameters = as.data.frame(read_xlsx(temp_file))
    message("parameters.xlsx imported to R via Github.")
  } else {
    parameters = as.data.frame(read_xlsx("parameters.xlsx"))
    message("parameters.xlsx imported from local .csv file.")
  }
}
# EXPORT TABLES QUERY
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
    writeDataTable(wb, "Raw Data", dmrs)
    for (i in seq_along(outfalls_list)) {
      outfalls_list[[i]]$writeToExcelWorksheet(wb = wb, wsName = paste0(names(outfalls_list)[i], " Table"),
                                               topRowNumber = 1, leftMostColumnNumber = 1,
                                               applyStyles = TRUE, outputValuesAs="formattedValueAsText")
    }
    saveWorkbook(wb, file = paste0(glue("{getwd()}/{dmrs$EXTERNAL_PERMIT_NMBR[[1]]}_Data.xlsx")), overwrite = TRUE)
    message(paste0(glue("Tables exported to {getwd()}/{dmrs$EXTERNAL_PERMIT_NMBR[[1]]}_Data.xlsx")))
    shell.exec(paste0("file:", getwd()))
  } else {
    message("Tables not exported.")
  }
}

################################################################################
### RUN SETUP QUERIES ###

querySetup()
parametersQuery()
downloadQuery()

# FINAL DATA COMPILATION

dmrs = rbind(dmrs23, dmrs24)

dat = dmrs %>%
  mutate(MONITORING_PERIOD_END_DATE = as.Date(MONITORING_PERIOD_END_DATE,
                                              format = "%m/%d/%Y")) %>%
  filter(EXTERNAL_PERMIT_NMBR == npdesID,
         MONITORING_PERIOD_END_DATE > minDate,
         MONITORING_PERIOD_END_DATE <= maxDate) %>%
  inner_join(parameters[,c(1,3)], join_by(PARAMETER_CODE),
             relationship = "many-to-many") %>%
  mutate(parameter_name = glue("{parameter_alias} ({LIMIT_UNIT_DESC})"),
         limit_table_name = "Limit")

################################################################################
### SUBSETTING THE FACILITY DATA ###

effluent = dat %>%
  filter(PERM_FEATURE_TYPE_CODE != "WEL" & !grepl("MW", PERM_FEATURE_NMBR))
# Split effluent data.frame into list of grouped_dfs
outfalls = split(effluent, effluent$PERM_FEATURE_NMBR)
# Create empty list to use later for recalling outfall tables
outfalls_list = list()
# Creates separate grouped_dfs named by outfall
for (i in 1:length(outfalls)) {
  assign(paste0(
    unique(outfalls[[i]]$PERM_FEATURE_TYPE_CODE), "_",
    unique(outfalls[[i]]$PERM_FEATURE_NMBR)),
    outfalls[[i]])
}

wells = dat %>%
  filter(PERM_FEATURE_TYPE_CODE == "WEL" | grepl("MW", PERM_FEATURE_NMBR)) %>%
  # Extracts numeric well numbers from alphanumeric permit_feature_nmbr column
  mutate(strcapture("(.*?)([[:digit:]]+)", PERM_FEATURE_NMBR,
                    proto = list(MW_prefix = "", wellNo = as.numeric()))) %>%
  arrange(wellNo) %>%
  # Orders column by numeric instead of alphabetical order
  mutate(PERM_FEATURE_NMBR = factor(PERM_FEATURE_NMBR,
                                    unique(PERM_FEATURE_NMBR)))

effluent_limits = effluent %>%
  select(PERM_FEATURE_NMBR, PARAMETER_CODE, PARAMETER_DESC, STATISTICAL_BASE_TYPE_CODE,
         LIMIT_VALUE_ID, LIMIT_VALUE_NMBR, LIMIT_UNIT_DESC) %>%
  distinct()
# Test if there are multiple limits for the same statistic type and parameter (e.g. DP-2801)
multiTest = nrow(distinct(effluent_limits[,c("PARAMETER_DESC","STATISTICAL_BASE_TYPE_CODE")])) == nrow(distinct(effluent_limits[,c("PARAMETER_DESC","LIMIT_VALUE_ID")]))
# Split effluent_limits data.frame into list of grouped_dfs
outfall_limits = split(effluent_limits, effluent_limits$PERM_FEATURE_NMBR)
# Creates separate grouped_dfs named by outfall
for (i in 1:length(outfall_limits)) {
  assign(paste0(
    unique(outfalls[[i]]$PERM_FEATURE_TYPE_CODE), "_",
    unique(outfall_limits[[i]]$PERM_FEATURE_NMBR), "_limits"),
    outfall_limits[[i]])
}

gw_limits = wells %>%
  select(PARAMETER_CODE, PARAMETER_DESC, STATISTICAL_BASE_TYPE_CODE,
         LIMIT_VALUE_NMBR, LIMIT_UNIT_DESC) %>%
  distinct()

################################################################################
### CREATING THE EFFLUENT SUMMARY TABLE(S) ###

# Pivot table(s) of effluent values
for (i in seq_along(outfalls)){
  effluent_pivot = PivotTable$new()
  effluent_pivot$addData(outfalls[[i]])
  effluent_pivot$addRowDataGroups("MONITORING_PERIOD_END_DATE",
                                  dataFormat = list(format = "%B"),
                                  addTotal = FALSE)
  effluent_pivot$addColumnDataGroups("parameter_name",
                                     addTotal = FALSE)
  effluent_pivot$addColumnDataGroups("STATISTICAL_BASE_TYPE_CODE",
                                     addTotal = FALSE)
  if(multiTest == FALSE) {effluent_pivot$addColumnDataGroups("LIMIT_VALUE_ID",
                                                             addTotal = FALSE)}
  effluent_pivot$defineCalculation(calculationName = paste0("meanValue_", i),
                                   summariseExpression = "mean(DMR_VALUE_NMBR, na.rm = T)")
  effluent_pivot$evaluatePivot()
  effluent_basic = effluent_pivot$asBasicTable()
  # Pivot table of effluent limits
  effluent_limit_pivot = PivotTable$new()
  effluent_limit_pivot$addData(outfalls[[i]])
  effluent_limit_pivot$addRowDataGroups("limit_table_name",
                                        addTotal = FALSE)
  effluent_limit_pivot$addColumnDataGroups("parameter_name",
                                           addTotal = FALSE)
  effluent_limit_pivot$addColumnDataGroups("STATISTICAL_BASE_TYPE_CODE",
                                           addTotal = FALSE)
  if(multiTest == FALSE) {effluent_limit_pivot$addColumnDataGroups("LIMIT_VALUE_ID",
                                                                   addTotal = FALSE)}
  effluent_limit_pivot$defineCalculation(calculationName = paste0("meanLimit_", i),
                                         summariseExpression = "unique(na.omit(LIMIT_VALUE_NMBR))")
  effluent_limit_pivot$evaluatePivot()
  effluent_limit_basic = effluent_limit_pivot$asBasicTable()
  # Combining effluent tables
  effluent_basic$cells$insertRow(effluent_basic$rowCount + 1)
  effluent_basic$cells$setRow(effluent_basic$rowCount,
                              startAtColumnNumber = 1,
                              cellTypes = c("rowHeader"),
                              effluent_limit_basic$cells$getRowValues(rowNumber = 4,
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

################################################################################
### CREATING THE GROUNDWATER SUMMARY TABLE ###

# Pivot table of GW values
gw_pivot = PivotTable$new()
gw_pivot$addData(wells)
gw_pivot$addRowDataGroups("PERM_FEATURE_NMBR",
                          addTotal = FALSE)
gw_pivot$addColumnDataGroups("parameter_name",
                             addTotal = FALSE)
gw_pivot$defineCalculation(calculationName = "meanValue",
                           summariseExpression = "mean(DMR_VALUE_NMBR, na.rm = T)")
gw_pivot$evaluatePivot()
gw_basic = gw_pivot$asBasicTable()
# Pivot table of GW limits
gw_limit_pivot = PivotTable$new()
gw_limit_pivot$addData(wells)
gw_limit_pivot$addRowDataGroups("limit_table_name",
                                addTotal = FALSE)
gw_limit_pivot$addColumnDataGroups("parameter_name",
                                   addTotal = FALSE)
gw_limit_pivot$defineCalculation(calculationName = "meanLimit",
                                 summariseExpression = "mean(LIMIT_VALUE_NMBR, na.rm = T)")
gw_limit_pivot$evaluatePivot()
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

################################################################################
### RUN EXPORT QUERY ###

DMR_export()
