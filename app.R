################################################################################
##### DMR Table Shiny App ######################################################
################################################################################
############################################################### Setup codes ####
library(shiny)
library(bslib)
library(conflicted) # Shows error if functions from multiple packages conflict
library(devtools) # Allows installation of packages from Github
library(tidyverse) # Main collection of packages for QoL
conflicts_prefer(dplyr::filter, dplyr::mutate, dplyr::arrange, readxl::read_xlsx)
library(readxl)
library(reshape2)
library(pivottabler); library(basictabler) # For making pivot tables
library(glue) # For building strings from variables
library(openxlsx) # Used to export tables to Excel
library(DT)
library(htmlwidgets)
library(officer)
library(flextable)

options(scipen=999)

## Custom functions in use ##
ECHO_data <- function(NPDES_ID, start_date, end_date) {
  start_date = format(as.Date(start_date), "%m/%d/%Y")
  end_date = format(as.Date(end_date), "%m/%d/%Y")
  url = paste0("https://echodata.epa.gov/echo/eff_rest_services.download_effluent_chart?",
               "p_id=", NPDES_ID,
               "&start_date=", start_date,
               "&end_date=", end_date,
               "&p_echo=Y")
  data = as.data.frame(read_csv(url))
  return(data)
}
nodiLogic <- function(nodi_code, dmr_value_nmbr, violation_code) {
  if(all(is.na(nodi_code))) {
    return(paste0(ifelse(dmr_value_qualifier_code=='<', dmr_value_qualifier_code,''),
                  mean(dmr_value_nmbr, na.rm = TRUE),
                  ifelse(is.na(violation_code)==TRUE,'','*')))
  } else {
    return(nodi_code[!is.na(nodi_code)][1])
  }
}
fx <- function(v, cell) { # Highlights violations detected in NetDMR
  if(isTRUE(str_detect(v,"[*]"))) return("#f1a983")
  if(isTRUE(str_detect(v,"E"))) return("#f1a984")
  return("#a6c9ec")
}

################################################################################
######################################################################## UI ####
ui <- page_sidebar(
  title = "DMR Table Generator - beta v0.5",
  sidebar = sidebar(
    textInput("npdes", "Enter NPDES ID:", ""),
    textInput("date", "Enter Data Year:", "2024"),
    actionButton("load_data", "Load Data"),
    actionButton("generate_basic", "Generate DMR Tables"),
    downloadButton("download",
                   label = "Save as Excel")#,#tagList(icon("download"), "Save as Excel"))#,
    # downloadButton("download_docx",
                   # label = "Save as Word")
    # actionButton("download.docx",
    #              label = tagList(icon("download"), "Save as Word"))
  ),
  navset_card_underline(
    id = "my_tabs",
    nav_panel("Raw Data",
              value = "raw_data",
              DTOutput("raw_table")),
    nav_panel("Tables",
              value = "tables",
              card(
                card_header("Effluent"),
                uiOutput("basic_tabs")
              ),
              card(
                card_header("Groundwater"),
                basictablerOutput("gw_tbl")
              )
    )
  )
)
################################################################################
#################################################################### SERVER ####
server <- function(input, output, session) {
  
  
  data_raw <- eventReactive(input$load_data, {
    req(input$npdes, input$date)
    npdesID = input$npdes
    sdate = paste0(input$date,"-01-01")
    edate = paste0(input$date,"-12-31")
    ECHO_data(npdesID,sdate,edate)
  }) # Raw data
  
  output$raw_table <- renderDT({
    req(data_raw())
    datatable(
      data_raw(),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  params <- eventReactive(input$load_data, {
    # Download parameters table from Github
    url <- "https://github.com/PetrAqua/aquaData/raw/refs/heads/main/parameters.xlsx"
    temp_file <- tempfile(fileext = ".xlsx")
    download.file(url, destfile = temp_file, mode = "wb")
    parameters <- as.data.frame(read_xlsx(temp_file))
    parameters
  })
  
  dataPrep <- eventReactive(input$load_data, {
    req(data_raw(), params())
    
    # Run ECHO query function and assign output to table
    DMR_data <- data_raw()
    parameters <- params()
    # Subset raw data to useful columns and combine with parameter table
    dataR = DMR_data %>%
      select(perm_feature_id, perm_feature_nmbr, perm_feature_type_code, monitoring_period_end_date,
             parameter_code, parameter_desc, limit_value_id, limit_value_type_desc, limit_value_nmbr,
             limit_unit_desc, statistical_base_short_desc, dmr_value_nmbr, dmr_value_qualifier_code, nodi_code, nodi_desc,
             violation_code, violation_desc, rnc_resolution_code, rnc_resolution_desc) %>%
      mutate(monitoring_period_end_date = as_date(monitoring_period_end_date, format = "%m/%d/%Y")) %>%
      inner_join(parameters[,c(1,3)], join_by(parameter_code), relationship = "many-to-many") %>%
      mutate(parameter_name = glue("{parameter_alias} ({limit_unit_desc})"),
             limit_table_name = "Limit")
    
    # Subset effluent data
    effluent = dataR %>%
      filter(!grepl("MW", perm_feature_nmbr))
    effluent_limits = effluent %>%
      select(perm_feature_nmbr, parameter_code, parameter_desc, statistical_base_short_desc,
             limit_value_id, limit_value_nmbr, limit_unit_desc) %>%
      distinct()
    # Test if there are multiple limits for the same statistic type and parameter (e.g. DP-2801)
    multiTest = nrow(distinct(effluent_limits[,c("parameter_desc","statistical_base_short_desc")])) == nrow(distinct(effluent_limits[,c("parameter_desc","limit_value_id")]))
    # Split effluent data.frame into list of grouped_dfs
    outfalls = split(effluent, effluent$perm_feature_nmbr)
    
    outfalls
  }) # Outfalls
  
  dataPrepW <- eventReactive(input$load_data, {
    req(data_raw(), params())
    
    # Run ECHO query function and assign output to table
    DMR_data <- data_raw()
    parameters <- params()
    # Subset raw data to useful columns and combine with parameter table
    dataR = DMR_data %>%
      select(perm_feature_id, perm_feature_nmbr, perm_feature_type_code, monitoring_period_end_date,
             parameter_code, parameter_desc, limit_value_id, limit_value_type_desc, limit_value_nmbr,
             limit_unit_desc, statistical_base_short_desc, dmr_value_nmbr, dmr_value_qualifier_code, nodi_code, nodi_desc,
             violation_code, violation_desc, rnc_resolution_code, rnc_resolution_desc) %>%
      mutate(monitoring_period_end_date = as_date(monitoring_period_end_date, format = "%m/%d/%Y")) %>%
      inner_join(parameters[,c(1,3)], join_by(parameter_code), relationship = "many-to-many") %>%
      mutate(parameter_name = glue("{parameter_alias} ({limit_unit_desc})"),
             limit_table_name = "Limit")
    wells = dataR %>%
      filter(grepl("MW", perm_feature_nmbr)) %>%
      # Extracts numeric well numbers from alphanumeric permit_feature_nmbr column
      mutate(strcapture("(.*?)([[:digit:]]+)", perm_feature_nmbr,
                        proto = list(MW_prefix = "", wellNo = as.numeric()))) %>%
      arrange(wellNo) %>%
      # Orders column by numeric instead of alphabetical order
      mutate(perm_feature_nmbr = factor(perm_feature_nmbr,
                                        unique(perm_feature_nmbr)))
    
    wells
  }) # Wells
  
################################################################################
###################################################### Effluent table codes ####
  basic_list <- eventReactive(input$generate_basic, {
    req(dataPrep())
    
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
    outfalls <- dataPrep()
    b_list <- list()
    for(i in seq_along(outfalls)) {
      effluent_pivot = PivotTable$new()
      effluent_pivot$addData(outfalls[[i]])
      effluent_pivot$addRowDataGroups("monitoring_period_end_date",
                                      dataFormat = list(format = "%B"),
                                      addTotal = FALSE)
      effluent_pivot$addColumnDataGroups("parameter_name",
                                         addTotal = FALSE)
      effluent_pivot$addColumnDataGroups("statistical_base_short_desc",
                                         addTotal = FALSE)
      effluent_pivot$defineCalculation(calculationName = paste0("meanValue_", i),
                                       summariseExpression = "{
                                       if(all(is.na(nodi_code))) {
    paste0(ifelse(dmr_value_qualifier_code=='<',dmr_value_qualifier_code,''),
    mean(dmr_value_nmbr, na.rm = TRUE),
    ifelse(is.na(violation_code)==TRUE,'','*'))
  } else {
    paste0('NODI ', nodi_code[!is.na(nodi_code)][1])
  }
}[1]"
      )
      effluent_pivot$evaluatePivot()
      effluent_pivot$theme <- tableTheme
      
      effluent_pivot$mapStyling(cells=effluent_pivot$allCells, styleProperty="background-color", mappings=fx)
      effluent_basic = effluent_pivot$asBasicTable()
      effluent_limit_pivot = PivotTable$new()
      effluent_limit_pivot$addData(outfalls[[i]])
      effluent_limit_pivot$addRowDataGroups("limit_table_name",
                                            addTotal = FALSE)
      effluent_limit_pivot$addColumnDataGroups("parameter_name",
                                               addTotal = FALSE)
      effluent_limit_pivot$addColumnDataGroups("statistical_base_short_desc",
                                               addTotal = FALSE)
      effluent_limit_pivot$defineCalculation(calculationName = paste0("meanLimit_", i),
                                             summariseExpression = "{
                                     paste0(ifelse(is.na(limit_value_nmbr)==TRUE,'N/A',
                                     mean(limit_value_nmbr, na.rm = T))
                                     )}[1]")
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
      b_list[[paste0("Outfall_",i)]] <- effluent_basic
    }
    b_list
  })
  
  output$basic_tabs <- renderUI({
    req(basic_list())
    tabs <- lapply(names(basic_list()), function(name) {
      tabPanel(
        title = name,
        basictablerOutput(outputId = paste0("pivot_", name))
      )
    })
    do.call(tabsetPanel, tabs)
  })
  
  observe({
    req(basic_list())
    for (name in names(basic_list())) {
      local({
        nm <- name
        output[[paste0("pivot_", nm)]] <- renderBasictabler({
          basictabler(basic_list()[[nm]])
        })
      })
    }
  })
###############################################################################
################################################## Groundwater table codes ####
  gw_tbl <- eventReactive(input$generate_basic, {
    req(dataPrepW())
    
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
    wells <- dataPrepW()
    gw_pivot = PivotTable$new()
    gw_pivot$addData(wells)
    gw_pivot$addRowDataGroups("perm_feature_nmbr",
                              addTotal = FALSE)
    gw_pivot$addColumnDataGroups("parameter_name",
                                 addTotal = FALSE)
    gw_pivot$addColumnDataGroups("statistical_base_short_desc",
                                 addTotal = FALSE)
    gw_pivot$defineCalculation(calculationName = "meanValue",
                               summariseExpression = "{
                                       if(all(is.na(nodi_code))) {
    paste0(ifelse(dmr_value_qualifier_code=='<',dmr_value_qualifier_code,''),
    mean(dmr_value_nmbr, na.rm = TRUE),
    ifelse(is.na(violation_code)==TRUE,'','*'))
  } else {
    paste0('NODI ', nodi_code[!is.na(nodi_code)][1])
  }
}[1]") # Not sure why this has to have [1] for it to work
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
    # print(
    #   summarise(
    #     group_by(wells, perm_feature_nmbr, parameter_code),
    #     unique_desc = n_distinct(statistical_base_short_desc),
    #     desc_le_site = unique_desc > 1
    #   ),
    #   n = 27
    # )
    # if(
    #   any(summarise(
    #     group_by(wells, parameter_code),
    #     unique_desc = n_distinct(statistical_base_short_desc),
    #     unique_site = n_distinct(perm_feature_nmbr),
    #     desc_le_site = unique_desc != unique_site
    #   )[,4])==TRUE
    # ) {
    gw_limit_pivot$addColumnDataGroups("statistical_base_short_desc",
                                       addTotal = FALSE)
    # }
    gw_limit_pivot$defineCalculation(calculationName = "meanLimit",
                                     summariseExpression = "{
                                     paste0(ifelse(is.na(limit_value_nmbr)==TRUE,'N/A',
                                     mean(limit_value_nmbr, na.rm = T))
                                     )}[1]")
    gw_limit_pivot$evaluatePivot()
    gw_limit_pivot$theme <- tableTheme
    
    gw_limit_basic = gw_limit_pivot$asBasicTable()
    
    gw_basic$cells$insertRow(gw_basic$rowCount + 1) # Combining GW tables
    gw_basic$cells$setRow(gw_basic$rowCount,
                          startAtColumnNumber = 1,
                          cellTypes = c("rowHeader"),
                          gw_limit_basic$cells$getRowValues(rowNumber = 3, # change back to 3 if using statistical_base_short_desc column data group
                                                            columnNumbers = 1:gw_limit_basic$columnCount,
                                                            formattedValue = FALSE,
                                                            asList = TRUE,
                                                            rebase = TRUE))
    gw_basic
  })
  
  output$gw_tbl <- renderBasictabler({
    req(gw_tbl())
    basictabler(gw_tbl())
  })
  
################################################################################
####################################################### Download file codes ####
  output$download <- downloadHandler(
    filename = function(){
      paste0(data_raw()$npdes_id[[1]],"_",input$date,"_Data.xlsx")
    },
    content = function(file) {
      wb = createWorkbook()
      for (i in seq_along(basic_list())) {
        addWorksheet(wb,
                     paste0(names(basic_list())[i],
                            " Table"))
      }
      addWorksheet(wb, "Groundwater Table")
      gw_tbl()$writeToExcelWorksheet(wb = wb, wsName = "Groundwater Table",
                                     topRowNumber = 1, leftMostColumnNumber = 1,
                                     applyStyles = TRUE,
                                     outputValuesAs="formattedValueAsText")
      addWorksheet(wb, "Raw Data")
      writeDataTable(wb, "Raw Data", data_raw())
      for (i in seq_along(basic_list())) {
        basic_list()[[i]]$writeToExcelWorksheet(wb = wb,
                                                wsName = paste0(names(basic_list())[i],
                                                                " Table"),
                                                topRowNumber = 1,
                                                leftMostColumnNumber = 1,
                                                applyStyles = TRUE,
                                                outputValuesAs="formattedValueAsText")
      }
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
################################################################################
# WORK IN PROGRESS #
  # output$download_docx <- downloadHandler(
  #   filename = function(){
  #     paste0(data_raw()$npdes_id[1], "_Data.docx")
  #   },
  #   content = function(file) {
  #     doc <- officer::read_docx()
  #     for (i in seq_along(basic_list())) {
  #     ft <- basic_list()$asFlexTable()
  #     doc <- body_add_flextable(doc, ft)
  #     doc <- body_add_break(doc)
  #     }
  #     gw_ft <- gw_tbl()$asFlexTable()
  #     doc <- body_add_flextable(doc, gw_ft)
  #     print(doc, target = file)
  #   }
  # )
################################################################################
  observeEvent(input$generate_basic, {
    nav_select("my_tabs", selected = "tables")
  })
################################################################################ 
}

shinyApp(ui, server)
