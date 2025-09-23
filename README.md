# aquaData

Tools for water quality and hydrogeologic analysis

-   Scraping EPA ECHO database for DMR data (DMR_Parser.r)
-   More to come...

## DMR_Parser.r

![](images/DMR%20Parser%20Diagram.png)

This script retrieves discharge monitoring data from EPA's Enforcement and Compliance History Online ([ECHO](https://echo.epa.gov/ "echo.epa.gov")) database using the `ECHO_data()` function and **parameters.xlsx** as a key for EPA's STOrage and RETrieval (STORET) respository designations for water quality parameters.

This script is designed to be accessible to beginner R users. Custom functions prompt users to input project-specific information into the console so that text in the script itself does not need to be altered. A user running every line of the script and responding to the console prompts will end up with summarized discharge monitoring data for any project with an NPDES ID. This summary data is exported as an .xlsx file to the user's specified directory.

*Warning: ECHO was not designed for automated queries and does not have an Application Programming Interface (API). If the website is queried too many times in quick succession, EPA may temporarily block the user's computer from making further queries.*

## DMR_Parser_v3.0.R

This script retrieves discharge monitoring data from EPA's Enforcement and Compliance History Online ([ECHO](https://echo.epa.gov/ "echo.epa.gov")) database using bulk data grouped by fiscal year and **parameters.xlsx** as a key for EPA's STOrage and RETrieval (STORET) respository designations for water quality parameters.

Version 3.0 allows the user to quickly summarize data from ECHO's data for all facilities by fiscal year. Currently, Version 3.0 allows the user to summarize data from fiscal years 2023 and 2024. This version is meant for users summarizing recent annual data from calandar year 2023 who wish to run the scripts without an internet connection, or who intend to run multiple summarizations and do not want to risk being locked out of ECHO by performing too many queries.

## DMR_Parser_Formatted.R

This script is like the original DMR_Parser.R script, but provides an output that is formatted more closely to the MDE Groundwater Discharge Permits Divsion fact sheet tables.

## app.R

This Shiny app is a version of the DMR_Parser_Formatted.R code that can be published and used outside of an R IDE. The application allows for display and inspection of the raw data taken from ECHO, as well as renders the formatted DMR summary tables. The raw data and tables can be exported from the application to an Excel file.
