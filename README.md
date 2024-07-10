# aquaData

Tools for water quality and hydrogeologic analysis

-   Scraping EPA ECHO database for DMR data (DMR_Parser.r)
-   More to come...

## DMR_Parser.r

![](images/DMR%20Parser%20Diagram.png)

This script retrieves discharge monitoring data from EPA's Enforcement and Compliance History Online ([ECHO](https://echo.epa.gov/ "echo.epa.gov")) database using the `ECHO_data()` function and **parameters.xlsx** as a key for EPA's STOrage and RETrieval (STORET) respository designations for water quality parameters.

This script is designed to be accessible to beginner R users. Custom functions prompt users to input project-specific information into the console so that text in the script itself does not need to be altered. A user running every line of the script and responding to the console prompts will end up with summarized discharge monitoring data for any project with an NPDES ID. This summary data is exported as an .xlsx file to the user's specified directory.

*Warning: ECHO was not designed for automated queries and does not have an Application Programming Interface (API). If the website is queried too many times in quick succession, EPA may temporarily block the user's computer from making further queries.*
