# Direct SQL Server ETS Connection Script (Initial draft 2/12/26)

start_time <- Sys.time()
message(paste(start_time, " - START: R Script execution started."))

library(tidyverse)
library(DBI)
library(odbc)
library(openxlsx)
library(gert)
library(sf)

message(paste(Sys.time(), " - STATUS: Starting data creation and saving."))

con <- dbConnect(odbc::odbc(),
                 Driver = "SQL Server",
                 Server = "MDE-ETSSQL01P",
                 Database = "MDE_ETSReport",
                 uid = "MDE_MESReadUser",
                 pwd = "MDE_MESReadU5er2o20!")

# Future connection to ETS inspections data

# inspections_df <- tbl(con, Id(schema = "WMA", table = "v_wma_inspections_rs")) %>%
#   filter(permit_num %like% "%DP%" &
#            media_type %like% "%Ground%") %>%
#   collect()

permits <- tbl(con, Id(schema = "MES", table = "MV_WMA_EPSC_PERMIT")) %>%
  mutate(cur_status_comments = sql("CAST(cur_status_comments AS VARCHAR(4000))")) %>%
  filter(permit_type == "Groundwater") %>%
  collect()
facilities <- tbl(con, Id(schema = "MES", table = "MV_WMA_EPSC_FACILITY")) %>%
  collect()
wal <- tbl(con, Id(schema = "MES", table = "V_WMA_WWPP_WAL")) %>%
  collect()

dbDisconnect(con)

per_fac <- full_join(permits, facilities, by = "fac_num")
per_fac_wal <- full_join(per_fac, wal, by = join_by("wal_int_doc_id" == "int_doc_id"), relationship = "many-to-many")

cleandbwal <- per_fac_wal %>%
  filter(permit_type == "Groundwater") %>%
  select("permit_num","app_num","npdes_num","master_ai_name" = "master_ai_name.x","COUNTY_DESC" = "parish_or_county_desc","perwriter_userid",
         "ptype","permit_class","permit_mode","cur_status_descr","cur_status_date",
         "orig_sub_date","lst_issu",
         "exp_date","approval_issued",
         "prev_permit_exp", "effective_end_date",
         "custom_task_desc","completed_date") %>%
  mutate(cur_status_date = as.Date(cur_status_date),
         orig_sub_date = as.Date(orig_sub_date),
         lst_issu = as.Date(lst_issu),
         exp_date = as.Date(exp_date),
         approval_issued = as.Date(approval_issued),
         prev_permit_exp = as.Date(prev_permit_exp),
         effective_end_date = as.Date(effective_end_date),
         completed_date = as.Date(completed_date),
         permit_number = ifelse(is.na(permit_num), app_num, permit_num),
         permit_id = substr(permit_number, 3, 8)) %>%
  select("permit_id","permit_number","npdes_num","master_ai_name","COUNTY_DESC",
         "perwriter_userid","ptype","permit_class","permit_mode","cur_status_descr","cur_status_date",
         "prev_permit_exp","orig_sub_date",
         "custom_task_desc","completed_date",
         "approval_issued","lst_issu","exp_date", "effective_end_date") %>%
  mutate(custom_task_desc_rank = case_when(
    custom_task_desc == "OIM - First notice published" ~ 2,
    custom_task_desc == "OIM - Second notice published" ~ 3,
    custom_task_desc == "OIM - Meeting request period ends" ~ 4,
    custom_task_desc == "TDOPH - First notice published" ~ 8,
    custom_task_desc == "TDOPH - Second notice published" ~ 9,
    custom_task_desc == "TDOPH - Hearing request period ends" ~ 10,
    custom_task_desc == "TDOPH - Written comment period ends" ~ 11,
    is.na(custom_task_desc) ~ 1, # Usually prior to AR publication
    custom_task_desc == "PH - Notice of hearing published" ~ 15,
    custom_task_desc == "PH - Hearing date" ~ 16,
    custom_task_desc == "PH - Comment Period extended until" ~ 14,
    custom_task_desc == "Final Determination - First notice published" ~ 19,
    custom_task_desc == "Final Determination - Second notice published" ~ 20,
    custom_task_desc == "IM - Notice of meeting published" ~ 6,
    custom_task_desc == "IM - Meeting date" ~ 7,
    custom_task_desc == "TDOPH - Date hearing requested" ~ 12,
    custom_task_desc == "TDOPH - Public Docket Complete" ~ 13,
    custom_task_desc == "OIM - Date Meeting requested" ~ 5,
    custom_task_desc == "Final Determination - Response to comments approved" ~ 18,
    custom_task_desc == "Final Determination - Response to comments drafted"  ~ 17,
    TRUE ~ 21 # If no matches
  )) %>%
  mutate(custom_task_desc_cat = case_when(
    custom_task_desc == "OIM - First notice published" ~ "AR Notice",
    custom_task_desc == "OIM - Second notice published" ~ "AR Notice",
    custom_task_desc == "OIM - Meeting request period ends" ~ "AR Notice",
    custom_task_desc == "TDOPH - First notice published" ~ "TD Notice",
    custom_task_desc == "TDOPH - Second notice published" ~ "TD Notice",
    custom_task_desc == "TDOPH - Hearing request period ends" ~ "TD Notice",
    custom_task_desc == "TDOPH - Written comment period ends" ~ "TD Notice",
    is.na(custom_task_desc) ~ "", # Usually prior to AR publication
    custom_task_desc == "PH - Notice of hearing published" ~ "Public Hearing",
    custom_task_desc == "PH - Hearing date" ~ "Public Hearing",
    custom_task_desc == "PH - Comment Period extended until" ~ "Public Hearing",
    custom_task_desc == "Final Determination - First notice published" ~ "FD Notice",
    custom_task_desc == "Final Determination - Second notice published" ~ "FD Notice",
    custom_task_desc == "IM - Notice of meeting published" ~ "Informational Meeting",
    custom_task_desc == "IM - Meeting date" ~ "Informational Meeting",
    custom_task_desc == "TDOPH - Date hearing requested" ~ "Public Hearing",
    custom_task_desc == "TDOPH - Public Docket Complete" ~ "Public Hearing",
    custom_task_desc == "OIM - Date Meeting requested" ~ "Informational Meeting",
    custom_task_desc == "Final Determination - Response to comments approved" ~ "FD Notice",
    custom_task_desc == "Final Determination - Response to comments drafted"  ~ "FD Notice",
    TRUE ~ "" # If no matches
  )) %>%
  mutate(added_status_desc = case_when(
    cur_status_descr == "Issued" ~ paste(cur_status_descr, lst_issu),
    cur_status_descr == "Issued - Admin Extended" ~ paste(cur_status_descr, exp_date),
    cur_status_descr == "Approval Terminated" ~ paste(cur_status_descr, effective_end_date),
    cur_status_descr == "Application Withdrawn" ~ paste(cur_status_descr, cur_status_date),
    cur_status_descr == "Pending Effective" ~ paste(cur_status_descr, lst_issu),
    cur_status_descr == "Pending" ~ paste(custom_task_desc_cat, completed_date),
    TRUE ~ "Status Unavailable"
  )) %>%
  group_by(permit_id) %>%
  mutate(latest = permit_number == max(permit_number) &
           custom_task_desc_rank == max(custom_task_desc_rank[permit_number == max(permit_number)])) %>%
  mutate(latest = as.integer(latest)) %>%
  ungroup() %>%
  filter(is.na(npdes_num) == FALSE) %>%
  arrange(desc(completed_date))

geoDat <- read.xlsx("https://github.com/PetrAqua/aquaData/raw/refs/heads/main/Groundwater_Discharge_Permit_Coordinates.xlsx") # Coordinates are in Web Mercator (meters)
geoDat2 <- geoDat %>%
  select(X,Y,NPDES.Number,Permit.Number,Facility.Name) %>%
  mutate(permit_id = str_sub(Permit.Number,3, 8)) %>%
  distinct(permit_id, .keep_all = TRUE)

etsDat <- cleandbwal

# There are ~170 facilities in the ETS data table that have not been geocoded in the GIS data table - will need to fix in the future

gwDat <- etsDat %>% # All data with geocoding
  full_join(geoDat2, by = "permit_id") %>%
  select(-c(NPDES.Number,Permit.Number,Facility.Name)) %>%
  filter(is.na(X) == FALSE)


sf_object <- st_as_sf(gwDat, coords = c("X", "Y"), crs = 3857) # WGS84 equivalent (Meters / Web Mercator)

sf_object_geo <- st_transform(sf_object, 4326) # Transform to WGS84 decimal system

output_file <- "C://Users/lhudson/Documents/aquaData/ETS_Report_geo.geojson"

st_write(sf_object_geo, output_file, driver = "GeoJSON", delete_dsn = TRUE)

repo_path <- "C://Users/lhudson/Documents/aquaData"

git_open(repo_path)

message(paste(Sys.time(), " - STATUS: Starting Git stage/commit."))

git_add("ETS_Report_geo.geojson", repo = repo_path)

gert::git_config_set("user.name", "MDE GDPD R Script", repo = repo_path)
gert::git_config_set("user.email", "lukehudson422@yahoo.com", repo = repo_path)

status_df <- gert::git_status(repo = repo_path)

has_changes_to_commit <- any(
  status_df$file == "ETS_Report_geo.geojson" & status_df$staged)

if (has_changes_to_commit) {
  message(paste(Sys.time(), " - SUCCESS: Changes detected. Proceeding with commit and push."))

  gert::git_commit("Automated table update", repo = repo_path)

  message(paste(Sys.time(), " - STATUS: Pulling remote changes..."))
  gert::git_pull(remote = "origin", rebase = TRUE, repo = repo_path)

  message(paste(Sys.time(), " - STATUS: Pushing updates to remote..."))
  gert::git_push(remote = "origin", repo = repo_path)

  message(paste(Sys.time(), " - COMPLETE: Repository successfully updated."))
} else {
  message(paste(Sys.time(), " - ALERT: No changes detected in ETS_Report_geo.geojson. Script finished without commit/push."))
}

invisible(TRUE)
