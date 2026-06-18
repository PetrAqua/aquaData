# Direct SQL Server ETS Connection Script (Initial draft 2/12/26)
# Added inspections data from ETS to script (2/24/26)
# Added ArcGIS Online layer update codes, VW and CT support, and more coordinates (4/16/26)
# Removed as.Date() functions from inspections_df to fix ArcGIS importing issues (6/18/26)

start_time <- Sys.time()
message(paste(start_time, " - START: R Script execution started."))

library(tidyverse)
library(DBI)
library(odbc)
library(openxlsx)
library(gert)
library(sf)

message(paste(Sys.time(), " - STATUS: Starting data creation and saving."))

options(scipen = 999)

con <- dbConnect(odbc::odbc(),
                 Driver = "SQL Server",
                 Server = "MDE-ETSSQL01P",
                 Database = "MDE_ETSReport",
                 uid = "MDE_MESReadUser",
                 pwd = "MDE_MESReadU5er2o20!")

permits <- tbl(con, Id(schema = "MES", table = "MV_WMA_EPSC_PERMIT")) %>%
  mutate(cur_status_comments = sql("CAST(cur_status_comments AS VARCHAR(4000))")) %>%
  filter(permit_type == "Groundwater") %>%
  collect()
facilities <- tbl(con, Id(schema = "MES", table = "MV_WMA_EPSC_FACILITY")) %>%
  collect()
wal <- tbl(con, Id(schema = "MES", table = "V_WMA_WWPP_WAL")) %>%
  collect()
inspections_df <- tbl(con, Id(schema = "WMA", table = "v_wma_inspections_rs")) %>%
  filter(media_type %like% "%al Ground Water%") %>%
  collect() %>%
  select(-county, -approvedbysupervisor, -mda_operator_id, -reference_task_id) %>%
  unique()

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
  mutate(permit_number = ifelse(is.na(permit_num), app_num, permit_num),
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
    cur_status_descr == "Issued" ~ paste(cur_status_descr, as.Date(lst_issu)),
    cur_status_descr == "Issued - Admin Extended" ~ paste(cur_status_descr, as.Date(exp_date)),
    cur_status_descr == "Approval Terminated" ~ paste(cur_status_descr, as.Date(effective_end_date)),
    cur_status_descr == "Application Withdrawn" ~ paste(cur_status_descr, as.Date(cur_status_date)),
    cur_status_descr == "Pending Effective" ~ paste(cur_status_descr, as.Date(lst_issu)),
    cur_status_descr == "Pending" ~ paste(custom_task_desc_cat, as.Date(completed_date)),
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

# Unconfirmed coordinates taken from ETS facility table as subbedCoords - facilities with no ETS coordinates subset as missingCoords table
# Some missingCoords facilities have addresses that can be geocoded in the future - subbedCoords facilities can be validated with addresses in future too

gwDat <- etsDat %>% # All data with geocoding in xlsx file
  full_join(geoDat2, by = "permit_id") %>%
  mutate(master_ai_name = coalesce(master_ai_name, Facility.Name),
         permit_number = coalesce(permit_number, Permit.Number),
         npdes_num = coalesce(npdes_num, NPDES.Number),
         ptype = if_else(is.na(ptype) & grepl("CT|VW", permit_id), "WMA5", ptype)) %>%
  select(-c(NPDES.Number,Permit.Number,Facility.Name))

match_keys <- facilities %>% # Allow matching of facility name across multiple alternate name columns in facilities data frame
  mutate(row_id = row_number()) %>% 
  pivot_longer(
    cols = c(master_ai_name, DOC_MASTER_AI_NAME, alternate_ai_name, FAC_NAME),
    values_to = "join_key",
    values_drop_na = TRUE
  ) %>%
  distinct(row_id, join_key)

noCoords <- unique(gwDat[is.na(gwDat$X) & gwDat$latest == 1,c(1:4, 9, 22:25)]) # Sites with no coordinates from xlsx
noCoords_subbed <- noCoords %>% # Sites with no coordinates matched to ETS coordinates
  left_join(match_keys, by = c("master_ai_name" = "join_key")) %>%
  left_join(facilities[,c(6:14, 42:43)] %>%
  mutate(row_id = row_number()), by = "row_id") %>%
  select(-row_id) %>%
  distinct(master_ai_name.x, .keep_all = TRUE) %>%
  mutate(X = coalesce(X, x_coord_standard_value),
         Y = coalesce(Y, y_coord_standard_value)) %>%
  select(1:4, 8:9, 14:18)
subbedCoords <- noCoords_subbed %>% # All sites that have ETS coordinates
  filter(is.na(X) == FALSE)
missingCoords <- noCoords_subbed %>% # All sites without ETS coordinates
  filter(is.na(X))
subbedCoords_sf <- st_as_sf(subbedCoords, coords = c("X", "Y"), crs = 4326)
subbedCoords_t <- st_transform(subbedCoords_sf, 3857) # Convert to xlsx coordinate system
subbedCoords2 <- subbedCoords_t %>%
  mutate(X = st_coordinates(.)[, "X"],
         Y = st_coordinates(.)[, "Y"]) %>%
  st_drop_geometry()

gwDat2 <- gwDat %>% # All data with geocoding joined to ETS coordinate sites
  left_join(subbedCoords2[,c(4, 10:11)], by = join_by(master_ai_name == master_ai_name.x)) %>%
  mutate(X = coalesce(X.x, X.y),
         Y = coalesce(Y.x, Y.y)) %>%
  filter(is.na(X) == FALSE) %>%
  select(-c(24:27))

sf_object <- st_as_sf(gwDat2, coords = c("X", "Y"), crs = 3857) # WGS84 equivalent (Meters / Web Mercator)

sf_object_geo <- st_transform(sf_object, 4326) # Transform to WGS84 decimal system

output_file <- "C://Users/lhudson/Documents/aquaData/ETS_Report_geo.geojson"

st_write(sf_object_geo, output_file, driver = "GeoJSON", delete_dsn = TRUE)

insp_file <- "C://Users/lhudson/Documents/aquaData/ETS_Inspections_sheet.csv"

insp_tbl <- write_csv(inspections_df, insp_file)

repo_path <- "C://Users/lhudson/Documents/aquaData"

git_open(repo_path)

message(paste(Sys.time(), " - STATUS: Starting Git stage/commit."))

git_add("ETS_Report_geo.geojson", repo = repo_path)
git_add("ETS_Inspections_sheet.csv", repo = repo_path)

gert::git_config_set("user.name", "MDE GDPD R Script", repo = repo_path)
gert::git_config_set("user.email", "lukehudson422@yahoo.com", repo = repo_path)

status_df <- gert::git_status(repo = repo_path)

has_changes_to_commit <- any(
  status_df$file == "ETS_Report_geo.geojson" & status_df$staged,
  status_df$file == "ETS_Inspections_sheet.csv" & status_df$staged)

if (has_changes_to_commit) {
  message(paste(Sys.time(), " - SUCCESS: Changes detected. Proceeding with commit and push."))

  gert::git_commit("Automated table update", repo = repo_path)

  message(paste(Sys.time(), " - STATUS: Pulling remote changes..."))
  gert::git_pull(remote = "origin", rebase = TRUE, repo = repo_path)

  message(paste(Sys.time(), " - STATUS: Pushing updates to remote..."))
  gert::git_push(remote = "origin", repo = repo_path)

  message(paste(Sys.time(), " - COMPLETE: Repository successfully updated."))
} else {
  message(paste(Sys.time(), " - ALERT: No changes detected in data. Script finished without commit/push."))
}

### Using R to update the layers in ArcGIS Online ###

library(arcgisutils)
library(arcgislayers)

client_id <- Sys.getenv("ARCGIS_CLIENT_ID")
client_secret <- Sys.getenv("ARCGIS_CLIENT_SECRET")

token <- auth_client(client_id, client_secret)
set_arc_token(token)

print("Reading local data...")
geo_data <- sf_object_geo
csv_data <- inspections_df

geo_layer_url <- "https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/ETS_Report_geojson_hosted/FeatureServer/0"
csv_table_url <- "https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/ETS_Inspections_Table/FeatureServer/0"

geo_layer <- arc_open(geo_layer_url)
csv_table <- arc_open(csv_table_url)

print("Updating spatial layer...")
geo_data_matched <- st_transform(geo_data, crs = st_crs(geo_layer))
delete_features(geo_layer, where = "1=1")
add_features(geo_layer, geo_data_matched)

print("Updating inspections table...")
delete_features(csv_table, where = "1=1")
add_features(csv_table, csv_data)

print("ArcGIS layers updated successfully!")

invisible(TRUE)
