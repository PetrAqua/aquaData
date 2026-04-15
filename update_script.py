from arcgis.gis import GIS
from arcgis.features import FeatureLayerCollection
import requests
import os

# 1. Connect to ArcGIS Online (automatically uses your Notebook credentials)
gis = GIS("home")

# 2. Define IDs and URLs
geojson_url = "https://raw.githubusercontent.com/PetrAqua/aquaData/main/ETS_Report_geo.geojson"
feature_layer_item_id = "8d1944d08b79449fbd21401826cdc7ef"

# 3. Download the latest GeoJSON from GitHub to the Notebook's local storage
response = requests.get(geojson_url)
file_path = "/arcgis/home/ETS_Report_geo.geojson"
with open(file_path, 'wb') as f:
    f.write(response.content)

# 4. Access the Hosted Feature Layer and Overwrite
flc = FeatureLayerCollection.fromitem(gis.content.get(feature_layer_item_id))
flc.manager.overwrite(file_path)

print("Permit status layer updated successfully.")

csv_url = "https://github.com/PetrAqua/aquaData/raw/main/ETS_Inspections_sheet.csv"
feature_layer_item_id = "0a9a8f6e29c64aef8af9de4e8e9ea6a3"

response = requests.get(csv_url)
file_path = "/arcgis/home/ETS_Inspections_sheet.csv"
with open(file_path, 'wb') as f:
    f.write(response.content)

flc = FeatureLayerCollection.fromitem(gis.content.get(feature_layer_item_id))
flc.manager.overwrite(file_path)

print("Inspections table layer updated successfully.")