################################################################################
#
# Project:  Mediated effects of foreclosures on coursetaking
# Purpose:  Download BEA data
# Author:   Patrick Lavallee Delgado
# Created:  9 July 2024
#
# Notes:    BEA API documentation: https://apps.bea.gov/API/docs/index.htm
#           LineCode in the config file maps statistic ids to variable names.
#
# To do:
#
################################################################################

# Impact packages.
import argparse
import sys
import requests
import yaml
import pandas as pd
from typing import Any

# Identify API endpoint.
API = "https://apps.bea.gov/api/data"

# Identify unique identifiers on response.
IDXLIST = ["GeoFips", "GeoName", "TimePeriod"]


# Parse config file.
def get_params(path: str, key: str) -> tuple[dict[str, str], dict[int, str]]:

  # Load parameters from YAML file.
  with open(path) as f:
    params = yaml.safe_load(f)

  # Add request method and user key.
  params["UserID"] = key
  params["method"] = "GetData"

  # Fix years.
  params["Year"] = ",".join(map(str, params["Year"]))

  # Peel off column queue.
  varlist = params.pop("LineCode")

  # Return parameters and variable list.
  return params, varlist


# Request data.
def make_request(url: str, params: dict[str, Any]) -> pd.DataFrame:
  resp = requests.get(url, params).json()
  data = resp["BEAAPI"]["Results"]["Data"]
  return pd.DataFrame(data)


# Download from BEA.
def get_bea(config: str, key: str) -> pd.DataFrame:

  # Read config file.
  params, varlist = get_params(config, key)

  # Consider each column in the queue.
  dfs = []
  for idx, col in varlist.items():

    # Request this column.
    params["LineCode"] = idx
    df = (
      make_request(API, params)
      .rename(columns={"DataValue": col})
      .loc[:, IDXLIST + [col]]
    )

    # Register dataframe with all others.
    dfs.append(df)

  # Merge columns.
  dta = pd.DataFrame(columns=IDXLIST)
  for df in dfs:
    dta = dta.merge(df, how="outer", on=IDXLIST, validate="1:1")

  # Return dataset.
  return dta


# Run.
if __name__ == "__main__":

  # Parse arguments.
  parser = argparse.ArgumentParser()
  parser.add_argument("config", type=str)
  parser.add_argument("key", type=str)
  parser.add_argument("out", nargs="?", type=argparse.FileType("w"), default=sys.stdout)
  args = parser.parse_args()

  # Request and write data.
  get_bea(args.config, args.key).to_csv(args.out, index=False)
