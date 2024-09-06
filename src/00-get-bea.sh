#!/bin/bash

################################################################################
#
# Project:  Mediated effects of foreclosures on coursetaking
# Purpose:  Download BEA data
# Author:   Patrick Lavallee Delgado
# Created:  9 July 2024
#
# Notes:    BEA API documentation: https://apps.bea.gov/API/docs/index.htm
#
# To do:
#
################################################################################

# Identify inputs and outputs.
PRG="$(pwd)/src/00-get-bea.py"
DTA="$(pwd)/in/bea.yaml"
KEY="$1"
OUT="$(pwd)/in/bea.csv"

# Get data.
python $PRG $DTA $KEY > $OUT
