#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jul 12 16:55:53 2018

@author: catherineschluth
"""
import pandas as pd
helminths = pd.read_excel("ascaris_lumbricoides_raw_data.xlsx", sheetname = "ascaris_lumbricoides_raw_data")
print(helminths.head(506))