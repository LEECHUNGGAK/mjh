# -*- coding: utf-8 -*-
"""
Created on Thu Jul 30 17:16:20 2020

@author: Administrator
"""


#%% Import Packages
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import (accuracy_score, confusion_matrix, precision_score,
                             recall_score, f1_score, roc_curve, precision_recall_curve)
from sklearn.ensemble import RandomForestClassifier
import matplotlib.pyplot as plt
import numpy as np


#%% Read Data
# dementia_dataframe_0 = pd.read_csv("C:/Users/Administrator/wd/alzheimer/data/man_data.csv",
#                                    sep = ",")

dementia_dataframe_0 = pd.read_csv("C:/Users/Administrator/wd/alzheimer/data/non_zero_coordinate_combination_data.csv",
                                 sep = ",")