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
import os

os.chdir("C:/Users/Administrator/wd/alzheimer")

#%% Read Data
# dementia_dataframe_0 = pd.read_csv("C:/Users/Administrator/wd/alzheimer/data/man_data.csv",
#                                    sep = ",")

dementia_dataframe_0 = pd.read_csv("C:/Users/Administrator/wd/alzheimer/data/non_zero_coordinate_combination_data.csv",
                                 sep = ",")

dementia_dataframe_1 = dementia_dataframe_0.drop(["Sample", "coordinate_count"], axis = 1)

features_dataframe = dementia_dataframe_1.drop("dementia", axis = 1)
labels_dataframe = dementia_dataframe_1["dementia"]

(training_features_dataframe, validation_features_dataframe,
 training_labels_dataframe, validation_labels_dataframe) = train_test_split(
    features_dataframe, labels_dataframe, test_size = 0.25, random_state = 28)


#%% Predict
def score_fn(validation_labels, prediction_labels, file_name):
    confusion_matrix_0 = confusion_matrix(validation_labels, prediction_labels)
    
    confusion_matrix_1 = confusion_matrix_0.astype(str)
    confusion_matrix_1 = np.insert(confusion_matrix_1,
                                       0,
                                       ["Predicted False", "Predicted True"],
                                       axis = 0)
    confusion_matrix_1 = np.insert(confusion_matrix_1,
                                   0,
                                   ["", "Actual False", "Actual True"],
                                   axis = 1)
    
    np.savetxt(file_name + "_Confusion_Matrix.csv",
               confusion_matrix_1,
               delimiter = ",",
               fmt = "%s")
    
    score_datafarme = pd.DataFrame({
        "Score":["Accuracy", "Precision", "Recall", "F1 Score",
                 "True Negatvie Rate", "Negatvie Predictive Value"], 
        "Value":[accuracy_score(validation_labels,prediction_labels), 
                 precision_score(validation_labels, prediction_labels),
                 recall_score(validation_labels, prediction_labels),
                 f1_score(validation_labels, prediction_labels),
                 confusion_matrix_0[0, 0] / sum(confusion_matrix_0[0, ]),
                 confusion_matrix_0[0, 0] / sum(confusion_matrix_0[:, 0])]})
    
    score_datafarme.to_csv(file_name + "_Score.csv", index = False)

def draw_plot_fn(validation_labels, prediction_labels, file_name):
    # Precision recall curve
    precisions, recalls, thresholds = precision_recall_curve(
        validation_labels, prediction_labels
        )
    threshold_boundary = thresholds.shape[0]
    
    plt.plot(thresholds, precisions[0:threshold_boundary], linestyle = "--",
         label = "Precision")
    plt.plot(thresholds, recalls[0:threshold_boundary], label = "Recall")
    plt.savefig(file_name + "_Precision_Recall_Curve.png")
    
    # ROC curve
    fprs, tprs, thresholds = roc_curve(validation_labels, prediction_labels)
    
    plt.plot(fprs, tprs, label = "ROC")
    plt.plot([0, 1], [0, 1], "k--", label = "Random")
    plt.savefig(file_name + "_ROC_Curve.png")

def prediction_fn(training_features, training_labels, validation_features,
                  validation_labels, file_path):
    # Logistic Regression
    lr_clf = LogisticRegression()
    
    lr_clf.fit(training_features, training_labels)
    
    lr_pred = lr_clf.predict(validation_features)
    
    score_fn(validation_labels, lr_pred, "Logistic_Regression")
    draw_plot_fn(validation_labels, lr_pred, "Logistic_Regression")
    
    # Coefficient
    coefficients = lr_clf.coef_
    coefficient_dataframe = pd.DataFrame({"coordinate":features_dataframe,
                                       "coefficient":coefficients.tolist()[0]})
    coefficient_dataframe.to_csv("Logistic_Regression_Coefficient.csv",
                             index = False)
    
    # Random Forest
    rf_clf = RandomForestClassifier(random_state = 28)
    
    rf_clf.fit(training_features, training_labels)

    rf_pred = rf_clf.predict(validation_features)
    
    score_fn(validation_labels, rf_pred, "Random Forest")
    draw_plot_fn(validation_labels, rf_pred, "Random Forest")
    
    # Feature importances
    rf_feature_importances = rf_clf.feature_importances_

    indices = np.argsort(rf_feature_importances)[::-1][:10]

    feature_importance_dataframe = pd.DataFrame({"coordinate":features_dataframe,
                                             "feature_importance":rf_feature_importances})
    feature_importance_dataframe.to_csv("C:/Users/Administrator/wd/alzheimer/Random_Forest_Feature_Importance.csv",
                             index = False)

    plt.title("Feature Importances")
    plt.bar(range(len(indices)), rf_feature_importances[indices])
    plt.xticks(range(len(indices)), [features_dataframe[i] for i in indices], rotation = -90)
    plt.savefig("Random_Forest_Feature_Importance_Plot.png")