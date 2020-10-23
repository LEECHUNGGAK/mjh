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
                             recall_score, f1_score, roc_curve, precision_recall_curve,
                             auc)
from sklearn.ensemble import (RandomForestClassifier, GradientBoostingClassifier)
from sklearn.neighbors import KNeighborsClassifier
import matplotlib.pyplot as plt
import numpy as np
import os

path = "C:/Users/Administrator/wd/alzheimer/output/top3b_201023"

if not os.path.isdir(path):
    os.mkdir(path)
    
os.chdir(path)


#%% Functions
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
                 "True Negatvie Rate", "Negative Predictive Value"], 
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
    plt.legend(loc = "lower right")
    plt.savefig(file_name + "_Precision_Recall_Curve.png")
    plt.close()
    
    # ROC curve
    fprs, tprs, thresholds = roc_curve(validation_labels, prediction_labels)
    roc_auc = auc(fprs, tprs)
    
    plt.plot(fprs, tprs, label = "ROC; AUC = %0.4f" % roc_auc)
    plt.plot([0, 1], [0, 1], "k--", label = "Random")
    plt.legend(loc = "lower right")
    plt.ylabel("True Positive Rate")
    plt.xlabel("False Positive Rate")
    plt.savefig(file_name + "_ROC_Curve.png")
    plt.close()

def prediction_fn(features, labels):
    features_name = features.columns
    
    # Train test split
    (training_features, validation_features,
     training_labels, validation_labels) = train_test_split(
    features, labels, test_size = 0.25, random_state = 28
    )
     
    # Logistic Regression
    lr_clf = LogisticRegression()
    
    lr_clf.fit(training_features, training_labels)
    
    lr_pred = lr_clf.predict(validation_features)
    
    score_fn(validation_labels, lr_pred, "Logistic_Regression")
    draw_plot_fn(validation_labels, lr_pred, "Logistic_Regression")
    
    # Coefficient
    coefficients = lr_clf.coef_
    coefficient_dataframe = pd.DataFrame({"coordinate":features_name,
                                       "coefficient":coefficients.tolist()[0]})
    coefficient_dataframe.to_csv("Logistic_Regression_Coefficient.csv",
                             index = False)
    
    # Random Forest
    rf_clf = RandomForestClassifier(random_state = 28)
    
    rf_clf.fit(training_features, training_labels)

    rf_pred = rf_clf.predict(validation_features)
    
    score_fn(validation_labels, rf_pred, "Random_Forest")
    draw_plot_fn(validation_labels, rf_pred, "Random_Forest")
    
    # Feature importances
    rf_feature_importances = rf_clf.feature_importances_

    indices = np.argsort(rf_feature_importances)[::-1][:10]

    feature_importance_dataframe = pd.DataFrame({"coordinate":features_name,
                                             "feature_importance":rf_feature_importances})
    feature_importance_dataframe.to_csv("Random_Forest_Feature_Importance.csv",
                             index = False)

    plt.title("Feature Importances")
    plt.bar(range(len(indices)), rf_feature_importances[indices])
    plt.xticks(range(len(indices)), [features_name[i] for i in indices], rotation = -90)
    plt.tight_layout()
    plt.savefig("Random_Forest_Feature_Importance_Plot.png", dpi = 300,
                )
    
    # K NeighborsClassifier
    kn_clf = KNeighborsClassifier()
    
    kn_clf.fit(training_features, training_labels)
    
    kn_pred = kn_clf.predict(validation_features)
    
    score_fn(validation_labels, kn_pred, "K_Neighbors")
    draw_plot_fn(validation_labels, kn_pred, "K_Neighbors")
    
    # Gradient Boosting
    gb_clf = GradientBoostingClassifier()
    
    gb_clf.fit(training_features, training_labels)
    
    gb_pred = gb_clf.predict(validation_features)
    
    score_fn(validation_labels, gb_pred, "Gradient_Boosting")
    draw_plot_fn(validation_labels, gb_pred, "Gradient_Boosting")


#%% TOP3B Predict
top3b_dataframe_0 = pd.read_csv("C:/Users/Administrator/wd/alzheimer/data/ngs/combn_top3b_v2.csv",
                                 sep = ",")

top3b_dataframe_1 = top3b_dataframe_0.drop(["id"], axis = 1)

features_dataframe = top3b_dataframe_1.drop("dementia", axis = 1)
labels_dataframe = top3b_dataframe_1["dementia"]

prediction_fn(features_dataframe, labels_dataframe)


#%% APOE Predict
apoe_dataframe_0 = pd.read_csv("C:/Users/Administrator/wd/alzheimer/data/apoe_data_t2.csv", 
                               sep = ",")

apoe_dataframe_1 = apoe_dataframe_0.drop("id", axis = 1)

features_dataframe = apoe_dataframe_1.drop("dementia", axis = 1)
labels_dataframe = apoe_dataframe_1["dementia"]

prediction_fn(features_dataframe, labels_dataframe)