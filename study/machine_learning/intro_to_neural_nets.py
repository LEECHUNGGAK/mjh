# -*- coding: utf-8 -*-
"""
Created on Wed Oct 28 16:11:39 2020

@author: Administrator
"""

#%% Set Work Environment
from __future__ import print_function
import math
from IPython import display
from matplotlib import (cm, gridspec, pyplot as plt)
import numpy as np
import pandas as pd
from sklearn import metrics
import tensorflow as tf
from tensorflow.python.data import Dataset
import absl
from functools import partial

absl.logging.set_verbosity(absl.logging.ERROR)
pd.options.display.max_rows = 10
pd.options.display.float_format = "{:.1f}".format

california_housing_df = pd.read_csv("https://download.mlcc.google.com/mledu-datasets/california_housing_train.csv",
                                    sep=",")

california_housing_df = california_housing_df.reindex(np.random.permutation(california_housing_df.index))


#%% Functions
def preprocess_features(california_housing_df):
    """ Prepare input features from California housing data set.
    
    Args:
        california_housing_df: A Pandas DataFrame expected to contain data from
            the California housing data set.
    Return:
        A DataFrame that contains the features to be used for the model,
        including synthetic features.
    """
    
    selected_features = california_housing_df[["latitude",
                                              "longitude",
                                              "housing_median_age",
                                              "total_rooms",
                                              "total_bedrooms",
                                              "population",
                                              "households",
                                              "median_income"]]
    
    processed_features = selected_features.copy()
    
    # Create a synthetic feature.
    processed_features["rooms_per_person"] = (
        california_housing_df["total_rooms"] / california_housing_df["population"]
    )
    
    return processed_features

def preprocess_targets(california_housing_df):
    """ Prepares target features (i.e., labels) from California housing data set.
    
    Args:
        california_housing_df: A Pandas DataFrame expected to contain data from
            the California housing data set.
    Returns:
        A DataFrame that conatins the target feature.
    """
    
    output_targets = pd.DataFrame()
    
    # Create a boolean categorical feature representing whether the
    # median_house_value is above a set threshold.
    output_targets["median_house_value"] = (
        california_housing_df["median_house_value"] / 1000
    )
    
    return output_targets

def construct_feature_columns(input_features):
    """ Construct the TensorFlow Feature Columns.
    
    Args:
        input_features: The names of the numerical input features to use.
    Returns:
        A set of feature columns
    """
    
    return set([tf.feature_column.numeric_column(my_feature)
                for my_feature in input_features])

def my_input_fn(features, targets, batch_size = 1, shuffle = True,
                num_epochs = None):
    """ Train a neural net regression model.
    
    Args:
        features: pandas DataFrame of features
        targets: pandas DataFrame of targets
        batch_size: Size of batches to be passed to the model
        shuffle: True of False. Whether to shuffle the data.
        num_epochs: Number of epochs for which data should be repeated.
            None = repeat indefinitely
    Returns:
        Tuple of (features, labels) for next data batch
    """
    
    # Convert pandas data into a dict of np arrays.
    features = {key:np.array(value) for key, value in dict(features).items()}
    
    # Construct a dataset, and configure batching/repeating.
    ds = Dataset.from_tensor_slices((features, targets)) # warning: 2GB limit
    ds = ds.batch(batch_size).repeat(num_epochs)
    
    # Shuffle the data, if specified.
    if shuffle:
        ds = ds.shuffle(10000)
    
    # Return the next batch of data.
    features, labels = ds.make_one_shot_iterator().get_next()
    
    return features, labels

def train_nn_regression_model(
        learning_rate, steps, batch_size, hidden_units, training_examples,
        training_targets, validation_examples, validation_targets
        ):
    """ Trains a neural network regression model.
    
    In addition to training, this function also prints training progress
    information, as well as a plot of the trainig and validation loss over time.
    
    Args:
        learning_raet: A 'float', the learning rate.
        steps: A non-zero 'int', the total number of training steps. A training
            step consists of a forward and backward pass using a single batch.
        batch_size: A non-zero 'int', the batch_size.
        hidden_units: A 'list' of int values, specifying the number of neurons
            in each layer.
        training_examples: A 'DataFrame' containing one or more columns from
            'california_housing_df' to use as input features for training.
        training_targets: A 'DataFrame' containing exactly one column from
            'california_housing_df' to use as target for training.
        validation_examples: A 'DataFrame' containing exactly one or more
            columns from 'california_housing_df' to use as input features for
            validation.
        validation_targets: A 'DataFrame' containing exactly one column from
            'california_housing_df' to use as target for validation.
    
    Returns:
        A 'DNNRegressor' object trained on the training data.
    """
    
    periods = 10
    steps_per_period = steps / periods
    
    # Create a linear regressor object.
    my_optimizer = partial(tf.optimizers.Ftrl, learning_rate = learning_rate,
                           clipnorm = 5.0)
    dnn_regressor = tf.estimator.DNNRegressor(
        feature_columns = construct_feature_columns(training_examples),
        hidden_units = hidden_units,
        optimizer = my_optimizer
    )
    
    # Create input functions.
    training_input_fn = lambda: my_input_fn(training_examples,
                                            training_targets["median_house_value"],
                                            batch_size = batch_size)
    predict_training_input_fn = lambda: my_input_fn(training_examples,
                                                    training_targets["median_house_value"],
                                                    num_epochs = 1,
                                                    shuffle = False)
    predict_validation_input_fn = lambda: my_input_fn(validation_examples,
                                                      validation_targets["median_house_value"],
                                                      num_epochs = 1,
                                                      shuffle = False)
    
    # Train the model, but do so inside a loop so that we can periodically assess
    # loss metrics.
    print("Training model...")
    print("LogLoss (on traiing data):")
    training_rmse = []
    validation_rmse = []
    for period in range (0, periods):
        # Train the model, starting from the prior state.
        dnn_regressor.train(input_fn = training_input_fn,
                            steps = steps_per_period)
        
        # Take a break and compute  predictions.
        training_predictions = dnn_regressor.predict(input_fn = predict_training_input_fn)
        training_predictions = np.array([item["predictions"][0] 
                                           for item in training_predictions])
        
        validation_predictions = dnn_regressor.predict(input_fn = predict_validation_input_fn)
        validation_predictions = np.array([item["predictions"][0] 
                                             for item in validation_predictions])
        
        # Compute training and validation loss.
        training_root_mean_squared_error = metrics.root_mean_squared_error(
            training_targets, training_predictions
        )
        validation_root_mean_squared_error = metrics.root_mean_squared_error(
            validation_targets, validation_predictions
        )
        
        # Occasionally print the current loss.
        print("period %02d: %0.2f" % (period, training_root_mean_squared_error))
        # Add the loss metrics from this period to our list.
        training_rmse.append(training_root_mean_squared_error)
        validation_rmse.append(validation_root_mean_squared_error)
    print("Model training finished.")
    
    # Output a graph of loss metrics over periods.
    plt.ylabel("RMSE")
    plt.xlabel("Periods")
    plt.title("Root Mean Squared Error vs. Periods")
    plt.tight_layout()
    plt.plot(training_rmse, label = "training")
    plt.plot(validation_rmse, label = "validation")
    plt.legend()
    
    print("Fianl RMSE (on training data): % 0.2f" % training_root_mean_squared_error)
    print("Fianl RMSE (on validation data): % 0.2f" % validation_root_mean_squared_error)
    
    return dnn_regressor


#%% Preprocess Data
# Choose the first 12000 (out of 17000) examples for training.
training_examples = preprocess_features(california_housing_df.head(12000))
training_targets= preprocess_targets(california_housing_df.head(12000))

# Choose the last 5000 (out of 17000) examples for validation.
validation_examples = preprocess_features(california_housing_df.tail(5000))
validation_targets = preprocess_targets(california_housing_df.tail(5000))

# Double-check that we've done the right thing.
print("Training examples summary:")
display.display(training_examples.describe())
print("Validation examples summary:")
display.display(validation_examples.describe())

print("Training targets summary:")
display.display(training_targets.describe())
print("Validation targets summary:")
display.display(validation_targets.describe())


#%% Run
dnn_regressor = train_nn_regression_model(
    learning_rate = 0.01,
    steps = 500,
    batch_size = 10,
    hidden_units = [10, 2],
    training_examples = training_examples,
    training_targets = training_targets,
    validation_examples = validation_examples,
    validation_targets = validation_targets
)