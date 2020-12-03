# Imports
import tensorflow as tf
import numpy as np
import matplotlib.pyplot as plt
import tensorflow_hub as hub
import tensorflow_datasets as tfds
from tensorflow.keras import layers
import logging

logger = tf.get_logger()
logger.setLevel(logging.ERROR)

# Download the Flowers Datasets using TensorFlow Datasets
(training_set, validation_set), dataset_info = tfds.load(
    "tf_flowers",
    split=["train[:70%]", "train[70%:]"],
    with_info=True,
    as_supervised=True
)

# Print Information about the Flowers Dataset
num_classes = dataset_info.features["label"].num_classes

num_training_examples = 0
for example in training_set:
    num_training_examples += 1

num_validation_examples = 0
for example in validation_set:
    num_validation_examples += 1

print("Total Number of Classes: {}".format(num_classes))
print("Total Number of Training Images: {}".format(num_training_examples))
print("Total Number of Validation Images: {}".format(num_validation_examples))

for i, example in enumerate(training_set.take(5)):
    print("Image {} shape: {} label: {}".format(i + 1, example[0].shape, example[1]))

# Reformat Images and Create Batches
IMAGE_RES = 299


def format_image(image, label):
    image = tf.image.resize(image, (IMAGE_RES, IMAGE_RES)) / 255
    return image, label


BATCH_SIZE = 32

train_batches = training_set.shuffle(num_training_examples // 4).map(format_image).batch(BATCH_SIZE).prefetch(1)
validation_batches = validation_set.map(format_image).batch(BATCH_SIZE).prefetch(1)

# Transfer Learning with TensorFlow Hub
URL = "https://tfhub.dev/google/tf2-preview/inception_v3/feature_vector/4"
feature_extractor = hub.KerasLayer(URL, input_shape=(IMAGE_RES, IMAGE_RES, 3))

# Freeze the Pre-Trained Model
feature_extractor.trainable = False

# Attach a classificaiton head
model = tf.keras.Sequential([
    feature_extractor,
    layers.Dense(num_classes, activation="softmax")
])

model.summary()

# Train the model
model.compile(
    optimizer=tf.optimizers.Adam(0.1),
    loss=tf.keras.losses.SparseCategoricalCrossentropy(from_logits=True),
    metrics=["accuracy"]
)

EPOCHS = 6

history = model.fit(train_batches, epochs=EPOCHS, validation_data=validation_batches)

# Plot Training and Validation Graphs
acc = history.history["accuracy"]
val_acc = history