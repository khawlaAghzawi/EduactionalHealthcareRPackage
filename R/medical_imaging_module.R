#' Medical Imaging Module
#'
#' This module covers techniques for medical imaging analysis, including image
#' segmentation, object detection, and radiological image interpretation.
#'
#' @author Khawla Aghzawi
#' @keywords healthcare data medical imaging analysis
#'

#' Load Medical Imaging Data
#'
#' This function reads a medical imaging dataset in PNG format from the
#' specified file path.
#'
#' @param path A character string specifying the file path to the medical
#' imaging dataset in PNG format.
#' @return A 3D array representing the medical imaging data. Each layer
#' corresponds to a color channel.
#'
#' @examples medical_data <- load_medical_imaging_data("path/to/your/image.png")
#' @export
#'
load_medical_imaging_data <- function(path) {

  medical_imaging_data <- png::readPNG(path)
  return(medical_imaging_data)
}

#' Preprocess Medical Imaging Data
#'
#' This function performs preprocessing on medical imaging data.
#'
#' @param medical_imaging_data A 2D or 3D array representing medical imaging data.
#' @return Preprocessed medical imaging data.
#' @export
#'
preprocess_medical_imaging_data <- function(medical_imaging_data) {

  # Placeholder for preprocessing steps
  message("Preprocessing medical imaging data...")

  # Check if the image is a 3D array
  if (is.array(medical_imaging_data) && length(dim(medical_imaging_data)) == 3) {
    # Convert the 3D array to grayscale
    preprocessed_data <- apply(medical_imaging_data, c(2, 3), mean)
  } else if (is.array(medical_imaging_data) && length(dim(medical_imaging_data)) == 2) {
    # Image is already grayscale or 2D
    preprocessed_data <- medical_imaging_data
  } else {
    stop("Invalid input: The input should be a 2D or 3D array.")
  }

  # Additional preprocessing steps
  # Normalize or scale pixel values if necessary
  preprocessed_data <- (preprocessed_data - min(preprocessed_data)) / (max(preprocessed_data) - min(preprocessed_data))

  # Handle any missing or abnormal values (replace missing values with appropriate values)
  preprocessed_data[is.na(preprocessed_data)] <- 0

  # Apply image processing techniques (smoothing)
  # Example: preprocessed_data <- imager::smooth(preprocessed_data, sigma = 2)

  # Return the preprocessed data
  return(preprocessed_data)
}


#' Perform Image Segmentation
#'
#' This function performs image segmentation on medical imaging data.
#'
#' @param image An image for segmentation.
#' @return A segmented image.
#' @export
#'
perform_image_segmentation <- function(image) {
  # Placeholder for image segmentation code
  # Replace this with your actual image segmentation code

  # Example: Simple thresholding for demonstration purposes
  threshold_value <- 0.5
  segmented_image <- imager::threshold(image, threshold_value)

  message("Performing image segmentation...")

  # Return the segmented image
  return(segmented_image)
}

#' Perform Object Detection on Breast Ultrasound Images
#'
#' This function facilitates object detection on breast ultrasound images for the task
#' of identifying tumors. It leverages the \code{imager} package for image processing
#' and \code{keras} for deep learning model creation.
#'
#' @param dataset_path Path to the directory containing breast ultrasound images.
#' @param dimensions Dimensions to which the images will be resized (default is c(256, 256)).
#' @param test_split Fraction of images for testing (default is 0.2).
#' @param validation_split Fraction of images for validation (default is 0.1).
#' @param epochs Number of training epochs for the model (default is 20).
#' @param batch_size Batch size used during model training (default is 8).
#' @param threshold Probability threshold for image segmentation (default is 0.8).
#'
#' @return A list containing the results of object detection on test images.
#'
#' @examples
#' dataset_path <- "C:/Users/User/Downloads/Dataset_BUSI_with_GT"
#' detection_results <- perform_object_detection(dataset_path, 20)
#'
#' @seealso
#' \code{\link{imager}} package for image processing.
#' \code{\link{keras}} package for deep learning model creation.
#'
#' @export
#'
perform_object_detection <- function(dataset_path, subset_size = NULL, dimensions = c(256, 256), test_split = 0.2, validation_split = 0.1, epochs = 20, batch_size = 8, threshold = 0.8) {

  all_files <- list.files(dataset_path, full.names = TRUE, recursive = TRUE)

  # Take a subset of files for demonstration
  if (!is.null(subset_size)) {
    all_files <- sample(all_files, size = subset_size)
    }

  img <- vector("list", length = length(all_files))

  for (i in seq_along(all_files)) {
    img[i] <- imager::load.image(all_files[i])
    if (i %% batch_size == 0) {
      message("Loaded ", i, " images out of ", length(all_files))
      }
  }

  # Remove images with multiple masks
  lf <- list.files(dataset_path, full.names = TRUE, recursive = TRUE)
  which_extra_mask_1 <- grep("mask_1", lf)
  to_remove <- c(sort(c(which_extra_mask_1, which_extra_mask_1 - 1, which_extra_mask_1 - 2)), grep("mask_2", lf))
  lf_subset <- lf[-to_remove]

  # Load images and masks
  img <- purrr::map(lf_subset, ~ imager::load.image(.))
  mask <- purrr::map(lf_subset, ~ imager::load.image(.))

  # Split the data
  split_tmp <- split_data(n = nrow(img$info), frac_test = test_split, frac_val = validation_split, seed = 123)
  test_index <- split_tmp$index_test
  val_index <- split_tmp$index_val
  train_index <- split_tmp$index_train

  # Convert images and masks to 4D arrays for keras
  x_train <- imagesToKerasInput(img, type = "image", grayscale = TRUE, subset = train_index)
  y_train <- imagesToKerasInput(images = mask, type = "mask", subset = train_index)
  x_test <- imagesToKerasInput(img, type = "image", grayscale = TRUE, subset = test_index)
  y_test <- imagesToKerasInput(images = mask, type = "mask", subset = test_index)

  x_val <- imagesToKerasInput(img, type = "image", grayscale = TRUE, subset = val_index)
  y_val <- imagesToKerasInput(images = mask, type = "mask", subset = val_index)

  # Create the model architecture
  model <- u_net(net_w = dimensions[1], net_h = dimensions[2], grayscale = TRUE, n_class = 1, filters = 32)

  # Model compilation
  model %>% compile(optimizer = optimizer_adam(), loss = loss_binary_crossentropy, metrics = list(metric_binary_accuracy))

  # Set up the sampling generator
  sampling_generator <- function(x_data, y_data, batch_size) {
    function() {
        rows <- sample(1:nrow(x_data), batch_size, replace = TRUE)
        list(x_data[rows,,,, drop = FALSE], y_data[rows,,,, drop = FALSE])
      }
      }

  # Train the model
  history <- model %>% fit(x = sampling_generator(x_train, y_train, batch_size = batch_size),
                             epochs = epochs,
                             steps_per_epoch = round(nrow(x_train) / batch_size),
                             validation_data = list(x_val, y_val))

  # Model evaluation
  scores <- model %>% evaluate(x_test, y_test, verbose = 0)
  print(scores)

  # Perform image segmentation on test images
  out <- imageSegmentation(model, x = x_test, threshold = threshold)

  # Return the results or do further processing as needed
  return(out)
}

#' Interpret Radiological Images
#'
#' This function provides guidance on interpreting radiological images.
#'
#' @param radiological_image A radiological image for interpretation.
#' @export
#'
interpret_radiological_image <- function(radiological_image) {
  # Placeholder for radiological image interpretation code
  # Replace this with your actual interpretation code
  message("Interpreting radiological images...")
  # Example: your_interpretation_function(radiological_image)

  # Additional guidance or educational content can be included
}

