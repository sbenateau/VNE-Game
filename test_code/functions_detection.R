# generate virtualenv
# python3 -m venv opencv
# . opencv/bin/activate
# pip install opencv-contrib-python

# initiate reticulate with the virtual environment
reticulate::use_virtualenv("opencv/", required = TRUE)
detection_of_codes <- function(image_path, show_image = FALSE) {

  library(reticulate)
  # run python code to identify the "codes"
  py_run_string("import cv2")
  py_run_string("from cv2 import aruco")
  
  py$path_to_frame <- image_path
  
  py_run_string("frame = cv2.imread(path_to_frame)")
  #py_run_string("%%time")
  py_run_string("gray = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)")
  py_run_string("aruco_dict = aruco.Dictionary_get(aruco.DICT_6X6_250)")
  py_run_string("parameters =  aruco.DetectorParameters_create()")
  py_run_string("corners, ids, rejectedImgPoints = aruco.detectMarkers(gray, aruco_dict, parameters=parameters)")
  # py_run_string("frame_markers = aruco.drawDetectedMarkers(frame.copy(), corners, ids)")
  
  # get results to R
  if (!is.null(unlist(py$corners))){
    corners <- matrix(unlist(py$corners), ncol = 8, byrow = TRUE)
    top_left_x <- corners[, 1]
    top_right_x <- corners[, 2]
    bottom_right_x <- corners[, 3]
    bottom_left_x <- corners[, 4]
    top_left_y <- corners[, 5]
    top_right_y <- corners[, 6]
    bottom_right_y <- corners[, 7]
    bottom_left_y <- corners[, 8]
    
    results <- data.frame(py$ids, top_left_x, top_right_x, bottom_right_x, bottom_left_x, 
                          top_left_y, top_right_y, bottom_right_y, bottom_left_y)
    results
    #points(rejectedImgPoints[,1], rejectedImgPoints[,5])
    if (show_image == TRUE){
      library(imager)
      
      frame <- load.image(image_path)
      par(mar = c(0,0,0,0))
      plot(frame)
      
      text(labels = results$py.ids, 
           rowMeans(cbind(results$top_left_x, results$top_right_x, results$bottom_left_x, results$bottom_right_x)), 
           rowMeans(cbind(results$top_left_y, results$top_right_y, results$bottom_left_y, results$bottom_right_y)),
           col = 2, font = 2, cex = 1.5)
    }
    return(results)
  } else {
    print("no corner found")
    if (show_image == TRUE){
      library(imager)
      frame <- load.image(image_path)
      par(mar = c(0,0,0,0))
      plot(frame)
    }
    results = NULL
    return(results)
    
  }
}

test_orientation <- function (results) {
  
  adjacent = results$top_left_x - results$bottom_left_x
  oppose = results$top_left_y - results$bottom_left_y
  atan(oppose/adjacent) * 180 / pi
  
  adjacent = results$top_left_x - results$top_right_x
  oppose = results$top_left_y - results$top_right_y
  atan(oppose/adjacent) * 180 / pi
  
}

equivalence_code <- data.frame(    
  id = c(11, 13, 7, 16, 1, 9, 12, 14, 21, 3, 4, 5, 6, 30, 10, 17, 15, 31, 32, 33, 34, 35, 2, 8, 22),
  tool = c("DOis", ":A", "Env", ":G", "DVdt", ":N", ":E", "DSau", ":C", "Dep", "Cha", "Pra", "Boi", "Hum", ":V", "Ann", "Moi", "Lon", "Lat", "Lgr","Hab","Aca", "DEsc", ":T", "Reg")
)

coordinates_to_code <- function(results_from_identification){
  if (!is.null(results_from_identification)){
  results_from_identification_ordered <- results_from_identification[order(results_from_identification$top_left_x), ]
  tools <- dplyr::left_join(results_from_identification_ordered, equivalence_code, by = c("py.ids" = "id"))
  code <- paste(tools$tool, collapse = "")
  } else {
    code = ""
  }
  print(code)
  return(code)
}
