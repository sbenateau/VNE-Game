# generate virtualenv
# python3 -m venv opencv
# . opencv/bin/activate
# pip install opencv-contrib-python

# initiate reticulate with the virtual environment
reticulate::use_virtualenv("opencv/", required = TRUE)
detection_of_codes <- function(image, show_image = FALSE) {

  library(reticulate)
  
  # write image with R
  source_python("read_and_detect_aruco.py")
  
  res <- detect(image)
  
  # get results to R
  if (!is.null(unlist(res[[1]]))){
    corners <- matrix(unlist(res[[1]]), ncol = 8, byrow = TRUE)
    top_left_x <- corners[, 1]
    top_right_x <- corners[, 2]
    bottom_right_x <- corners[, 3]
    bottom_left_x <- corners[, 4]
    top_left_y <- corners[, 5]
    top_right_y <- corners[, 6]
    bottom_right_y <- corners[, 7]
    bottom_left_y <- corners[, 8]
    
    results <- data.frame(py.ids = res[[2]], top_left_x, top_right_x, bottom_right_x, bottom_left_x, 
                          top_left_y, top_right_y, bottom_right_y, bottom_left_y)
    return(results)
  } else {
    print("no corner found")
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
  id = c(11, 13, 7, 16, 1, 9, 12, 14, 21, 3, 4, 5, 6, 30, 10, 17, 15, 31, 32, 33, 34, 35, 2, 8, 22, 36, 37),
  tool = c("DOis", ":A", "Env", ":G", "DVdt", ":N", ":E", "DSau", ":C", "Dep", "Cha", "Pra", "Boi", "Hum", ":V", "Ann", "Moi", "Lon", "Lat", "Lgr","Hab","Aca", "DEsc", ":T", "Reg", "Tau", "Dif")
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
