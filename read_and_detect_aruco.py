import numpy as np
import cv2
from cv2 import aruco

def to_rgb(v):
    return np.array([int(v[1:3],16), int(v[3:5],16) , int(v[5:7],16)])
    
image_test = image.flatten().reshape(640, 360, order='F')
image_cv = np.array([to_rgb(h) for h in image_test.flatten()]).reshape(360, 640, 3).astype(np.uint8)

gray = cv2.cvtColor(image_cv, cv2.COLOR_BGR2GRAY)
aruco_dict = aruco.Dictionary_get(aruco.DICT_6X6_250)
parameters =  aruco.DetectorParameters_create()
corners, ids, rejectedImgPoints = aruco.detectMarkers(gray, aruco_dict, parameters=parameters)

cv2.imwrite('result_fin.png', image_cv)
