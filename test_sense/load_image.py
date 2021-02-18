import numpy as np
import re
import cv2
#from cv2 import aruco

print(image)

# Enter the image height and width
height = int(len(image[0]))
width  = int(len(image[0][0]))

print(width)

# Create numpy array of BGR triplets
im = np.zeros((height,width,3), dtype=np.uint8)

for row in range (height):
    for col in range(width):
        hex = image[row, col][1:]
        R = int(hex[0:2],16)
        G = int(hex[2:4],16)
        B = int(hex[4:6],16)
        im[row,col] = (B,G,R)

print(im)
print(im.shape)
frame = im
#%%time
gray = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)
aruco_dict = aruco.Dictionary_get(aruco.DICT_6X6_250)
parameters = aruco.DetectorParameters_create()
corners, ids, rejectedImgPoints = aruco.detectMarkers(gray, aruco_dict, parameters = parameters)
#frame_markers = aruco.drawDetectedMarkers(frame.copy(), corners, ids)
print(corners)
# Save to disk
cv2.imwrite('result_python_.png', im)
