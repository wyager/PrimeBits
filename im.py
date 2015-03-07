import numpy as np
import cv2

image = np.reshape(np.array(eval(open("bits.txt").read()))*255, (1000,1000)).astype(np.uint8)
cv2.imwrite("out.png", image)