import numpy as np
import csv
import cv2
from os import listdir

# dir for training images
path1 = '/Users/Prayerest_/Github/spr2017-proj3-group7-master/data/raw_images/'
# dir for burred images
path2 = '/Users/Prayerest_/Github/spr2017-proj3-group7-master/data/burred_images/'


image_lists = listdir(path1)
len(image_lists)

image_dir_list = []
burred_dir_list = []
for i in range(len(image_lists)-1):
    image_dir_list.append(path1+image_lists[i+1])
    burred_dir_list.append(path2+image_lists[i+1])


# get burred images
for i in range(len(image_dir_list)):
    print(i)
    img = cv2.imread(image_dir_list[i])
    median = cv2.medianBlur(img,7)
    cv2.imwrite(burred_dir_list[i],median)
    

# ORB
num = 0
for i in range(len(burred_dir_list)):
    print(burred_dir_list[i])
    image = cv2.imread(burred_dir_list[i])
    gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY) 
    orb = cv2.ORB_create(nfeatures=50)

    # find the keypoints
    kp = orb.detect(gray,None)
    # compute the descriptors
    kp, des = orb.compute(gray, kp)
    
    label = np.array([i+1])
    label = np.repeat(label, len(kp))
    label = label.reshape(len(kp),1)
    if len(kp) == 0:
        continue
    
    if i == 0:
        feature_matrix = np.concatenate((des, label), 1)
        
    else:
        des_label = np.concatenate((des, label), 1)
        feature_matrix = np.concatenate((feature_matrix, des_label), axis=0)

    num += len(des)
    print(num)
    
feature_list = feature_matrix.tolist()

with open("/Users/Prayerest_/Github/spr2017-proj3-group7-master/data/orb_feature.csv", "w") as f:
    writer = csv.writer(f, delimiter =',')
    writer.writerows(feature_list)
