# Project: Labradoodle or Fried Chicken? In Blakc and White. 
![image](figs/poodleKFC.jpg)

### [Full Project Description](doc/project3_desc.html)

Term: Spring 2017

+ Team #7
+ Team members
  + Nikita Oleg Bondarenko
	+ Yue Gao
	+ Ruochen Liu
	+ Xuehan Liu
	+ Xiaowo Sun

+ Project summary: In this project, we created a classification engine for grayscale images of poodles versus images of fried chickens. For the feature extraction methods, in addition to the sift feature provided in class, we tried three new methods: HOG, ORB, and EigenFace; for the classification models, we have trained seven classifiers: GBM, Random Forest, SVM with linear kernel, SVM with RBF kernel, and xgboost. We trained each classification model, implemented on training data (which is randomly sampled from the data provided by professor) and then compute the cross validation error rate for each trained model on test data. Based on the test error rate, we have chose the model with HOG and SVM with Linear Kernel to be our best model, because this model gives us the most consistent training error and test error. All the other models are somehow suffering from the overfitting problem. Lastly, compared to the baseline model with test error rate of 0.25, the test error rate for our best model (HOG+SVM with linear kernel) is 0.122, which is improved by (0.25-0.122)/0.122=51.2%.      
	
**Contribution statement**: ([default](doc/a_note_on_contributions.md)) All team members contributed equally in all stages of this project. Specifically, Nikita Bondarenko were in charge of presentation of all our work on this project; Yue Gao contributed to EigenFace and clean up the file feature.r; Xiaowo Sun were in charge of OGB (feature extractions); Ruochen Liu contributed to HOG and training classifiers; and Xuehan Liu contributed to training classifiers and compute test errors. All team members approve our work presented in this GitHub repository including this contributions statement. 

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
