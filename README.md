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

+ Project summary: In this project, we created a classification engine for grayscale images of poodles versus images of fried chickens. For the feature extraction methods, in addition to the sift feature provided in class, we tried three new methods: HOG, ORB, and EigenFace; for the classification models, we have trained seven classifiers: GBM, Random Forest, SVM with linear kernel, SVM with RBF kernel, xgboost, and Deep Boost. We trained each classification model, implemented on training data (which is randomly sampled from the data provided by professor) and then compute the cross validation error rate for each trained model on test data. Based on the error rate, we have chose the model with HOG and SVM with Linear Kernel to be our best model, because the test error rate is the lowest among all the other combinations of feature and classifications.    
	
**Contribution statement**: ([default](doc/a_note_on_contributions.md)) All team members contributed equally in all stages of this project. Specifically, Yue Gao contributed to EigenFace (feature extraction); Xiaowo Sun were in charge of OGB (feature extractions); Ruochen Liu contributed to HOG (feature.r)and Xuehan Liu contributed to  All team members approve our work presented in this GitHub repository including this contributions statement. 

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
