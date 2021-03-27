# The Algorithms - R

![build](https://img.shields.io/badge/build-passing-brightgreen.svg)
![license](https://img.shields.io/badge/License-MIT-brightgreen.svg)
![prs](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)

*R is a programming language and free software environment for statistical computing and graphics supported by the R Foundation for Statistical Computing. The R language is widely used among statisticians and data miners for developing statistical software and data analysis. Polls, data mining surveys and studies of scholarly literature databases show substantial increases in popularity in recent years. As of November 2019, R ranks 16th in the TIOBE index, a measure of popularity of programming languages. ([Wikipedia](https://en.wikipedia.org/wiki/R_(programming_language)))*

## General Algorithms List
Here are some common algorithms that can be applied to almost all data problems:

### PREPROCESSING
- Feature projection
    - [Dimensionality Reduction Algorithm](https://github.com/TheAlgorithms/R/blob/master/preprocessing/dimensionality_reduction_algorithms.R)
- Data manipulattion
    - [Data Processing](https://github.com/TheAlgorithms/R/blob/master/preprocessing/data_processing.R)
    - [Data normalization and standardization](https://github.com/TheAlgorithms/R/blob/master/preprocessing/data_normalization_standardization.R)
    - Categorical variable into numerical
      - [One Hot Encoding](https://github.com/TheAlgorithms/R/blob/master/datamanipulation/OneHotEncode.R)
      - [Label Encoding](https://github.com/TheAlgorithms/R/blob/master/datamanipulation/LabelEncode.R)
      
### [MACHINE LEARNING](https://github.com/TheAlgorithms/R/tree/master/Machine-Learning)
- Tutorials
    - [Introduction to machine learning in R (tutorial) --- from Kaggle](https://www.kaggle.com/camnugent/introduction-to-machine-learning-in-r-tutorial)
    - [An Introduction to Machine Learning with R](https://lgatto.github.io/IntroMachineLearningWithR/)
    - [Machine Learning in R for beginners](https://www.datacamp.com/community/tutorials/machine-learning-in-r)
    - [Machine Learning in R: mlr-tutorial](https://www.notion.so/mlr-Tutorial-b71444fe979c4a8cafe91e10e7f81d79)

### [DATA MINING](https://github.com/TheAlgorithms/R/tree/master/Data-Mining)

### SUPERVISED LEARNING
  - Classification
    - [Decision Tree](https://github.com/TheAlgorithms/R/blob/master/classification/decision_tree.R)
    - [Gradient Boosting Algorithm](https://github.com/TheAlgorithms/R/blob/master/classification/gradient_boosting_algorithms.R)
    - [KNN](https://github.com/TheAlgorithms/R/blob/master/classification/KNN.R)
    - [LightGBM](https://github.com/TheAlgorithms/R/blob/master/classification/LightGBM.R)
    - [Logistic Regression](https://github.com/TheAlgorithms/R/blob/master/classification/logistic_regression.R)
    - [Naive Bayes](https://github.com/TheAlgorithms/R/blob/master/classification/naive_bayes.R)
    - [Random Forest](https://github.com/TheAlgorithms/R/blob/master/classification/random_forest.R)
    - [SVM](https://github.com/TheAlgorithms/R/blob/master/classification/SVM.R)
    - [XGBoost](https://github.com/TheAlgorithms/R/blob/master/classification/xgboost.R)
    - [Lasso](https://github.com/TheAlgorithms/R/blob/master/classification/lasso.R)
  - Regression
    - [Gradient Boosting Algorithm](https://github.com/TheAlgorithms/R/blob/master/regression/gradient_boosting_algorithms.R)
    - [KNN](https://github.com/TheAlgorithms/R/blob/master/regression/KNN.R)
    - [LightGBM](https://github.com/TheAlgorithms/R/blob/master/regression/LightGBM.R)
    - [Linear Regression](https://github.com/TheAlgorithms/R/blob/master/regression/linear_regression.R)
    - [Artificial Neural Net](https://github.com/TheAlgorithms/R/blob/master/regression/ANN.R)
    
### UNSUPERVISED LEARNING
  - Clustering
    - [K-Means](https://github.com/TheAlgorithms/R/blob/master/clustering/K-Means.R)
    - [DbScan Clustering](https://github.com/TheAlgorithms/R/blob/master/clustering/dbscan_clustering.R)
    - [Heirarchical Clustering](https://github.com/TheAlgorithms/R/blob/master/clustering/heirarchical_clustering.R)
    - [K-Means Clustering](https://github.com/TheAlgorithms/R/blob/master/clustering/kmeans_clustering.R)

### SORTING
  - [Bubble Sort](https://github.com/TheAlgorithms/R/blob/master/sorting/Bubble%20sort.R)
  - [Comb Sort](https://github.com/TheAlgorithms/R/blob/master/sorting/Comb%20sort.R)
  - [Insertion Sort](https://github.com/TheAlgorithms/R/blob/master/sorting/Insertion%20sort.R)
  - [Quick Sort](https://github.com/TheAlgorithms/R/blob/master/sorting/Quick%20sort.R)  
  - [Selection Sort](https://github.com/TheAlgorithms/R/blob/master/sorting/Selection%20sort.R)
  - [Stooge Sort](https://github.com/TheAlgorithms/R/blob/master/sorting/Stooge%20sort.R)
  - [Merge Sort](https://github.com/TheAlgorithms/R/blob/master/sorting/Merge%20sort.R)
  - [Radix Sort](https://github.com/TheAlgorithms/R/blob/master/sorting/Radix%20sort.R)
  - [Heap Sort](https://github.com/TheAlgorithms/R/blob/master/sorting/Heap%20sort.R)

## Contribution Guidelines
Please ensure to follow the points stated below if you would like to contribute:
- If your proposing a new algorithm or making changes to an existing one, make sure your code works. Reviewers or the general user must be able to directly emplace it in an R environment and get the desired output.
- Add an example to showcase the use of an algorithm proposed. It can be commented.
- Follow proper naming convention for variables (use `.` or `_` to seperate terms, such as `results.df` for a data frame containing some results) and filenames (follow the convention that has been followed for files under the directory your committing to).
- Feel free to add links here to the newly added file(s), but ensure that they do not result in a merge conflict with different versions of this readme under previous pull requests. 
