# The Algorithms - R

![build](https://img.shields.io/badge/build-passing-brightgreen.svg)
![license](https://img.shields.io/badge/License-MIT-brightgreen.svg)
![prs](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)

*R is a programming language and free software environment for statistical computing and graphics supported by the R Foundation for Statistical Computing. The R language is widely used among statisticians and data miners for developing statistical software and data analysis. Polls, data mining surveys and studies of scholarly literature databases show substantial increases in popularity in recent years. As of November 2019, R ranks 16th in the TIOBE index, a measure of popularity of programming languages. ([Wikipedia](https://en.wikipedia.org/wiki/R_(programming_language)))*

## General Algorithms List
Here are some common algorithms that can be applied to almost all data problems:

### PREPROCESSING
- Feature projection
    - [Dimensionality Reduction Algorithm](https://github.com/TheAlgorithms/R/blob/master/Data-Preprocessing/dimensionality_reduction_algorithms.R)
- Data manipulation
    - [Data Processing](https://github.com/TheAlgorithms/R/blob/master/Data-Preprocessing/data_processing.R)
    - [Data normalization and standardization](https://github.com/TheAlgorithms/R/blob/master/Data-Preprocessing/data_normalization_standardization.R)
    - Categorical variable into numerical
      - [One Hot Encoding](https://github.com/TheAlgorithms/R/blob/master/Data-Manipulation/OneHotEncode.R)
      - [Label Encoding](https://github.com/TheAlgorithms/R/blob/master/Data-Manipulation/LabelEncode.R)

### [MACHINE LEARNING](https://github.com/TheAlgorithms/R/tree/master/Machine-Learning)
- Tutorials
    - [Introduction to machine learning in R (tutorial) --- from Kaggle](https://www.kaggle.com/camnugent/introduction-to-machine-learning-in-r-tutorial)
    - [An Introduction to Machine Learning with R](https://lgatto.github.io/IntroMachineLearningWithR/)
    - [Machine Learning in R for beginners](https://www.datacamp.com/community/tutorials/machine-learning-in-r)
    - [Machine Learning in R: mlr-tutorial](https://www.notion.so/mlr-Tutorial-b71444fe979c4a8cafe91e10e7f81d79)

### [DATA MINING](https://github.com/TheAlgorithms/R/tree/master/Data-Mining)

### SUPERVISED LEARNING
  - Classification
    - [Decision Tree](https://github.com/TheAlgorithms/R/blob/master/Classification-Algorithms/decision_tree.R)
    - [Gradient Boosting Algorithm](https://github.com/TheAlgorithms/R/blob/master/Classification-Algorithms/gradient_boosting_algorithms.R)
    - [KNN](https://github.com/TheAlgorithms/R/blob/master/Classification-Algorithms/KNN.R)
    - [LightGBM](https://github.com/TheAlgorithms/R/blob/master/Classification-Algorithms/LightGBM.R)
    - [Logistic Regression](https://github.com/TheAlgorithms/R/blob/master/Classification-Algorithms/logistic_regression.R)
    - [Naive Bayes](https://github.com/TheAlgorithms/R/blob/master/Classification-Algorithms/naive_bayes.R)
    - [Random Forest](https://github.com/TheAlgorithms/R/blob/master/Classification-Algorithms/random_forest.R)
    - [SVM](https://github.com/TheAlgorithms/R/blob/master/Classification-Algorithms/SVM.R)
    - [XGBoost](https://github.com/TheAlgorithms/R/blob/master/Classification-Algorithms/xgboost.R)
    - [Lasso](https://github.com/TheAlgorithms/R/blob/master/Classification-Algorithms/lasso.R)
  - Regression
    - [Gradient Boosting Algorithm](https://github.com/TheAlgorithms/R/blob/master/Regression-Algorithms/gradient_boosting_algorithms.R)
    - [KNN](https://github.com/TheAlgorithms/R/blob/master/Regression-Algorithms/KNN.R)
    - [LightGBM](https://github.com/TheAlgorithms/R/blob/master/Regression-Algorithms/LightGBM.R)
    - [Linear Regression](https://github.com/TheAlgorithms/R/blob/master/Regression-Algorithms/linear_regression.R)
    - [Artificial Neural Net](https://github.com/TheAlgorithms/R/blob/master/Regression-Algorithms/ANN.R)

### UNSUPERVISED LEARNING
  - Clustering
    - [K-Means](https://github.com/TheAlgorithms/R/blob/master/Clustering-Algorithms/K-Means.R)
    - [DbScan Clustering](https://github.com/TheAlgorithms/R/blob/master/Clustering-Algorithms/dbscan_clustering.R)
    - [Heirarchical Clustering](https://github.com/TheAlgorithms/R/blob/master/Clustering-Algorithms/heirarchical_clustering.R)
    - [K-Means Clustering](https://github.com/TheAlgorithms/R/blob/master/Clustering-Algorithms/kmeans_clustering.R)

### SORTING
  - [Bubble Sort](https://github.com/TheAlgorithms/R/blob/master/Sorting-Algorithms/bubble_sort.R)
  - [Comb Sort](https://github.com/TheAlgorithms/R/blob/master/Sorting-Algorithms/comb_sort.R)
  - [Insertion Sort](https://github.com/TheAlgorithms/R/blob/master/Sorting-Algorithms/insertion_sort.R)
  - [Quick Sort](https://github.com/TheAlgorithms/R/blob/master/Sorting-Algorithms/quick_sort.R)
  - [Selection Sort](https://github.com/TheAlgorithms/R/blob/master/Sorting-Algorithms/selection_sort.R)
  - [Stooge Sort](https://github.com/TheAlgorithms/R/blob/master/Sorting-Algorithms/stooge_sort.R)
  - [Merge Sort](https://github.com/TheAlgorithms/R/blob/master/Sorting-Algorithms/merge_sort.R)
  - [Radix Sort](https://github.com/TheAlgorithms/R/blob/master/Sorting-Algorithms/radix_sort.R)
  - [Heap Sort](https://github.com/TheAlgorithms/R/blob/master/Sorting-Algorithms/heap_sort.R)

### MATHEMATICS
  - [HammingDistance](https://github.com/TheAlgorithms/R/blob/master/Mathematics/HammingDistance.R)
  
## Contribution Guidelines
Please ensure to follow the points stated below if you would like to contribute:
- If your proposing a new algorithm or making changes to an existing one, make sure your code works. Reviewers or the general user must be able to directly emplace it in an R environment and get the desired output.
- Add an example to showcase the use of an algorithm proposed. It can be commented.
- Follow proper naming convention for variables (use `.` or `_` to seperate terms, such as `results.df` for a data frame containing some results) and filenames (follow the convention that has been followed for files under the directory your committing to).
- Feel free to add links here to the newly added file(s), but ensure that they do not result in a merge conflict with different versions of this readme under previous pull requests.
