# -*- coding: utf-8 -*-
"""
Created on Sun Jul  2 16:08:14 2018

@author: Hao J.
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import time

from sklearn.model_selection import learning_curve
from sklearn.linear_model import LinearRegression
from sklearn.preprocessing import PolynomialFeatures
from sklearn.pipeline import Pipeline
from sklearn.model_selection import train_test_split
from sklearn.model_selection import ShuffleSplit

def get_data_5yr_est(price, generation, sales):
    # dataframe column names
    cols = ['State','Year','NetGeneration','Price','Sales',
            'GenLag5','GenLag6','GenLag7','GenLag8','GenLag9',
            'PriceLag5','PriceLag6','PriceLag7','PriceLag8','PriceLag9',
            'SalesLag5','SalesLag6','SalesLag7','SalesLag8','SalesLag9']
    # create an empty dataframe
    df = pd.DataFrame(columns = cols)
    
    # iterating all the data in price, generation and sales
    for st in generation.State:
        # start from 2009, since my data only contains data from year 2000
        for yr in range(2009,2017):
            # create a temp dataframe to put all the data together
            tmp = pd.DataFrame(columns = cols)
            tmp.at[0,'State'] = st
            tmp.at[0,'Year'] = yr
            tmp.at[0,'NetGeneration'] = generation[generation.State == st][str(yr)].iloc[0]
            tmp.at[0,'Price'] = price[price.State == st][str(yr)].iloc[0]
            tmp.at[0,'Sales'] = sales[sales.State == st][str(yr)].iloc[0]
            # calculate for the lag
            for i in range(5,10):
                # create targeted column name
                GenLag = 'GenLag' + str(i)
                PriceLag = 'PriceLag' + str(i)
                SalesLag = 'SalesLag' + str(i)
                #
                if (yr-i) >= 2000:
                    tmp.at[0,GenLag] = generation[generation.State == st][str(yr-i)].iloc[0]
                    tmp.at[0,PriceLag] = price[price.State == st][str(yr-i)].iloc[0]
                    tmp.at[0,SalesLag] = sales[sales.State == st][str(yr-i)].iloc[0]
                else:
                    print('Error! Exceed the year 2000, the year and i is', yr, '  ', i)
                    
            df = df.append(tmp, ignore_index=True)
    
    df.Year = pd.to_numeric(df.Year)
    df.NetGeneration = pd.to_numeric(df.NetGeneration)
    df.Price = pd.to_numeric(df.Price)
    df.Sales = pd.to_numeric(df.Sales)
    df.GenLag5 = pd.to_numeric(df.GenLag5)
    df.GenLag6 = pd.to_numeric(df.GenLag6)
    df.GenLag7 = pd.to_numeric(df.GenLag7)
    df.GenLag8 = pd.to_numeric(df.GenLag8)
    df.GenLag9 = pd.to_numeric(df.GenLag9)
    df.PriceLag5 = pd.to_numeric(df.PriceLag5)
    df.PriceLag6 = pd.to_numeric(df.PriceLag6)
    df.PriceLag7 = pd.to_numeric(df.PriceLag7)
    df.PriceLag8 = pd.to_numeric(df.PriceLag8)
    df.PriceLag9 = pd.to_numeric(df.PriceLag9)
    df.SalesLag5 = pd.to_numeric(df.SalesLag5)
    df.SalesLag6 = pd.to_numeric(df.SalesLag6)
    df.SalesLag7 = pd.to_numeric(df.SalesLag7)
    df.SalesLag8 = pd.to_numeric(df.SalesLag8)
    df.SalesLag9 = pd.to_numeric(df.SalesLag9)
    
    return df

def normalization_bygroup(df):
    cols = ('State','Year','NetGeneration','Price','Sales',
            'GenLag5','GenLag6','GenLag7','GenLag8','GenLag9',
            'PriceLag5','PriceLag6','PriceLag7','PriceLag8','PriceLag9',
            'SalesLag5','SalesLag6','SalesLag7','SalesLag8','SalesLag9')
    df_ = df.loc[:,cols]
    grouped = df_.groupby('State')
    # zero mean normalization
#    zero_mean = lambda x: (x - x.mean()) / x.std()
    # min max normalization
    zero_mean = lambda x: (x - x.min()) / (x.max() - x.min())
    df_norm = grouped.transform(zero_mean)
    df_norm['State'] = df.loc[:,'State']
    
    #save the mean and std for furture recoverary
    cols = ('Year','NetGeneration','Price','Sales',
            'GenLag5','GenLag6','GenLag7','GenLag8','GenLag9',
            'PriceLag5','PriceLag6','PriceLag7','PriceLag8','PriceLag9',
            'SalesLag5','SalesLag6','SalesLag7','SalesLag8','SalesLag9')
    tab = grouped[cols].agg([np.mean, np.std])
    
    return df_norm, tab
    
def plot_visual(df, df_, state, title1, title2):
    
    fig, axes = plt.subplots(nrows=1, ncols=2, squeeze=False,)
    
    tmp = df[df.State == state]
    tmp.plot(x = 'Year', y = 'NetGeneration', ax = axes[0,0], 
             figsize=(15,5), title = title1)
    
    tmp = df_[df_.State == state]
    tmp.plot(x = 'Year', y = 'NetGeneration', ax = axes[0,1], 
             figsize=(15,5), title = title2)
    

def polynomial_model(degree = 1):
    polynomial_features = PolynomialFeatures(degree=degree, 
                                             include_bias=False)
    linear_regression = LinearRegression(normalize=False)
    pipeline = Pipeline([("polynomial_features", polynomial_features),
                         ("linear_regression", linear_regression)])
    return pipeline

def plot_learning_curve(plt, estimator, title, X, y, ylim=None, cv=None,
                        n_jobs=1, train_sizes=np.linspace(.1, 1.0, 5)):
    """
    Generate a simple plot of the test and training learning curve.
    ----------
    estimator : object type that implements the "fit" and "predict" methods
        An object of that type which is cloned for each validation.

    title : string title for the chart.

    X : array-like, shape (n_samples, n_features)
        Training vector, where n_samples is the number of samples and
        n_features is the number of features.

    y : array-like, shape (n_samples) or (n_samples, n_features), optional
        Target relative to X for classification or regression;
        None for unsupervised learning.

    ylim : tuple, shape (ymin, ymax), optional
        Defines minimum and maximum yvalues plotted.

    cv : int, cross-validation generator or an iterable, optional
        Determines the cross-validation splitting strategy.
        Possible inputs for cv are:
          - None, to use the default 3-fold cross-validation,
          - integer, to specify the number of folds.
          - An object to be used as a cross-validation generator.
          - An iterable yielding train/test splits.

        For integer/None inputs, if ``y`` is binary or multiclass,
        :class:`StratifiedKFold` used. If the estimator is not a classifier
        or if ``y`` is neither binary nor multiclass, :class:`KFold` is used.

    n_jobs : integer, optional
        Number of jobs to run in parallel (default 1).
    """
    plt.title(title)
    if ylim is not None:
        plt.ylim(*ylim)
    plt.xlabel("Training examples")
    plt.ylabel("Score")
    train_sizes, train_scores, test_scores = learning_curve(
        estimator, X, y, cv=cv, n_jobs=n_jobs, train_sizes=train_sizes)
    train_scores_mean = np.mean(train_scores, axis=1)
    train_scores_std = np.std(train_scores, axis=1)
    test_scores_mean = np.mean(test_scores, axis=1)
    test_scores_std = np.std(test_scores, axis=1)
    plt.grid()

    plt.fill_between(train_sizes, train_scores_mean - train_scores_std,
                     train_scores_mean + train_scores_std, alpha=0.1,
                     color="r")
    plt.fill_between(train_sizes, test_scores_mean - test_scores_std,
                     test_scores_mean + test_scores_std, alpha=0.1, color="g")
    plt.plot(train_sizes, train_scores_mean, 'o--', color="r",
             label="Training score")
    plt.plot(train_sizes, test_scores_mean, 'o-', color="g",
             label="Cross-validation score")

    plt.legend(loc="best")
    return plt
  
# get data
## Average retail price cents kWh
file_loc = "data/1.csv"
price = pd.read_csv(file_loc, sep=',', thousands=',')
## Net generation (MWh)
file_loc = "data/2.csv"
generation = pd.read_csv(file_loc, sep=',', thousands=',')
## Total retail sales (MWh)
file_loc = "data/3.csv"
sales = pd.read_csv(file_loc, sep=',', thousands=',')

# extract useful data
df = get_data_5yr_est(price,generation,sales)

# normalization
df_, table = normalization_bygroup(df)
## visualization the data, double check the normalization does not
## change the relation between data
plot_visual(df, df_, 'Texas', 'Texas State, Before Normalization', 
            'Texas State, After Normalization' )
#plot_visual(df, df_, 'California', 'California State, Before Normalization', 
#            'California State, After Normalization' )

#### begin #####


cols = ('Year','GenLag5','GenLag6','GenLag7','GenLag8','GenLag9',
        'PriceLag5','PriceLag6','PriceLag7','PriceLag8','PriceLag9',
        'SalesLag5','SalesLag6','SalesLag7','SalesLag8','SalesLag9')
X = df_.loc[:,cols]
Y = df_.loc[:,'NetGeneration']

# get training dataset and and testing dataset
x_, x_test, y_, y_test = train_test_split(X, Y, test_size=0.2, random_state=3)

cv = ShuffleSplit(n_splits=10, test_size=0.2, random_state=0)
plt.figure(figsize=(18, 4), dpi=200)
title = 'Learning Curves (degree={0})'

degrees = [1, 2, 3, 4]

start = time.clock()

for i in range(len(degrees)):
    plt.subplot(1, len(degrees), i + 1)
    plot_learning_curve(plt, polynomial_model(degrees[i]), title.format(degrees[i]), x_, y_, ylim=(0.01, 1.01), cv=cv)

print('elaspe: {0:.6f}'.format(time.clock()-start))


