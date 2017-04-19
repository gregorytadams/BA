# Rebuilt (from scratch!) version of my magic loop.
# Greg Adams

from __future__ import division
import pandas as pd
import numpy as np
from sklearn import preprocessing, cross_validation, svm, metrics
from sklearn.ensemble import RandomForestClassifier, ExtraTreesClassifier, GradientBoostingClassifier, AdaBoostClassifier
from sklearn.linear_model import LogisticRegression, SGDClassifier
from sklearn.naive_bayes import GaussianNB, MultinomialNB
from sklearn.tree import DecisionTreeClassifier
from sklearn.neighbors import KNeighborsClassifier
from sklearn.cross_validation import train_test_split
from sklearn.grid_search import ParameterGrid
from sklearn.metrics import precision_recall_curve, recall_score, auc, f1_score, roc_curve, precision_score, accuracy_score
import pylab as pl
import matplotlib.pyplot as plt

# MODELS_TO_RUN = CLFS.keys()
MODELS_TO_RUN =  ['NB','KNN','RF']
K_FOLDS = 3
THRESHOLDS = [x/10 for x in range(10)][1::2]
TOP_MODELS_FILENAME = ''
WHERE_TO_SAVE_PLOTS = ''
TOP_N = 30

CLFS = {'RF': RandomForestClassifier(n_estimators=50, n_jobs=-1),
    'ET': ExtraTreesClassifier(n_estimators=10, n_jobs=-1, criterion='entropy'),
    'AB': AdaBoostClassifier(DecisionTreeClassifier(max_depth=1), algorithm="SAMME", n_estimators=200),
    'LR': LogisticRegression(penalty='l1', C=1e5),
    'SVM': svm.SVC(kernel='linear', probability=True, random_state=0),
    'GB': GradientBoostingClassifier(learning_rate=0.05, subsample=0.5, max_depth=6, n_estimators=10),
    'NB': GaussianNB(),
    'DT': DecisionTreeClassifier(),
    # 'SGD': SGDClassifier(loss="hinge", penalty="l2"), #Doesn't give probability estimates or there's an overflow.  SVM is similar anyway.
    'KNN': KNeighborsClassifier(n_neighbors=3) 
        }

PARAMS = { 
    'RF':{'n_estimators': [1,10,100,1000, 10000], 'max_depth': [1,5,10,20,50,100], 'max_features': ['sqrt','log2'],'min_samples_split': [2,5,10]},
    'LR': { 'penalty': ['l1','l2'], 'C': [0.00001, 0.0001,0.001,0.01,0.1,1,10]},
    # 'SGD': { 'loss': ['log''hinge','perceptron'], 'penalty': ['l2','l1','elasticnet']},
    'ET': { 'n_estimators': [1,10,100,1000,10000], 'criterion' : ['gini', 'entropy'] ,'max_depth': [1,5,10,20,50,100], 'max_features': ['sqrt','log2'],'min_samples_split': [2,5,10]},
    'AB': { 'algorithm': ['SAMME', 'SAMME.R'], 'n_estimators': [1,10,100,1000, 10000]},
    'GB': {'n_estimators': [1,10,100,1000,10000], 'learning_rate' : [0.001,0.01,0.05,0.1,0.5],'subsample' : [0.1,0.5,1.0], 'max_depth': [1,3,5,10,20,50,100]},
    'NB' : {},
    'DT': {'criterion': ['gini', 'entropy'], 'max_depth': [1,5,10,20,50,100], 'max_features': ['sqrt','log2'],'min_samples_split': [2,5,10]},
    'SVM' :{'C' :[0.00001, 0.0001, 0.001,0.01,0.1,1,10],'kernel':['linear', 'poly', 'rbf', 'sigmoid']},
    'KNN' :{'n_neighbors': [1,5,10,25,50,100],'weights': ['uniform','distance'],'algorithm': ['auto','ball_tree','kd_tree']}
        }

def main(df, response, plot = False, filename = TOP_MODELS_FILENAME):
    '''
    df is a DataFrame
    response is a string
    '''
    y = df[response]
    X = df.drop(response, 1)
    top_models = magic_loop(X, y)
    top_models = rebuild_and_get_point_metrics(X, y, top_models, plot = plot)
    top_models.to_csv(filename)

#remember you can use df = df[[col1, col2 ... ]] as X 
def magic_loop(X, y, models_to_run = MODELS_TO_RUN, clfs = CLFS, \
                params = PARAMS, K_folds = K_FOLDS, top_n = TOP_N):
    ''' 
    Builds a lot of different ML models to find most predictive by cross-validated threshold-independent metrics.
    Metrics used: AUROC, AUPRC
    Takes a long time to run. 
    For numerical models.

    Inputs: 
    X: pandas dataframe of features/predictors
    y: 1-dimensional pandas dataframe (i.e. series) of response variables
    models_to_run: a list of model abbreviations that are the keys to clfs and params
    clfs: dictionary of classifiers to build 
    params: the paramaters of each classifier to try (tries every combination)
    K_folds: K for K-fold cross-validation.

    Output:
    top_models: a pandas dataframe of the top_n classifiers by each metric sans duplicates
    ROC and PRC plots of each classifier/parameter pair (the last model built during cross-validation)
    '''
    tracker = 0
    model_list = [['Models', 'Parameters', 'AUPRC', 'AUROC']]
    for index, clf in enumerate([clfs[x] for x in models_to_run]):
        parameter_values = params[models_to_run[index]]
        for p in ParameterGrid(parameter_values):
            print("Model: {}".format(models_to_run[index]))
            print("Params: {}".format(p))
            print("Model Number: {}".format(tracker))
            tracker += 1
            clf.set_params(**p)
            auprc, auroc = [], []
            for num_fold in range(K_folds):
                X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.1, random_state = num_fold)
                clf.fit(X_train, y_train)
                y_pred_probs = clf.predict_proba(X_test)[:,1]
                auprc_val, auroc_val = get_eval_stats(y_test, y_pred_probs)
                auprc.append(auprc_val)
                auroc.append(auroc_val)
            model_list.append([models_to_run[index], p, np.mean(auprc), np.mean(auroc)])
    model_df = pd.DataFrame(model_list[1:], columns = model_list[0]) 
    a = model_df[model_df.AUROC > 0.51] # drops random classifiers
    PR_df = a.sort('AUPRC').head(top_n) # grab top values
    RO_df = a.sort('AUROC').head(top_n)
    top_models = pd.concat([PR_df, RO_df]).drop_duplicates(['Models', 'AUPRC', 'AUROC']).reset_index(drop=True) # combine two dataframes
    return top_models 

def rebuild_and_get_point_metrics(X, y, top_models, clfs = CLFS, thresholds = THRESHOLDS, plot = False):
    '''
    Rebuilds best models from magic loop and gets point metrics at various thresholds.  Optionally plots.

    Inputs: 
    X: pandas dataframe of features/predictors
    y: 1-dimensional pandas dataframe (i.e. series) of response variables
    top_models, df from magic_loop
    clfs: dictionary of classifiers to build 
    thresholds, thresholds to evaluate the classifiers at
    plt, bool -- plot and save the models?

    outputs:
    new_df, top_models with point metrics added for each model
    '''
    model_list = [[]]
    for thresh in thresholds:
        model_list[0] += ['F1 at '+str(thresh), 'Precision at ' + str(thresh), \
                            'Recall at '+str(thresh), 'Accuracy at '+str(thresh)]    
    for index, row in top_models.iterrows():
        clf = clfs[row['Models']]
        p = row['Parameters']
        clf.set_params(**p)
        X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state = 0)
        clf.fit(X_train, y_train)
        y_pred_probs = clf.predict_proba(X_test)[:,1]
        row_of_metrics = []
        for thresh in thresholds:
            y_pred = np.asarray([1 if i >= thresh else 0 for i in y_pred_probs])
            row_of_metrics += get_model_evaluation_stats(y_test, y_pred)
        if plot:
            plot_precision_recall(y_test, y_pred_probs, index)
            plot_roc(y_test, y_pred_probs, index)
        model_list.append(row_of_metrics)
    df = pd.DataFrame(model_list[1:], columns = model_list[0]) 
    # print(df)
    new_df = pd.concat([top_models, df], axis=1)
    return new_df


# -----------------------------------------------------------------------
# ---------------- HELPER FUNCTIONS -------------------------------------
# -----------------------------------------------------------------------

def get_feature_importances(X, y, parameters, clfs = CLFS):
    clf = clfs['RF']
    p = parameters
    clf.set_params(**p)
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state = 0)
    clf.fit(X_train, y_train)
    fi = clf.feature_importances_
    return [(val, fi[i]) for i, val in enumerate(X.columns)]


def get_model_evaluation_stats(y_true, y_pred, labels=None, pos_label=1, average='binary', \
                               sample_weight=None, title="Summary", print_scores=False):
    '''
    Gives point evaulation metrics.

    Requires import above: 'from sklearn.metrics import f1_score, precision_score, recall_score, accuracy_score'
    
    Inputs:
    (all) same inputs as sklearn.model_selection inputs of the same name.  Refer to sklearn documentation at 
    http://scikit-learn.org/stable/modules/classes.html#module-sklearn.metrics

    note: y_pred must be discrete predictions
    '''
    f1 = f1_score(y_true, y_pred, pos_label=pos_label, average=average, sample_weight=sample_weight)
    precision = precision_score(y_true, y_pred, pos_label=pos_label, average=average, sample_weight=sample_weight)
    recall = recall_score(y_true, y_pred, pos_label=pos_label, average=average, sample_weight=sample_weight)
    acc = accuracy_score(y_true, y_pred, sample_weight=sample_weight)
    if print_scores:
        print("Title: {}".format(title))
        print("F1: {}".format(f1))
        print("Precision: {}".format(precision))
        print("Recall: {}".format(recall))
        print("Accuracy: {}".format(acc))
        print("-"*30)
    return [f1, precision, recall, acc]

def get_eval_stats(y_true, y_pred_probs):
    '''
    Returns non-point evaluation metrics AUROC and AUPRC for binary classification.
    '''
    precision_curve, recall_curve, pr_thresholds = precision_recall_curve(y_true, y_pred_probs)
    auprc_score = auc(recall_curve, precision_curve)
    fpr, tpr, thresholds = roc_curve(y_true, y_pred_probs)
    auroc_score = auc(fpr, tpr)
    return auprc_score, auroc_score

def plot_roc(y_true, y_score, model_name):
    fpr, tpr, _ =  roc_curve(y_true, y_score)
    plt.clf()
    plt.plot(fpr, tpr, lw=2, color='red', label='ROC curve')
    plt.plot([0,1], [0,1], color='blue', label='Random Classifier')
    plt.xlabel('FPR')
    plt.ylabel('TPR')
    plt.ylim([0.0, 1.05])
    plt.xlim([0.0, 1.0])
    plt.title('ROC')
    plt.savefig('{}ROC{}.png'.format(WHERE_TO_SAVE_PLOTS, model_name))
    plt.close('all')
    # plt.show()

def plot_precision_recall(y_true, y_score, model_name):
    '''
    This one gives recall vs. precision as one line.  

    Requires subfolder 'plots'
    '''
    precisions, recalls, thresholds =  precision_recall_curve(y_true, y_score)
    plt.clf()
    plt.plot(recalls, precisions, lw=2, color='red', label='Precision-Recall curve')
    plt.plot([0,1], [sum(y_true)/len(y_true)]*2, color='blue', label='Random Classifier')
    plt.xlabel('Recall')
    plt.ylabel('Precision')
    plt.ylim([0.0, 1.05])
    plt.xlim([0.0, 1.0])
    plt.title('Precision-Recall')
    plt.savefig('{}PR{}.png'.format(WHERE_TO_SAVE_PLOTS, model_name))
    plt.close('all')
    # plt.show()

def plot_precision_recall_separate(y_true, y_score, model_name, tracker = 0):
    '''
    this one give PR on 2 different y-axes. Slower than plot_precision_recall.

    Need to have a subfolder called "plots"
    '''
    precision_curve, recall_curve, pr_thresholds = precision_recall_curve(y_true, y_score)
    precision_curve = precision_curve[:-1]
    recall_curve = recall_curve[:-1]
    pct_above_per_thresh = []
    number_scored = len(y_score)
    for value in pr_thresholds:
        num_above_thresh = len(y_score[y_score>=value])
        pct_above_thresh = num_above_thresh / float(number_scored)
        pct_above_per_thresh.append(pct_above_thresh)
    pct_above_per_thresh = np.array(pct_above_per_thresh)
    plt.clf()
    fig, ax1 = plt.subplots()
    ax1.plot(pct_above_per_thresh, precision_curve, 'b')
    ax1.set_xlabel('percent of population')
    ax1.set_ylabel('precision', color='b')
    ax2 = ax1.twinx()
    ax2.plot(pct_above_per_thresh, recall_curve, 'r')
    ax2.set_ylabel('recall', color='r')
    name = model_name
    plt.title(name)
    plt.savefig('{}PR_sep{}.png'.format(WHERE_TO_SAVE_PLOTS, tracker))
    plt.close('all')
    # plt.show()



