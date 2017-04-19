# my_library.py
# Some redundant functions, but helpful general functions I used occasionally.


from sklearn.metrics import f1_score, precision_score, recall_score, accuracy_score, precision_recall_curve, auc, roc_curve
from sklearn.model_selection import KFold
from sklearn import preprocessing
import numpy as np
import pandas as pd 


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

def get_k_folds(data_list, target_list, k):
    '''
    Gets the k folds for cross validation.

    Inputs
    data_list, a list of lists, each sublist of which is a sentence
    target_list, a list of strings, each element of which is the label for data_list

    Outputs
    train_data and test_data:  a list of k folds, each of which is a list of lists with each sublist containing a sentence
    train_labels and test_labels: a list of lists, each sublist of which is comprised of strings that are labels
    '''
    kf = KFold(n_splits=k, shuffle=True, random_state=0)
    train_data = []
    train_labels = []
    test_data = []
    test_labels = []
    # kf.split returns tuple: ([all indices of training data], [all indices of test data])
    for tup in  kf.split(data_list):
        train_data.append([data_list[i] for i in tup[0]]) 
        train_labels.append([target_list[i] for i in tup[0]]) 
        test_data.append([data_list[i] for i in tup[1]]) 
        test_labels.append([target_list[i] for i in tup[1]]) 
    return train_data, train_labels, test_data, test_labels


def numberize(df, col):
    '''
    replaces categories with numbers.  Assumes some sort of sequence (i.e. 1 < 2 < 3).  Use split_to_indicators if that's wrong.
    http://scikit-learn.org/stable/modules/generated/sklearn.preprocessing.LabelEncoder.html#sklearn.preprocessing.LabelEncoder

    inputs: 
    df, a pandas DataFrame
    col, the column to numberize

    outputs:
    df, the DtataFrame with col numberized
    '''
    le = preprocessing.LabelEncoder()
    df[col] = le.fit_transform(list(df[col].values))
    return df

def split_to_indicators(df, col):
    '''
    Grabs categorical variables and makes them dummies for the models.
    '''
    df2 = pd.get_dummies(df[col])
    df = pd.concat([df, df2], axis=1)
    df = df.drop(col, 1)
    return df


def plot_roc(y_true, y_score, model_name):
    '''
    Plots ROC curve for binary classification
    '''
    fpr, tpr, _ =  roc_curve(y_true, y_score)
    plt.clf()
    plt.plot(fpr, tpr, lw=2, color='red', label='ROC curve')
    plt.plot([0,1], [0,1], color='blue', label='Random Classifier')
    plt.xlabel('FPR')
    plt.ylabel('TPR')
    plt.ylim([0.0, 1.05])
    plt.xlim([0.0, 1.0])
    plt.title('ROC')
    plt.savefig('plots/ROC/{}.png'.format(model_name))
    plt.close('all')
    # plt.show()

def plot_precision_recall(y_true, y_score, model_name):
    '''
    Plots recall vs. precision as one line in binary classification scenario.  

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
    plt.savefig('plots/PR/{}.png'.format(model_name))
    plt.close('all')
    # plt.show()

def plot_precision_recall_separate(y_true, y_score, model_name, tracker = 0):
    '''
    Plots PR on 2 different y-axes. Slower than plot_precision_recall.
    For Binary Classification.

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
    plt.savefig('plots/PR_plot{}.png'.format(tracker))
    plt.close('all')
    # plt.show()

def sorted_list_from_dict(dict_to_sort):
    '''
    takes dict d[string]:number and returns list [(string, number), ...] sorted by number 
    Easier than using a priority queue
    '''
    sorted_list = []
    for key in sorted(dict_of_centrality_scores, key=dict_of_centrality_scores.__getitem__):
        sorted_list.append((key, dict_of_centrality_scores[key]))
    return sorted_list

def change_categories(df, col, combine_dict):
    '''
    Combine dict looks like {A:B, B:C, C:C, D:B}
    '''
    # Alternative from Stack Overflow: 
    # df.col.map(combine_dict).astype("category", categories=set(combine_dict.values()))
    for i, line in df.iterrows():
        df.loc[i, col] = combine_dict[df.loc[i, col]]
    return df

def format_features_for_classifier(df, max_categories = 6):
    '''
    Rough and dirty: formats binary variables, splits categorical variables to indicators, and ignores continuous variables.

    input: df, the df of features 

    outpur: df, the formatted output 
    '''
    for col in df.columns:
        if len(df[col].unique()) == 2:
            df = numberize(df, col)
            print("{} is binary".format(col))
        elif len(df[col].unique()) <= max_categories:
            df = split_to_indicators(df, col)
            print("{} is categorical".format(col))
        else:
            print("{} is continuous".format(col))
    return df
