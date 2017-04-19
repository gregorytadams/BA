# cleaning.py

SENTIMENT_CATEGORIES = ['passed', 'not_passed']

from string import digits, punctuation

def fetch_and_format():
    '''
    Fetches and formats the data. Assumes polarity data was extracted in the current folder.  
    Hardcoded for polarity dataset.

    Outputs:
    data, a list of raw strings from the text files
    labels, a list of strings of the SENTIMENT_CATEGORIES, corresponding to the data's labels
    '''
    print("Fetching data...")
    data = []
    labels = []
    counter = 0
    for cat in SENTIMENT_CATEGORIES:
        filenames = next(walk('./all_bills/{}/txts'.format(cat)))[2]
        for f in filenames:
            with open('./all_bills/{}/txts/{}'.format(cat, f)) as txt_f:
                data.append(remove_junk(txt_f.read()))
                labels.append(cat)
            counter += 1
            if counter % 100 == 0:
                print("{}/4502".format(counter))
    return data, labels


def remove_junk(string):
    '''
    Cleans up some of the big chunks of the bills.
    '''
    string = string.lower()
    remove_digits = string.maketrans('', '', digits)
    remove_punctuation = string.maketrans('', '', punctuation)
    res = string.translate(remove_digits).translate(remove_punctuation).replace('\n', ' ')
    return res