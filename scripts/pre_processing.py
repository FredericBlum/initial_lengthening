"""
This script pre-processes the DoReCo dataset.
The main computations are local speech rate and phones in word.
"""
import pandas as pd
import numpy as np


##################################################
# Works
data = pd.read_csv(
    '../../doreco_cldf/cldf/phones.csv',
    sep=',',
    keep_default_na=False
    )

##################################################
# Testcase with pycldf
# from pycldf import Dataset
# doreco = Dataset.from_metadata('../../doreco_cldf/cldf/StructureDataset-metadata.json')

# phones = list(doreco['phones.csv'])

# print(phones[0])
# files = {r['id']: r for r in doreco['metadata.csv']}

# data = data[0:500]

# for x in files:
#     print(x)

##################################################
##################################################
# initial - final
##################################################

utt_level = []
word_level = []
final = []


def initial_position(df):
    """
    Computing whether any token is in word- or utterance initial position.
    """
    for index, _ in df.iterrows():
        prec = index - 1
        pos = index + 1

        if prec != -1 and df.loc[index, "wd_ID"] != df.loc[prec, "wd_ID"]:
            word_level.append('1')
        elif prec == -1:
            word_level.append('1')
        else:
            word_level.append('0')

        if prec != -1 and df.loc[prec, "ph"] == "<p:>":
            utt_level.append('1')
        elif prec == -1:
            utt_level.append('1')
        else:
            utt_level.append('0')

        if pos < data.index[len(df) - 1] and df.loc[pos, "ph"] == "<p:>":
            final.append('1')
        elif pos == data.index[len(df) - 1]:
            final.append('1')
        else:
            final.append('0')

    return utt_level, word_level


initial_position(data)

data["word_initial"] = word_level
data["utt_initial"] = utt_level
data["final"] = final


##################################################
##################################################
# IPU time
##################################################
ipu_len = []
count_ls = []


def ipu(df):
    for index, _ in df.iterrows():
        pos_min = index
        pos_max = index
        count = 1

        if "<" in df.loc[index, "ph"]:
            ipu_len.append("NA")
            count_ls.append("NA")
        else:
            if index != 0:
                while pos_min > 0 and "<" not in df.loc[pos_min, "ph"] and df.loc[pos_min, "Filename"] == df.loc[index, "Filename"]:
                    pos_min -= 1
                    count += 1
                pos_min += 1
                count -= 1

            if index != data.index[len(df) - 1]:
                while pos_max < data.index[len(df) - 1] and "<" not in df.loc[pos_max, "ph"] and df.loc[pos_max, "Filename"] == df.loc[index, "Filename"]:
                    pos_max += 1
                    count += 1
                if pos_max != 0:
                    pos_max -= 1
                    count -= 1

            start_time = df.loc[pos_min, 'start']
            end_time = df.loc[pos_max, 'end']

            # if end_time != 0:
            dur = end_time - start_time
            dur_per_phone = round((count/dur), 3)
            ipu_len.append(dur_per_phone)
            count_ls.append(count)
            # else:
            #     ipu_len.append("NA")
            #     count_ls.append("NA")
    return ipu_len, count_ls


ipu(data)

data['SpeechRate'] = ipu_len
data["count_ipu"] = count_ls

##################################################
##################################################
# Word length in Phonemes
##################################################
data = data[data['SpeechRate'] != "NA"]

data['PhonemesWord'] = data.groupby(
    ['wd_ID', 'Language_ID', 'Filename']
    )['wd_ID'].transform('count')

data['WordCount'] = data.groupby(
    ['wd', 'Language_ID', 'Filename']
    )['wd'].transform('count')

# data['lang_count'] = data.groupby("wd_ID").size().groupby(level=0).agg({'count(lang_count':'size'})

#data['WordFreq'] = data['WordCount'] / data['PhonemesWord']
#data['WordFormFreq'] = data['WordFreq'] / data['lang_count']

# log: SpeechRate, WordFormFreq, PhonemesWord, Duration
data['logDuration'] = round(np.log(data['duration']), 3)
data['logPhonWord'] = round(np.log(data['PhonemesWord']), 3)

data.to_csv('../data/new_predictors_fb.csv', index=False)
