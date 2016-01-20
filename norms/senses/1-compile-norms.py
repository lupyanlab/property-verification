#!/usr/bin/env python
import pandas as pd
import numpy as np

def remove_practice_cols(df):
    """
    Remove the practice columns before any summarizing
    """
    cols = pd.Series(df.columns)
    start_practice = cols[cols == 'PR1'].index[0]
    end_practice = start_practice + 11 # eleven practice columns
    practice_cols = df.columns[start_practice:end_practice]
    return df.drop(practice_cols, axis = 1)

def compile_question_types(df, q_version,
        q_types = ['truth', 'imagery', 'facts', 'difficulty']):
    """
    Compile the columns for each question type separately
    """
    all_questions = {}

    for q_type in q_types:
        all_questions[q_type] = compile_question(
            df, q_type = q_type, q_version = q_version
        )

    return all_questions

def compile_question(df, q_type, q_version):
    """
    Extract and label the columns corresponding to the question type
    """
    cols = pd.Series(df.columns)
    q_type_cols = df.columns[cols.str.contains(q_type)]

    df = df[q_type_cols]
    df.columns.name = 'q_str'
    df = df.T.reset_index()

    # extract the loop & merge row number from question string
    df['q_num'] = df.q_str.str.extract('(\d+)').astype(int)

    df = merge_question_id(df, q_version)
    return df

def merge_question_id(df, q_version):
    """
    Merge uninformative question number with informative question label
    """
    # merge question info with mTurk data by row number
    questions = pd.read_csv(q_version)
    questions['q_num'] = range(1, len(questions) + 1)
    questions['question_id'] = questions.cue + ':' + questions.ftype + ':' + \
                            questions.qid.astype(str) + ':' + questions.response
    questions = questions[['q_num','question_id']]

    df = questions.merge(df, on = 'q_num')
    return df.set_index('question_id').drop(['q_num', 'q_str'], axis = 1)

def merge_versions(version_dict):
    """
    Merge the compiled data for each question type across versions
    """
    all_questions = {}

    for q_type in ['truth', 'imagery', 'facts', 'difficulty']:
        version_keys = version_dict.keys()
        left = version_dict[version_keys.pop()][q_type]
        while len(version_keys) > 0:
            right = version_dict[version_keys.pop()][q_type]
            left = left.merge(right, how = 'outer', left_index = True, right_index = True)

        all_questions[q_type] = left

    return all_questions

def summarize_question_types(all_questions,
        q_types = ['truth', 'imagery', 'facts', 'difficulty']):
    """
    Once everything has been merged, then we can compute descriptive stats
    """
    all_summarized = {}

    for q_type in q_types:
        all_summarized[q_type] = summarize_question(all_questions[q_type])

    return all_summarized

def summarize_question(df):
    # summarize stats for each question
    # ignores NAs by default
    df = df.T.describe().T.reset_index()
    df = df.rename(columns = {'index': 'question_id'})
    df = df.drop(['25%','50%','75%'], axis = 1)
    return df

def merge_questions(all_questions):
    """
    Easy to merge all question types into a single frame with informative labels
    """
    # separate stat val cols by question
    duplicates = ['mean','std','min','max']

    for k, v in all_questions.items():
        all_questions[k] = v.rename(
            columns = {dup:k+'_'+dup for dup in duplicates}
        )

    keys = all_questions.keys()
    left = all_questions[keys.pop()]
    while len(keys) > 0:
        right = all_questions[keys.pop()]
        left = left.merge(right, on = ['question_id', 'count'])

    return left

def merge_info(df, q_version):
    """
    Add informative question information
    """
    questions = pd.read_csv(q_version)
    questions['question_id'] = questions.cue + ':' + questions.ftype + ':' + \
                            questions.qid.astype(str) + ':' + questions.response

    df = questions.merge(df, on = 'question_id')
    df = df.drop('question_id', axis = 1)
    return df

def convert_z(ser):
    return (ser - ser.mean())/ser.std()

if __name__ == '__main__':
    data_dir = './mturk-data/'

    senses_q_types = ['truth', 'senses', 'difficulty']

    v1 = pd.read_csv(data_dir + 'mwpfeaturessensory.csv', skiprows = [1,])
    v1 = remove_practice_cols(v1)
    v1 = compile_question_types(v1, data_dir + 'questions.csv',
        q_types = senses_q_types)

    # allv = merge_versions({'v1':v1, 'v2':v2, 'v3':v3})

    allv = summarize_question_types(v1, q_types = senses_q_types)
    df = merge_questions(allv)
    df = merge_info(df, data_dir + 'questions.csv')

    df['truth_z'] = convert_z(df.truth_mean)
    df['senses_z'] = convert_z(df.senses_mean)
    df['difficulty_z'] = convert_z(df.difficulty_mean)

    df = df.rename(columns = {'response':'truth_coded'})
    df['truth_normed'] = np.nan
    df['truth_normed'][df.truth_mean - (df.truth_std / df['count'].apply(np.sqrt)) > 0.0] = 'yes'
    df['truth_normed'][df.truth_mean + (df.truth_std / df['count'].apply(np.sqrt)) < 0.0] = 'no'

    df['truth_agree'] = np.nan
    df['truth_agree'][df.truth_normed.notnull()] = (df.truth_coded == df.truth_normed).astype(int)

    dis = df[(df.truth_agree.notnull() & (df.truth_agree == False))]
    dis.to_csv('./truth_disagree.csv', index = False)

    df.to_csv('./feature_norms_senses.csv', index = False)
