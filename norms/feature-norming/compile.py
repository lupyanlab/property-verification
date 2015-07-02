#!/usr/bin/env python
import pandas as pd

def remove_practice_cols(df):
    cols = pd.Series(df.columns)
    start_practice = cols[cols == 'PR1'].index[0]
    end_practice = start_practice + 11 # eleven practice columns
    practice_cols = df.columns[start_practice:end_practice]
    return df.drop(practice_cols, axis = 1)

def compile_question_types(df, q_version):
    all_questions = {}
    
    for q_type in ['truth', 'imagery', 'facts', 'difficulty']:
        all_questions[q_type] = compile_question(
            df, q_type = q_type, q_version = q_version
        )
    
    return all_questions

def compile_question(df, q_type, q_version):
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
    # merge question info with mTurk data by row number
    questions = pd.read_csv(q_version)
    questions['q_num'] = range(1, len(questions) + 1)
    questions['question_id'] = questions.cue + ':' + questions.ftype + ':' + \
                            questions.qid.astype(str) + ':' + questions.response
    questions = questions[['q_num','question_id']]    
    
    df = questions.merge(df, on = 'q_num')
    return df.set_index('question_id').drop(['q_num', 'q_str'], axis = 1)

def merge_versions(version_dict):
    all_questions = {}
    
    for q_type in ['truth', 'imagery', 'facts', 'difficulty']: 
        version_keys = version_dict.keys()
        left = version_dict[version_keys.pop()][q_type]
        while len(version_keys) > 0:
            right = version_dict[version_keys.pop()][q_type]
            left = left.merge(right, how = 'outer', left_index = True, right_index = True)
        
        all_questions[q_type] = left
    
    return all_questions

def summarize_question_types(all_questions):
    all_summarized = {}
    
    for q_type in ['truth', 'imagery', 'facts', 'difficulty']:
        all_summarized[q_type] = summarize_question(all_questions[q_type])
    
    return all_summarized

def summarize_question(df):
    # summarize stats for each question
    # ignores NAs by default
    df = df.T.describe().T.reset_index()
    df = df.drop(['25%','50%','75%'], axis = 1)
    return df

def merge_questions(all_questions):
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

def merge_info(df):
    questions = pd.read_csv('questions.csv')
    questions['question_id'] = questions.cue + ':' + questions.ftype + ':' + \
                            questions.qid.astype(str) + ':' + questions.response 
    
    df = questions.merge(df, on = 'question_id')
    return df

if __name__ == '__main__':
    v1 = pd.read_csv('mwpfeatures.csv', skiprows = [1,])
    v1 = remove_practice_cols(v1)
    v1 = compile_question_types(v1, 'questions.csv')
    
    v2 = pd.read_csv('mwpfeatures-sub2.csv', skiprows = [1,])
    v2 = remove_practice_cols(v2)
    v2 = compile_question_types(v2, 'questions-sub2.csv')
    
    v3 = pd.read_csv('mwpfeatures-sub3.csv', skiprows = [1,])
    v3 = remove_practice_cols(v3)
    v3 = compile_question_types(v3, 'questions-sub3.csv')
    
    
    allv = merge_versions({'v1':v1, 'v2':v2, 'v3':v3})
    
    allv = summarize_question_types(allv)
    df = merge_questions(allv)
    df = merge_info(df)
    
    df.to_csv('feature_norms.csv', index = False)
    

    
    
    

    
    
