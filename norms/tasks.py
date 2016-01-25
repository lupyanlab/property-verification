from invoke import task, run
import pandas as pd
import unipath

@task
def knowledge_type():
    knowledge_type_measures = ['truth', 'difficulty', 'imagery', 'facts']
    survey_dirs = [
        'knowledge_type/survey-1',
        'knowledge_type/survey-2',
        'knowledge_type/survey-3',
    ]
    surveys_data = [compile_survey(survey_dir, knowledge_type_measures)
                    for survey_dir in survey_dirs]
    survey_data = pd.concat(surveys_data)

    # merge in proposition info based on proposition_id
    propositions = pd.read_csv('propositions.csv')
    survey_data = survey_data.merge(propositions)

    # format data for inspection
    survey_data.sort_values(
        ['subj_id', 'proposition_id', 'measure'],
        inplace=True,
    )

    # save the long form data
    survey_data.to_csv('knowledge_type.csv', index=False)

    # survey_data_summary = summarize_survey(survey_data)
    # survey_data_summary.to_csv('knowledge_type_summary.csv', index=False)

def compile_survey(survey_dir, measures):
    """
    Usage::
        
        compile_survey('survey-1', ['truth', 'difficulty', 'imagery', 'facts'])

    Args:
        survey_dir (str): Place to look for loop merge and response data.
        measures (list): Measures to extract for each propositions.
    """
    loop_merge = process_loop_merge(survey_dir)
    qualtrics_data = process_qualtrics_data(survey_dir, measures)
    survey_data = pd.merge(qualtrics_data, loop_merge)

    final_data_cols = ['subj_id', 'proposition_id', 'measure', 'value']
    final_survey_data = survey_data[final_data_cols].reset_index(drop=True)
    return final_survey_data

def process_loop_merge(survey_dir):
    """Create a map between loop_merge_row and proposition_id."""
    loop_merge = pd.read_csv(unipath.Path(survey_dir, 'loop-merge.csv'))
    # label loop merge row, starting with 1
    loop_merge['loop_merge_row'] = range(1, len(loop_merge) + 1)
    loop_merge['proposition_id'] = loop_merge.apply(
        create_proposition_id,
        axis=1,
    )
    loop_merge_cols = ['loop_merge_row', 'proposition_id']
    return loop_merge[loop_merge_cols]

def process_qualtrics_data(survey_dir, measures):
    qualtrics_data = pd.read_csv(
        unipath.Path(survey_dir, 'qualtrics-data.csv'),
        skiprows=[1, ],
    )

    # label subj_id
    subj_id_col = 'subj_id'
    qualtrics_data.rename(columns={'worker': subj_id_col}, inplace=True)

    # remove practice columns before selecting columns
    qualtrics_data = remove_practice_cols(qualtrics_data)

    # subset requested measure columns
    all_qualtrics_cols = pd.Series(qualtrics_data.columns)
    re_contains = '|'.join(measures)
    qualtrics_measure_cols = all_qualtrics_cols[
        all_qualtrics_cols.str.contains(re_contains)
    ] 

    # melt data
    keep_cols = [subj_id_col, ] + qualtrics_measure_cols.tolist()
    long_data = pd.melt(
        qualtrics_data[keep_cols],
        id_vars = subj_id_col,
        var_name = 'qualtrics_col',
        value_name = 'value',
    )

    # drop empty cells
    long_data.dropna(subset=['subj_id', 'value'], inplace=True)
     
    # extract data from qualtrics col
    # example column: "truth(10)"
    re_qualtrics_col = r'^(?P<measure>[a-z]+)\((?P<loop_merge_row>\d+)\)$'
    data_from_qualtrics_col = long_data.qualtrics_col.str.extract(
        re_qualtrics_col
    )
    # convert loop_merge_row from str to int
    data_from_qualtrics_col['loop_merge_row'] = pd.to_numeric(
        data_from_qualtrics_col.loop_merge_row
    )

    # replace qualtrics col with data
    labeled_data = pd.concat([long_data, data_from_qualtrics_col], axis=1)
    labeled_data.drop('qualtrics_col', axis=1, inplace=True)

    return labeled_data

def remove_practice_cols(qualtrics_data):
    cols = pd.Series(qualtrics_data.columns)
    start_practice = cols[cols == 'PR1'].index[0]
    end_practice = start_practice + 11  # number of practice columns
    practice_cols = qualtrics_data.columns[start_practice:end_practice]
    return qualtrics_data.drop(practice_cols, axis=1)

def create_proposition_id(row):
    reqs = ['question', 'cue']
    assert all([x in row for x in reqs])

    question_slug = row['question'].lower().replace(' ', '-').strip('?')
    return '{}:{}'.format(question_slug, row['cue'])

@task
def senses():
    run("cd senses && python compile.py")

@task
def category_proportions():
    brm = pd.read_table('mcrae_et_al/McRae-BRM-InPress/CONCS_brm.txt')
    visual_columns = ['Num_Vis_Mot', 'Num_VisF&S', 'Num_Sound']
    brm['prop_visual'] = brm[visual_columns].sum(axis=1) / brm.Num_Feats_No_Tax

    category_proportions = brm[['Concept', 'prop_visual']].rename(
        columns = {'Concept': 'cue'}
    )

    dst = 'mcrae_et_al/category_proportions.csv'
    category_proportions.to_csv(dst, index=False)

@task(knowledge_type, senses, category_proportions)
def norms():
    knowledge_type = pd.read_csv('knowledge_type/knowledge_type.csv')
    senses = pd.read_csv('senses/senses.csv')
    category_proportions = pd.read_csv('mcrae_et_al/category_proportions.csv')

    # ignore truth and difficulty measures from senses survey
    sense_measure_cols = senses.columns[senses.columns.str.contains('senses')]
    senses_only = senses[['cue', 'question'] + sense_measure_cols.tolist()]

    norms = pd.merge(knowledge_type, senses_only)

    norms = norms.merge(category_proportions)
    norms.to_csv('norms.csv', index=False)
