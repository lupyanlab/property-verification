from invoke import task, run
import pandas as pd

@task
def knowledge_type():
    run("cd knowledge_type && python compile.py")

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
