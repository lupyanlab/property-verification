"""Tasks for property verification project.

Usage::

    $ invoke --list
    $ invoke [task_name]
"""
from invoke import task, run
import pandas as pd

@task
def install_r_package_data():
    """Install the propertyverificationdata R package."""
    # watch quotes!
    r_commands = [
        'devtools::document("propertyverificationdata")',
        'devtools::install("propertyverificationdata")',
    ]
    for r_command in r_commands:
        run("Rscript -e '{}'".format(r_command))

@task
def replace_propositions():
    """Move the selected propositions into experiment/stimuli."""
    src = 'reports/propositions/balanced_propositions.csv'
    dst = 'experiment/stimuli/propositions.csv'

    selected_proposition_ids = pd.read_csv(src).proposition_id.tolist()

    all_propositions = pd.read_csv('experiment/stimuli/all_propositions.csv')
    keepers = all_propositions.proposition_id.isin(selected_proposition_ids)
    selected_propositions = all_propositions.ix[keepers]

    selected_propositions.sort_values(
        by=['feat_type', 'correct_response', 'cue'],
        ascending=[0, 0, 1],
        inplace=True)

    selected_propositions.to_csv(dst, index=False)

@task(replace_propositions)
def create_survey_questions():
    """Format propositions as questions to use in the Qualtrics survey."""
    propositions = pd.read_csv('experiment/stimuli/propositions.csv')
    survey_questions = propositions.ix[
        propositions.correct_response == 'yes',
        ['proposition_id', 'question', 'cue']
    ] 

    survey_questions = survey_questions.groupby('cue').apply(format_question)
    survey_questions = survey_questions[['proposition_id', 'question_str']]
    survey_questions.to_csv('individual_diffs/survey_questions.csv', index=False)

def format_question(cue_questions):
    assert len(cue_questions.cue.unique()) == 0
    cue = cue_questions.cue.iloc[0]

    if cue[:1] in list('aeiou'):
        article = 'an'
    else:
        article = 'a'

    pronoun = r'\ it\ '
    noun = ' {} {} '.format(article, cue)
    cue_questions['question_str'] = cue_questions.question.str.replace(
        pronoun, noun
    )

    return cue_questions
