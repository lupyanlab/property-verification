"""Tasks for property verification project.

Usage::

    $ invoke --list
    $ invoke [task_name]
"""
import json

import pandas as pd
from invoke import task, run
from unipath import Path

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

@task
def compile_survey():
    """Insert question strings as items in the template survey."""
    survey_builder_dir = 'individual_diffs'
    survey_template = Path(survey_builder_dir, 'survey_template.qsf')
    survey_questions = Path(survey_builder_dir, 'survey_questions.csv')
    survey_output = Path(survey_builder_dir, 'survey_data.qsf')

    # where to start the slider
    slider_start = 0

    survey = json.load(open(survey_template))
    questions = pd.read_csv(survey_questions)

    # start index at 1 instead of 0
    questions.index = questions.index.values + 1

    # create choices from questions
    choices = {str(i): {'Display': text}
        for i, text in questions.question_str.iteritems()
    }

    # select the choices in the template survey
    visual_knowledge_question = pluck(survey['SurveyElements'], 'SQ')
    content = visual_knowledge_question['Payload']

    # insert choices and settings dependent on choices
    content['Choices'] = choices
    content['ChoiceOrder'] = sorted(map(int, choices.keys()))

    json.dump(survey, open(survey_output, 'w'))

def pluck(items, search_term):
    for item in items:
        try:
            values = item.values()
        except AttributeError:
            continue
        else:
            if search_term in values:
                return item
    raise AssertionError('search term {} not found'.format(search_term))
