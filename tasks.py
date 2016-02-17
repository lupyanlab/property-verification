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
def build_survey():
    """Insert question strings as items in the template survey."""
    survey_dir = 'individual_diffs'
    survey_questions = Path(survey_dir, 'survey_questions.csv')
    survey_builder_dir = Path(survey_dir, 'survey_builder')
    survey_template = Path(survey_builder_dir, 'property_verification_template.qsf')
    survey_output = Path(survey_builder_dir, 'property_verification.qsf')

    # load the survey data exported from qualtrics
    survey = json.load(open(survey_template))

    # modify survey name and id
    survey['SurveyEntry']['SurveyName'] = 'property_verification'
    survey['SurveyEntry']['SurveyID'] = 'SV_property_verification'

    # load questions
    questions = pd.read_csv(survey_questions)

    # start index at 1 instead of 0
    questions.index = questions.index.values + 1

    # create choices from questions
    choices = {str(i): {'Display': text}
        for i, text in questions.question_str.iteritems()
    }

    # select the correct question in the template survey
    visual_knowledge_question = pluck(survey['SurveyElements'], 'imagery')
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

@task
def compile_survey_data():
    """Transform the raw output from qualtrics to a format for analysis."""
    survey_dir = Path('individual_diffs')
    survey_questions_file = Path(survey_dir, 'survey_questions.csv')
    survey_data_dir = Path(survey_dir, 'survey_data')
    survey_data_file = Path(survey_data_dir, 'property_verification.csv')
    survey_data = pd.read_csv(survey_data_file, skiprows=[0,])

    imagery_output = Path(survey_dir, 'imagery.csv')
    # strategy_output = Path(survey_dir, 'strategy.csv')

    id_col = 'subj_id'

    is_imagery_col = survey_data.columns.str.contains('imagery')
    imagery_cols = survey_data.columns[is_imagery_col].tolist()
    imagery = pd.melt(survey_data, id_col, imagery_cols,
                      var_name = 'qualtrics', value_name = 'imagery')

    # separate choice text
    imagery['question_str'] = imagery.qualtrics.str.split('-').str.get(1)
    # drop qualtrics entry for the instructions "question"
    imagery = imagery.ix[imagery.question_str != 'text']
    imagery.drop('qualtrics', axis=1, inplace=True)

    # read in the map between question strings and proposition ids
    survey_questions = pd.read_csv(survey_questions_file)

    imagery = imagery.merge(survey_questions)
    imagery.to_csv(imagery_output, index=False)
