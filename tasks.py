"""Tasks for property verification project.

Usage::

    $ invoke --list
    $ invoke [task_name]
"""
import json
from StringIO import StringIO

from invoke import task, run

import pandas as pd
from unipath import Path
import requests
import yaml

@task
def gather():
    """Gather the experiment data and put it in the R pkg data-raw folder.
    
    Currently set to get the fourth run experiment data.
    """
    dest_dir = Path('propertyverificationdata', 'data-raw', 'question_first',
                    'fourth_run', 'data')
    if not dest_dir.exists():
        dest_dir.mkdir(parents=True)
    data_files = Path('experiment/data').listdir('PV*csv')
    for data_file in data_files:
        dest = Path(dest_dir, data_file.name)
        run('cp {src} {dest}'.format(src=data_file, dest=dest))
        # move the subj info sheet
        run('cp experiment/subj_info.csv {}'.format(dest_dir.parent))

@task
def compile():
    """Run the use-data.R script to compile raw data to .rda files."""
    run('cd propertyverificationdata && Rscript data-raw/use-data.R')

@task
def install():
    """Install the propertyverificationdata R package."""
    # watch quotes!
    r_commands = [
        'devtools::document("propertyverificationdata")',
        'devtools::install("propertyverificationdata")',
    ]
    for r_command in r_commands:
        run("Rscript -e '{}'".format(r_command))

@task
def get_survey_data():
    """Fetch the survey responses and format them for analysis."""
    survey_dir = Path('individual_diffs')
    survey_questions_file = Path(survey_dir, 'survey_questions.csv')
    survey_data_dir = Path(survey_dir, 'survey_data')
    survey_data_file = Path(survey_data_dir, 'property_verification.csv')

    get_qualtrics_responses(output=survey_data_file)
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

def get_qualtrics_responses(name='property_verification',
        output='individual_diffs/survey_data/property_verification.csv'):
    """Get the survey data."""
    qualtrics_api_creds = Path('individual_diffs', 'qualtrics_api.yml')
    creds = yaml.load(open(qualtrics_api_creds))
    qualtrics = Qualtrics(**creds)
    responses = qualtrics.get_survey_responses(name)
    responses.to_csv(output, index=False)

class Qualtrics:
    def __init__(self, user, token):
        self.root_url = 'https://survey.qualtrics.com/WRAPI/ControlPanel/api.php'
        self.base_params = dict(
            API_SELECT='ControlPanel',
            Version=2.5,
            User=user,
            Token=token,
        )

    def get_survey_id(self, name):
        """Get the survey id assigned by Qualtrics given the survey name."""
        response = self.get(Request='getSurveys', Format='JSON')
        surveys = response.json()['Result']['Surveys']
        for survey in surveys:
            if survey['SurveyName'] == name:
                return survey['SurveyID']
        raise AssertionError('survey {} not found'.format(name))

    def get_survey_responses(self, name):
        """Get the survey data."""
        survey_id = self.get_survey_id(name)
        response = self.get(Request='getLegacyResponseData', Format='CSV',
                            SurveyID=survey_id)
        response_csv = StringIO(response.content)
        survey_data = pd.DataFrame.from_csv(response_csv)
        return survey_data

    def get(self, **kwargs):
        params = self.base_params.copy()
        params.update(kwargs)
        return requests.get(self.root_url, params=params)

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
