#!/usr/bin/env python
from __future__ import print_function
import json

from pandas import read_csv
from invoke import task, run

@task
def compile_survey():
    """Insert question strings as items in the template survey."""
    survey_template = 'survey_template.qsf'

    survey = json.load(open(survey_template))
    questions = read_csv('survey_questions.csv')
    # where to star the slider
    slider_start = 0

    # create choices from questions
    choices = {str(i): text for i, text in questions.question_str.iteritems()}

    # select the choices in the template survey
    slider_question = pluck(survey['SurveyElements'], 'SQ')
    content = slider_question['Payload']

    # insert choices and settings dependent on choices
    content['Choices'] = choices
    content['ChoiceOrder'] = sorted(map(int, choices.keys()))
    starting_positions = {k: slider_start for k in choices}
    content['Configuration']['SliderStartPositions'] = starting_positions

    json.dump(survey, open('survey_data.qsf', 'w'))

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
