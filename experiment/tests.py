import yaml
import pytest

from question_first_trials import make_trials

@pytest.fixture
def trial_params():
    settings_file = 'question_first.yaml'
    return yaml.load(open(settings_file, 'r'))['trials']

def test_questions_are_unique(trial_params):
    trials = make_trials('.', trial_params, seed=100)
    trials['question_id'] = trials.cue + trials.question
    assert len(trials) == len(trials.question_id.unique())
    
