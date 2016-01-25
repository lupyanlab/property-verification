import os
import yaml

import pytest
import unipath

from run import Trials


@pytest.fixture(scope='module', params=[100, 104, 108])
def trials(request):
    trials_csv = 'test_trials.csv'
    trials = Trials.make(seed=request.param)
    trials.write(trials_csv)

    def remove_trials():
        os.remove(trials_csv)
    request.addfinalizer(remove_trials)

    return trials

def test_proposition_ids_are_unique(trials):
    frame = trials.to_dataframe()
    assert len(frame) == len(frame.proposition_id.unique())

def test_trials_are_correct_length(trials):
    assert len(trials) > 200 and len(trials) < 250

def test_cue_files_exist(trials):
    frame = trials.to_dataframe()
    available = unipath.Path('stimuli/cues').listdir('*.wav', names_only=True)
    used = frame.cue_file.unique()
    assert all([cue in available for cue in used])
