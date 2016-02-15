from __future__ import print_function
import os
import yaml

import pytest
import unipath
import pandas as pd

from run import Trials


@pytest.fixture(scope='module', params=[100, 104, 108])
def trials(request):
    return Trials.make(seed=request.param)

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

def test_propositions_are_correct_proportion(trials):
    frame = trials.to_dataframe()
    proportions = frame.groupby('correct_response').size() / len(frame)
    expected = pd.Series([0.5, 0.5], index=['no', 'yes'])
    assert all(proportions == expected)

def test_all_trials_are_reached(trials):
    frame = trials.to_dataframe()
    expected_num_blocks = len(frame.block.unique())
    blocks = list(trials.iter_blocks())
    for b in blocks:
        print(b[0]['block'])
    actual_num_blocks = len(list(trials.iter_blocks()))
    assert actual_num_blocks == expected_num_blocks
