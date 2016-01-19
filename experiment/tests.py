import os
import yaml

import pytest

from run import Trials


@pytest.fixture(scope='module')
def trials(request):
    trials_csv = 'test_trials.csv'
    trials = Trials.make()
    trials.write(trials_csv)

    def remove_trials():
        os.remove(trials_csv)
    request.addfinalizer(remove_trials)

    return trials

def test_proposition_ids_are_unique(trials):
    frame = trials.to_dataframe()
    assert len(frame) == len(frame.proposition_id.unique())
    
