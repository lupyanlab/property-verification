#!/usr/bin/env python
"""Make sure the stimuli are as expected for running the experiment.

Usage:
    $ invoke cue_info cue_stats
    # creates cue_info.csv and cue_stats.csv
"""
from __future__ import print_function

from invoke import task
from unipath import Path
from pandas import read_csv, DataFrame, concat

CUES_DIR = 'cues'
CUE_INFO_CSV = 'cue_info.csv'
CUE_STATS_CSV = 'cue_stats.csv'

@task
def cue_info():
    cues = Path(CUES_DIR).listdir('*.wav', names_only=True)
    cue_info = DataFrame({'cue_file': cues})
    filename_re = r'(?P<cue>[a-z]+)_(?P<version>\d)\.wav'
    filename_args = cue_info.cue_file.str.extract(filename_re)
    cue_info = concat([cue_info, filename_args], axis=1)
    cue_info.to_csv(CUE_INFO_CSV, index=False)

@task
def cue_stats():
    from psychopy import sound
    cues = Path(CUES_DIR).listdir('*.wav')  # with path
    cue_stats_records = []
    for cue in cues:
        # psychopy chokes on unipath.Path object
        sound_obj = sound.Sound(str(cue))
        cue_duration = (cue.name, sound_obj.getDuration())
        cue_stats_records.append(cue_duration)

    cue_stats = DataFrame.from_records(
        cue_stats_records,
        columns=['cue_file', 'duration'],
    )

    cue_stats.to_csv(CUE_STATS_CSV, index=False)

@task(cue_info)
def verify():
    _verify_cue_files()
    _verify_proposition_uniqueness()

def _verify_cue_files():
    _status_line('verifying cue files')

    propositions = read_csv('propositions.csv')
    available_cues = read_csv(CUE_INFO_CSV).cue.tolist()

    any_missing = False
    msg = "cue {} not found in cue files"
    for proposition_category in propositions.cue.unique():
        if proposition_category not in available_cues:
            print(msg.format(proposition_category))
            any_missing = True

    if not any_missing:
        print("all cues present and accounted for")

def _verify_proposition_uniqueness():
    _status_line('verifying proposition uniqueness')

    propositions = read_csv('propositions.csv')
    duplicates = propositions.ix[
        propositions.duplicated('proposition_id'),
        'proposition_id'
    ]

    any_duplicates = False
    msg = "proposition_id {} appeared multiple times"
    for proposition_id in duplicates:
        print(msg.format(proposition_id))
        any_duplicates = True

    if not any_duplicates:
        print("all propositions are unique")

def _status_line(header):
    print('\n')
    print(header)
    print('-'*len(header))
