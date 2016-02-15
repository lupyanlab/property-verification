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
def create_loop_merge():
    """Create a spreadsheet to upload to Qualtrics as loop and merge data."""
    propositions = pd.read_csv('experiment/stimuli/propositions.csv')
    loop_merge = propositions.ix[propositions.correct_response == 'yes']
    loop_merge.sort_values(by=['cue', ], inplace=True)
    loop_merge['loop_merge_row'] = range(1, len(loop_merge)+1)
    loop_merge = loop_merge[['loop_merge_row', 'question', 'cue', 'proposition_id']]
    loop_merge.to_csv('individual_diffs/qualtrics/loop-merge.csv',
                      index=False)
