#!/usr/bin/env python
import logging
from UserDict import UserDict
from UserList import UserList
import webbrowser

import yaml
import pandas as pd
from numpy import random
from unipath import Path

from psychopy import prefs
prefs.general['audioLib'] = ['pyo']
from psychopy import visual, core, event, sound

from labtools.psychopy_helper import get_subj_info, load_sounds
from labtools.dynamic_mask import DynamicMask
from labtools.trials_functions import (counterbalance, expand, extend,
                                       add_block, smart_shuffle)


class Participant(UserDict):
    """Manage participant data.

    >>> participant = Participant(subj_id=100, seed=539,
                                  _order=['subj_id', 'seed'])
    >>> participants.data_file
    # data/100.csv
    >>> participant.write_header(['trial', 'is_correct'])
    # writes "subj_id,seed,trial,is_correct\n" to the data file
    # and saves input as the order of columns in the output
    >>> participant.write_trial({'trial': 1, 'is_correct': 1})
    # writes "100,539,1,1\n" to the data file
    """
    DATA_DIR = 'data'
    DATA_DELIMITER = ','

    def __init__(self, **kwargs):
        """Standard dict constructor.

        Saves _order if provided. Raises an AssertionError if _order
        isn't exhaustive of kwargs.
        """
        self._data_file = None
        self._order = kwargs.pop('_order', kwargs.keys())

        correct_len = len(self._order) == len(kwargs)
        kwargs_in_order = all([kwg in self._order for kwg in kwargs])
        assert correct_len & kwargs_in_order, "_order doesn't match kwargs"

        # standard practice for extending UserDict
        self.data = dict(**kwargs)

    def keys(self):
        return self._order

    @property
    def data_file(self):
        if not Path(self.DATA_DIR).exists():
            Path(self.DATA_DIR).mkdir()

        if not self._data_file:
            data_file_name = '{subj_id}.csv'.format(**self)
            self._data_file = Path(self.DATA_DIR, data_file_name)
        return self._data_file

    def write_header(self, trial_col_names):
        """Writes the names of the columns and saves the order."""
        self._col_names = self._order + trial_col_names
        self._write_line(self._col_names, mode='w')

    def write_trial(self, trial):
        assert self._col_names, 'write header first to save column order'
        trial_data = dict(self)
        trial_data.update(trial)
        ordered_data = [trial_data[col] for col in self._col_names]
        self._write_line(ordered_data)

    def _write_line(self, row, mode='a'):
        # Join a list of data and write it to the participant's data file.
        row_str = self.DATA_DELIMITER.join([str(item) for item in row])
        with open(self.data_file, mode) as f:
            f.write(row_str + '\n')


class Trials(UserList):
    STIM_DIR = Path('stimuli')
    COLUMNS = [
        # Trial columns
        'block',
        'block_type',
        'trial',

        # Stimuli columns
        'proposition_id',
        'feat_type',
        'question',
        'cue',
        'cue_file',
        'mask_type',
        'correct_response',

        # Response columns
        'response',
        'rt',
        'is_correct',
    ]

    @classmethod
    def make(cls, **kwargs):
        """Create a list of trials.

        Each trial is a dict with values for all keys in self.COLUMNS.
        """
        # make convenience copies of class defaults
        stim_dir = cls.STIM_DIR
        columns = cls.COLUMNS

        settings = kwargs.copy()
        seed = settings.get('seed')
        try:
            seed = int(seed)
        except (TypeError, ValueError):
            logging.info('seed was None or couldn\'t be converted to int')
            seed = None
        prng = random.RandomState(seed)

        # Read in the trials (same for all participants)
        trials = pd.read_csv(Path(stim_dir, 'propositions.csv'))

        # Randomly assign mask trials
        trials['mask_type'] = prng.choice(['mask', 'nomask'], size=len(trials))

        # Choose a version of each cue at random
        all_cues = Path(stim_dir, 'cues').listdir('*.wav', names_only=True)
        def pick_cue_file(cue):
            options = [c for c in all_cues if c.find(cue) == 0]
            return prng.choice(options)
        trials['cue_file'] = trials.cue.apply(pick_cue_file)

        # Add columns for response variables
        for col in ['response', 'rt', 'is_correct']:
            trials[col] = ''

        # Add practice trials
        num_practice = 6
        practice_ix = prng.choice(trials.index, num_practice, replace=False)
        practice_trials = trials.ix[practice_ix, ]
        practice_trials['block'] = 0
        practice_trials['block_type'] = 'practice'
        trials.drop(practice_ix, inplace=True)

        # Finishing touches: block trials and shuffle
        num_blocks = 4
        trials_per_block = (len(trials)/num_blocks) + 1
        trials = add_block(trials, trials_per_block, name='block', start=1,
                           groupby='cue', seed=seed)
        trials = smart_shuffle(trials, col='cue', block='block', seed=seed)
        trials['block_type'] = 'test'

        # Merge practice trials
        trials = pd.concat([practice_trials, trials], ignore_index=True)

        # Label trial
        trials['trial'] = range(1, len(trials)+1)

        # Reorder columns
        trials = trials[columns]

        return cls(trials.to_dict('record'))

    def to_dataframe(self):
        return pd.DataFrame.from_records(self)[self.COLUMNS]

    def write(self, trials_csv):
        frame = self.to_dataframe()
        frame.to_csv(trials_csv, index=False)

    @classmethod
    def load(cls, trials_csv):
        trials = pd.read_csv(trials_csv)
        assert set(trials.columns) == set(cls.COLUMNS)
        return cls(trials.to_dict('records'))

    def iter_blocks(self, key='block'):
        """Yield blocks of trials."""
        block = self[0][key]
        trials_in_block = []
        for trial in self:
            if trial[key] == block:
                trials_in_block.append(trial)
            else:
                yield trials_in_block
                block = trial[key]
                trials_in_block = []
        # end of trials, so yield the last block
        yield trials_in_block


class Experiment(object):
    STIM_DIR = Path('stimuli')

    def __init__(self, settings_yaml, texts_yaml):
        """Create the window and stimuli using the settings provided."""
        with open(settings_yaml, 'r') as f:
            exp_info = yaml.load(f)

        self.waits = exp_info.pop('waits')
        self.response_keys = exp_info.pop('response_keys')
        self.survey_url = exp_info.pop('survey_url')

        with open(texts_yaml, 'r') as f:
            self.texts = yaml.load(f)

        self.win = visual.Window(
            fullscr=True,
            units='pix',
            allowGUI=False,
            winType='pyglet',
            color=[.6, .6, .6],
        )

        fixation_point = [0, 100]
        text_kwargs = dict(
            win=self.win,
            pos=fixation_point,
            height=40,
            font='Consolas',
            color='black',
            wrapWidth=int(self.win.size[0] * 0.8),
        )
        self.ready = visual.TextStim(text='READY', **text_kwargs)
        self.question = visual.TextStim(**text_kwargs)
        self.prompt = visual.TextStim(text='?', **text_kwargs)
        self.prompt.setHeight(100)  # increase font size from default

        cues_dir = Path(self.STIM_DIR, 'cues')
        self.cues = load_sounds(cues_dir, include_ext=True)

        mask_kwargs = dict(win=self.win, pos=fixation_point, size=[500, 500])
        self.mask = DynamicMask(**mask_kwargs)

        feedback_dir = Path(self.STIM_DIR, 'feedback')
        self.feedback = {}
        self.feedback[0] = sound.Sound(Path(feedback_dir, 'buzz.wav'))
        self.feedback[1] = sound.Sound(Path(feedback_dir, 'bleep.wav'))

        self.timer = core.Clock()

    def run_trial(self, trial):
        """Run a trial using a dict of settings."""
        self.question.setText(trial['question'])

        cue = self.cues[trial['cue_file']]
        cue_dur = cue.getDuration()

        show_mask = (trial['mask_type'] == 'mask')

        ############################
        # BEGIN TRIAL PRESENTATION #
        ############################

        # Show READY prompt
        self.timer.reset()
        self.ready.draw()
        self.win.flip()
        core.wait(self.waits['ready_duration'])

        # Delay between READY offset and question onset
        self.win.flip()
        core.wait(self.waits['ready_offset_to_question_onset'])

        # Show question
        self.question.draw()
        self.win.flip()
        core.wait(self.waits['question_duration'])

        # Delay between question offset and mask onset
        self.win.flip()
        core.wait(self.waits['question_offset_to_mask_interval_onset'])

        # If it's a mask trial, show the mask
        self.timer.reset()
        while self.timer.getTime() < self.waits['mask_interval_onset_to_cue_onset']:
            if show_mask:
                self.mask.draw()
            self.win.flip()
            core.wait(self.waits['mask_refresh'])

        # Play the cue and collect the response
        self.timer.reset()
        event.clearEvents()
        cue.play()
        while self.timer.getTime() < self.waits['max_wait']:
            if show_mask:
                self.mask.draw()
            self.prompt.draw()
            self.win.flip()

            response = event.getKeys(
                keyList=self.response_keys.keys(),
                timeStamped=self.timer,
            )
            if response:
                cue.stop()
                break

        self.win.flip()

        ##########################
        # END TRIAL PRESENTATION #
        ##########################

        # Determine the response type
        try:
            key, rt = response[0]
        except IndexError:
            rt = self.waits['max_wait']
            response = 'timeout'
        else:
            response = self.response_keys[key]

        # Evaluate the response
        is_correct = int(response == trial['correct_response'])
        if trial['block_type'] == 'practice':
            self.feedback[is_correct].play()

        # Update the data for this trial
        trial['response'] = response
        trial['rt'] = rt * 1000.0
        trial['is_correct'] = is_correct

        if response == 'timeout':
            # pause the experiment
            self.show_text('timeout')
        else:
            remaining_time = self.waits['max_wait'] - rt
            core.wait(remaining_time + self.waits['inter_trial_interval'])

        return trial

    def show_text(self, label):
        if label in ['instructions', 'end_of_practice']:
            self._show_text_block(label)
        else:
            text_str = self.texts[label]
            text_stim = visual.TextStim(
                win=self.win,
                text=text_str,
                height=30,
                wrapWidth=self.win.size[0] * 0.8,
                color='black',
                font='Consolas',
            )
            text_stim.draw()
            self.win.flip()
            event.waitKeys(keyList=['space', ])

    def _show_text_block(self, label):
        text_block = sorted(self.texts[label].items())

        text_kwargs = dict(
            win=self.win,
            wrapWidth=self.win.size[0] * 0.8,
            color='black',
            font='Consolas',
        )
        main = visual.TextStim(pos=[0, 200], **text_kwargs)
        example = visual.TextStim(pos=[0, -50], **text_kwargs)
        example.setHeight(30)

        for i, block in text_block:
            tag = block.pop('tag', None)
            advance_keys = [block.get('advance', 'space'), 'q']

            if 'main' in block:
                main.setText(block['main'])

                if tag == 'title':
                    main.setHeight(50)
                else:
                    main.setHeight(25)

                main.draw()

            if 'example' in block:
                example.setText(block['example'])
                example.draw()

            if tag == 'mask':
                img_path = Path('labtools', 'dynamic_mask', 'colored_1.png')
                mask = visual.ImageStim(self.win, str(img_path), pos=[0, -100])
                mask.draw()

            self.win.flip()
            key = event.waitKeys(keyList=advance_keys)[0]

            if key == 'q':
                core.quit()

            if key in ['up', 'down']:
                self.feedback[1].play()

def create_experiment():
    return Experiment('settings.yaml', 'texts.yaml')

def run_experiment():
    participant_data = get_subj_info(
        'gui.yaml',
        # check_exists is a simple function to determine if the data file
        # exists, provided subj_info data. Here the calculation of the data
        # file path is provided through the participant class.
        check_exists=lambda subj_info:
            Participant(**subj_info).data_file.exists()
    )

    participant = Participant(**participant_data)
    trials = Trials.make(**participant)

    # Start of experiment
    experiment = create_experiment()
    experiment.show_text('instructions')

    participant.write_header(trials.COLUMNS)

    for block in trials.iter_blocks():
        block_type = block[0]['block_type']

        for trial in block:
            trial_data = experiment.run_trial(trial)
            participant.write_trial(trial_data)

        if block_type == 'practice':
            experiment.show_text('end_of_practice')
        else:
            experiment.show_text('break')

    experiment.show_text('end_of_experiment')
    webbrowser.open(experiment.survey_url.format(**participant))


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument(
        'command',
        choices=['experiment', 'make', 'texts', 'trial', 'survey'],
        nargs='?',
        default='experiment',
    )
    parser.add_argument('--output', '-o', help='Name of output file')
    parser.add_argument('--seed', '-s',
                        help='Seed for random number generator')
    parser.add_argument('--trial-index', '-i', default=0, type=int,
                        help='Trial index to run from sample_trials.csv')
    parser.add_argument('--labels', '-l', nargs='+')

    args = parser.parse_args()

    if args.command == 'make':
        seed = args.seed or random.randint(100)
        output = args.output or 'sample_trials.csv'
        print "Making trials with seed %s: %s" % (seed, output)
        trials = Trials.make(seed=seed)
        trials.write(output)
    elif args.command == 'texts':
        experiment = create_experiment()
        for label in args.labels:
            experiment.show_text(label)
    elif args.command == 'trial':
        trials = Trials.load('sample_trials.csv')
        experiment = create_experiment()
        trial_data = experiment.run_trial(trials[args.trial_index])
        import pprint
        pprint.pprint(trial_data)
    elif args.command == 'survey':
        experiment = create_experiment()
        survey_url = experiment.survey_url.format(
            subj_id='TEST_SUBJ',
            computer='TEST_COMPUTER',
        )
        webbrowser.open(survey_url)
    else:
        run_experiment()
