#!/usr/bin/env python
import argparse
import copy
import yaml
from UserDict import UserDict
from UserList import UserList
import webbrowser

import pandas as pd
from numpy import random
from unipath import Path

from psychopy import prefs

try:
    import pyo
except ImportError:
    print 'pyo not found!'

prefs.general['audioLib'] = ['pyo']
from psychopy import visual, core, event, sound

print 'initializing pyo to 48000'
sound.init(48000, buffer=128)
print 'Using %s(with %s) for sounds' % (sound.audioLib, sound.audioDriver)

from labtools.psychopy_helper import get_subj_info, load_sounds, load_images
from labtools.dynamic_mask import DynamicMask
from labtools.trials_functions import (counterbalance, expand, extend,
                                       add_block, smart_shuffle)


class Participant(UserDict):
    """ Store participant data and provide helper functions.

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
        """ Standard dict constructor.

        Saves _order if provided. Raises an AssertionError if _order
        isn't exhaustive of kwargs.
        """
        self._data_file = None
        self._order = kwargs.pop('_order', kwargs.keys())

        correct_len = len(self._order) == len(kwargs)
        kwargs_in_order = all([kwg in self._order for kwg in kwargs])
        assert correct_len & kwargs_in_order, "_order doesn't match kwargs"

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
        """ Writes the names of the columns and saves the order. """
        self._col_names = self._order + trial_col_names
        self._write_line(self.DATA_DELIMITER.join(self._col_names))

    def write_trial(self, trial):
        assert self._col_names, 'write header first to save column order'
        trial_data = dict(self)
        trial_data.update(trial)
        row_data = [str(trial_data[key]) for key in self._col_names]
        self._write_line(self.DATA_DELIMITER.join(row_data))

    def _write_line(self, row):
        with open(self.data_file, 'a') as f:
            f.write(row + '\n')


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
        'mask_type',
        'correct_response',

        # Response columns
        'response',
        'rt',
        'is_correct',
    ]
    DEFAULTS = dict(ratio_yes_correct_responses=0.75)

    @classmethod
    def make(cls, **kwargs):
        """ Create a list of trials.

        Each trial is a dict with values for all keys in self.COLUMNS.
        """
        settings = dict(cls.DEFAULTS)
        settings.update(kwargs)

        seed = settings.get('seed')
        try:
            seed = int(seed)
        except TypeError, ValueError:
            seed = None
        prng = random.RandomState(seed)

        # Balance within subject variables
        trials = counterbalance({'feat_type': ['visual', 'nonvisual'],
                                 'mask_type': ['mask', 'nomask']})
        trials = expand(trials, name='correct_response', values=['yes', 'no'],
                        ratio=settings['ratio_yes_correct_responses'],
                        seed=seed)

        # Extend the trials to final length
        trials = extend(trials, max_length=230)

        # Read proposition info
        propositions_csv = Path(cls.STIM_DIR, 'propositions.csv')
        propositions = pd.read_csv(propositions_csv)

        # Add cue
        categories = propositions.cue.unique()
        trials['cue'] = prng.choice(categories, len(trials), replace=True)

        # Create a copy of the propositions that will be modified in place
        # to remove the questions that have already been used.
        _propositions = propositions.copy()

        def determine_question(row):
            """Pick a proposition_id for this trial."""
            is_cue = (_propositions.cue == row['cue'])
            is_feat_type = (_propositions.feat_type == row['feat_type'])
            is_correct_response = (_propositions.correct_response ==
                                   row['correct_response'])

            valid_propositions = (is_cue & is_feat_type & is_correct_response)

            # If there are no questions remaining for this trial type,
            # pick a new cue and try again.
            if valid_propositions.sum() == 0:
                trials.ix[row.name, 'cue'] = prng.choice(categories)
                return determine_question(trials.ix[row.name, ])
            else:
                options = _propositions.ix[valid_propositions, ]
                selected_ix = prng.choice(options.index)
                selected_proposition_id = options.ix[selected_ix, 'proposition_id']
                _propositions.drop(selected_ix, inplace=True)

                return selected_proposition_id

        trials['proposition_id'] = trials.apply(determine_question, axis=1)

        # Merge in question info
        trials = trials.merge(propositions)

        # Add columns for response variables
        for col in ['response', 'rt', 'is_correct']:
            trials[col] = ''

        # Add practice trials
        num_practice = 8
        practice_ix = prng.choice(trials.index, num_practice)
        practice_trials = trials.ix[practice_ix, ]
        practice_trials['block'] = 0
        practice_trials['block_type'] = 'practice'
        trials.drop(practice_ix, inplace=True)

        # Finishing touches
        trials = add_block(trials, 50, name='block', start=1, groupby='cue',
                           seed=seed)
        trials = smart_shuffle(trials, col='cue', block='block', seed=seed)
        trials['block_type'] = 'test'

        # Merge practice trials
        trials = pd.concat([practice_trials, trials])

        # Label trial
        trials['trial'] = range(len(trials))

        # Reorcder columns
        trials = trials[cls.COLUMNS]

        return cls(trials.to_dict('record'))

    def to_dataframe(self):
        return pd.DataFrame.from_records(self)[self.COLUMNS]

    def write(self, trials_csv):
        frame = self.to_dataframe()
        frame.to_csv(trials_csv, index=False)

    @classmethod
    def load_trials(cls, trials_csv):
        trials = pd.read_csv(trials_csv)
        return cls(trials.to_dict('records'))

    def iter_blocks(self, key='block'):
        """ Yield blocks of trials. """
        block = self[0][key]
        trials_in_block = []
        for trial in self:
            if trial[key] == block:
                trials_in_block.append(trial)
            else:
                yield trials_in_block
                block = trial[key]
                trials_in_block = []


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

        text_kwargs = dict(
            win=self.win,
            pos=[0,100],
            height=40,
            font='Consolas',
            color='black',
            wrapWidth=int(self.win.size[0] * 0.8),
        )
        self.ready = visual.TextStim(text='READY', **text_kwargs)
        self.question = visual.TextStim(**text_kwargs)
        self.prompt = visual.TextStim(text='?', **text_kwargs)
        self.prompt.setHeight(100) # increase font size from default

        self.cues = load_sounds(Path(self.STIM_DIR, 'cues'))

        mask_kwargs = dict(win=self.win, size=[500, 500])
        self.mask = DynamicMask(**mask_kwargs)

        feedback_dir = Path(self.STIM_DIR, 'feedback')
        self.feedback = {}
        self.feedback[0] = sound.Sound(Path(feedback_dir, 'buzz.wav'))
        self.feedback[1] = sound.Sound(Path(feedback_dir, 'bleep.wav'))

        self.timer = core.Clock()

    def run_trial(self, trial):
        """Run a trial using a dict of settings."""
        self.question.setText(trial['question'])

        cue = self.cues[trial['cue']]
        cue_dur = question.getDuration()

        stims_during_cue = []
        if trial['mask_type'] == 'mask':
            stims_during_cue.append(self.mask)

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
        core.wait(self.waits['question_dur'])

        # Delay between question offset and cue onset
        self.win.flip()
        core.wait(self.waits['question_offset_to_cue_onset'])

        # Play cue (and show mask)
        self.timer.reset()
        cue.play()
        while cue_timer.getTime() < cue_dur:
            for stim in stims_during_cue:
                stim.draw()
            self.win.flip()
            core.wait(self.waits['mask_refresh'])

        # Show the prompt, start the RT timer, and collect the response
        self.prompt.draw()
        self.timer.reset()
        event.clearEvents()
        self.win.flip()
        response = event.waitKeys(
            maxWait=self.waits['max_wait'],
            keyList=self.resp_keys.keys(),
            timeStamped=self.timer,
        )

        self.win.flip()

        ##########################
        # END TRIAL PRESENTATION #
        ##########################

        # Determine the response type
        try:
            key, rt = response[0]
        except TypeError:
            rt = self.waits['max_wait']
            response = 'timeout'
        else:
            response = self.resp_keys[key]

        # Evaluate the response
        is_correct = int(response == trial['correct_response'])
        self.feedback[is_correct].play()

        # Update the data for this trial
        trial['response'] = response
        trial['rt'] = rt * 1000.0
        trial['is_correct'] = is_correct

        if response == 'timeout':
            self.show_timeout_screen()
        else:
            remaining_time = self.waits['max_wait'] - rt
            core.wait(remaining_time + self.waits['inter_trial_interval'])

        return trial

    def show_text(self, label):
        if label == 'instructions':
            self._show_instructions()
        else:
            text = visual.TextStim(
                win=self.win,
                text=self.texts['label'],
                height=30,
                wrapWidth=self.win.size[0] * 0.8,
                color='black',
                font='Consolas',
            )
            text.draw()
            self.win.flip()
            event.waitKeys(keyList=['space', ])

    def _show_instructions(self):
        instructions = sorted(self.texts['instructions'].items())

        text_kwargs = dict(
            win=self.win,
            wrapWidth=self.win.size[0] * 0.8,
            color='black',
            font='Consolas',
        )
        main = visual.TextStim(pos=[0, 200], **text_kwargs)
        example = visual.TextStim(pos=[0, -50], **text_kwargs)
        example.setHeight(30)

        for i, block in instructions:
            tag = block.pop('tag', None)
            advance_keys = [block.get('advance', 'space'), 'q']

            if 'main' in block:
                main.setText(block['main'])

                if tag == 'title':
                    main.setHeight(50)
                else:
                    main.setHeight(20)

                main.draw()

            if 'example' in block:
                example.setText(block['example'])
                example.draw()

            # if tag == 'mask':
            #     img_path = str(Path('stimuli', 'dynamic_mask', 'colored_1.png'))
            #     mask = visual.ImageStim(self.win, img_path, pos=[0, -100])
            #     mask.draw()

            self.win.flip()
            key = event.waitKeys(keyList=advance_keys)[0]

            if key == 'q':
                core.quit()

            if key in ['y', 'n']:
                self.feedback[1].play()


def main():
    participant_data = get_subj_info(
        'gui.yaml',
        # check_exists is a simple function to determine if the data file
        # exists, provided subj_info data. Here it's used to check for
        # uniqueness in subj_ids when getting info from gui.
        check_exists=lambda subj_info:
            Participant(**subj_info).data_file.exists()
    )

    participant = Participant(**participant_data)
    trials = Trials.make(**participant)

    # Start of experiment
    experiment = Experiment('settings.yaml', 'texts.yaml')
    experiment.show_instructions()

    participant.write_header(trials.COLUMNS)

    for block in trials.iter_blocks():
        block_type = block[0]['block_type']

        for trial in block:
            trial_data = experiment.run_trial(trial)
            participant.write_trial(trial_data)

        if block_type == 'practice':
            experiment.show_end_of_practice_screen()
        else:
            experiment.show_break_screen()

    experiment.show_end_of_experiment_screen()
    webbrowser.open(experiment.survey_url.format(**participant))


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument(
        'command',
        choices=['experiment', 'make', 'instructions', 'trial', 'survey'],
        nargs='?',
        default='experiment',
    )
    parser.add_argument('--output', '-o', help='Name of output file')
    parser.add_argument('--seed', '-s', help='Seed for random number generator')
    parser.add_argument('--trial-index', '-i', default=0, type=int,
                        help='Trial index to run from sample_trials.csv')

    args = parser.parse_args()

    if args.command == 'make':
        seed = args.seed or random.randint(100)
        output = args.output or 'sample_trials.csv'
        print "Making trials with seed %s: %s" % (seed, output)
        trials = Trials.make(seed=seed)
        trials.write(output)
    elif args.command == 'instructions':
        experiment = Experiment('settings.yaml', 'texts.yaml')
        experiment._show_instructions()
    elif args.command == 'trial':
        experiment = Experiment('settings.yaml', 'texts.yaml')
        trials = Trials.load('sample_trials.csv')
        trial_data = experiment.run_trial(trials[args.trial_index])
        import pprint
        pprint.pprint(trial_data)
    elif args.command == 'survey':
        experiment = Experiment('settings.yaml', 'texts.yaml')
        survey_url = experiment.survey_url.format(
            subj_id='TEST_SUBJ',
            computer='TEST_COMPUTER',
        )
        webbrowser.open(survey_url)
    else:
        main()
