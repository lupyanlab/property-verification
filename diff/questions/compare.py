import pandas as pd

propositions = pd.read_csv('propositions.csv')
questions2 = pd.read_csv('questions2.csv')

# Rename questions2 columns
questions2.rename(columns={
    'ftype': 'feat_type',
    'qid': 'question_id',
    'response': 'correct_response',
}, inplace=True)

# Create proposition_id column
def concat_proposition_id(row):
    concat_cols = ['cue', 'feat_type', 'correct_response', 'question_id']
    return ':'.join([str(getattr(row, col)) for col in concat_cols])
questions2['proposition_id'] = questions2.apply(concat_proposition_id, axis=1)

# The proposition_id column of the questions2 data is not unique
print "number of questions", len(questions2)
print "number of unique proposition ids",\
      len(questions2.proposition_id.unique())

is_duplicated_proposition_id = questions2.duplicated(
    subset='proposition_id',
    keep=False,
)
duplicated_propositions = questions2[is_duplicated_proposition_id]
