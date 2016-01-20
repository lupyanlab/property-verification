#!/usr/bin/env python
import pandas as pd

if __name__ == '__main__':
    brm = pd.read_table('../../experiment/stimuli/archive/questions' + \
                        '/mcrae_et_al/McRae-BRM-InPress/CONCS_brm.txt')
    visual_columns = ['Num_Vis_Mot', 'Num_VisF&S', 'Num_Sound']
    brm['prop_visual'] = brm[visual_columns].sum(axis=1) / brm.Num_Feats_No_Tax
    
    frame = pd.read_csv('feature_norms.csv')
    
    to_merge = brm[['Concept', 'prop_visual']].rename(
        columns = {'Concept':'cue'}
    )
    result = frame.merge(to_merge, how = 'left')
    result.to_csv('feature_norms_merged.csv')
