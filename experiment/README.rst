Property Verification Experiment
================================

Installation
------------

1. If you have Canopy installed and activated, you can create a virtualenv to
   keep the requirements for this experiment separate from the requirements of
   other experiments::
      
        # creates a directory "name" to contain the venv
        $ canopy_cli venv [name]

   If you want to save some space for the bigger packages (numpy, scipy) you
   can provide the `--system-site-packages` flag, or `-s` for short::

        # creates a smaller directory that will symlink to system-site-packages
        # when it can
        $ canopy_cli venv -s [name]

1. Activate the virtualenv and install the required packages::

        $ source [name]/bin/activate
        ([name]) $ pip install -r requirements.txt

1. Run the experiment::
        
        ([name]) $ python run.py
