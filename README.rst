Property verification
=====================

This cognitive science research project intends to determine the extent to
which our knowledge of what things look like is represented in long-term memory
in a visual format. If visual knowledge is represented using visual mechanisms,
then visual interference might disrupt one's ability to answer even
simple yes or no questions about what things look like.

This repo includes the materials, methods, and results of our experiments
designed to address these questions. The experiments are run using python, and
the analyses are done in R. The results of the experiments can be downloaded as
an R package from this repo.

To run the experiment using python, you need to clone the repo and install
the required python packages. It is recommended that you set up a virtualenv
to keep the python packages for this experiment isolated from the rest of your
system::

    $ virtualenv --python=python2 ~/.venvs/property
    $ source ~/.venvs/property/bin/activate
    (property)$ git clone https://github.com/lupyanlab/property-verification.git
    (property)$ cd property-verification/experiment
    (property)$ pip install -r requirements.txt
    (property)$ python run.py  # runs the question first experiment

To download the data in this repo as an R package, you need to have the
devtools package installed, and then you can run this command in R to
download the data.

    > devtools::install_github("lupyanlab", "property-verification",
                               subdir = "propertyverificationdata")
    > library(propertyverificationdata)
    > data("question_first") # Load the data for the question first experiments

For our interpretation of the results, see the reports directory, or the
OSF page for this research at <osf.io/gctbx>. The manuscript reporting our
initial findings is under review.
