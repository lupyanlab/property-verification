"""Tasks for property verification project.

Usage::

    $ invoke --list
    $ invoke [task_name]
"""
from invoke import task, run

@task
def install_r_package_data():
    """Install the propertyverificationdata R package."""
    # watch quotes!
    r_command = 'devtools::install("propertyverificationdata")'
    run("Rscript -e '{}'".format(r_command))
