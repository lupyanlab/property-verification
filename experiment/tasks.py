from invoke import task
import json
import gspread
import pandas
from oauth2client.client import OAuth2Credentials as Credentials

def get_worksheet(workbook, worksheet):
    json_key = json.load(open('drive-api-creds.json'))
    scope = ['https://spreadsheets.google.com/feeds', ]

    credentials = Credentials(json_key['client_email'],
                              json_key['private_key'],
                              scope)
    gc = gspread.authorize(credentials)

    # get data from google drive
    workbook = gc.open(workbook)
    worksheet = workbook.worksheet(worksheet)
    records = worksheet.get_all_records()

    # massage it into a dataframe
    frame = pandas.DataFrame.from_records(records)
    header = worksheet.row_values(1)[:len(frame.columns)]
    frame = frame[header]

    return frame

@task
def get_subj_info():
    subj_info = get_worksheet(workbook='MWP Subject Info',
                              worksheet='property-verification-spring-2016')
    subj_info.to_csv('subj_info.csv', index=False)

@task
def get_google_survey():
    survey = get_worksheet(workbook='property-verification-responses',
                           worksheet='Form Responses 1')
    survey.to_csv('survey.csv', index=False)
