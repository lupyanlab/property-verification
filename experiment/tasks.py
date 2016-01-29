from invoke import task
import json
import gspread
import pandas
from oauth2client.client import SignedJwtAssertionCredentials as Credentials

def get_worksheet(workbook, worksheet):
    json_key = json.load(open('drive-api-creds.json'))
    scope = ['https://spreadsheets.google.com/feeds', ]

    credentials = Credentials(json_key['client_email'],
                              json_key['private_key'].encode(),
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
def subj_info():
    subj_info = get_worksheet(workbook='MWP Subject Info',
                              worksheet='property-verification-third-run')
    subj_info.to_csv('subj_info.csv', index=False)

@task
def survey():
    survey = get_worksheet(workbook='property-verification-responses',
                           worksheet='Form Responses 1')
    survey.to_csv('survey.csv', index=False)
