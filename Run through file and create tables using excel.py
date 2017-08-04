
import time
import openpyxl
import pandas as pd

wb = pd.ExcelFile('D:/College/Proposal 2/Crash data/Test Table with VCG for File Creation.xlsx')

df = wb.parse("Sheet1")

pathextract = 'D:/College/Proposal 2/Crash data/All data zipped/Full files/'
pathinput = 'D:/College/Proposal 2/Crash data/Sensor Data'

df['testtext'] = df['TSTNO'].astype(str)
df['testtext'] = df['testtext'].apply(lambda x: x.zfill(5))
df['testtext'] = 'v' + df['testtext'].astype(str)

df['curvetext'] = df[' CURNO'].astype(str)
df['curvetext'] = df['curvetext'].apply(lambda x: x.zfill(3))
df['curvetext'] = '.' + df['curvetext'].astype(str)

df['search'] = df['testtext']+df['curvetext']
del df['testtext']
del df['curvetext']

searchfile = open(pathextract +'v00006.001', "r")

preparation['preparation'] = searchfile

        search the pathextract folder for this file
        create an excel file in pathinput called preparation with all the data in it tstno and curvo as columns
        convert to flat file
        combine with the sensordata file
        if its equal to 
        linknumber = str(cell.value)