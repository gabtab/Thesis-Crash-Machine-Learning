import time
import openpyxl
import pandas as pd

#parse excel file into dataframe used for looping through sensor names data
wb = pd.ExcelFile('D:/College/Proposal 2/Crash data/Test Table with VCG for File Creation.xlsx')
df = wb.parse("Sheet1")
#select the folder where all of the unzipped files are saved
pathextract = 'D:/College/Proposal 2/Crash data/All data zipped/Full files/'
pathinput = 'D:/College/Proposal 2/Crash data/Sensor Data'
#manipulate the new data frame so the name of the file names in the data frame match match the name of the sensor data files
df['testtext'] = df['TSTNO'].astype(str)
df['testtext'] = df['testtext'].apply(lambda x: x.zfill(5))
df['testtext'] = 'v' + df['testtext'].astype(str)
df['curvetext'] = df[' CURNO'].astype(str)
df['curvetext'] = df['curvetext'].apply(lambda x: x.zfill(3))
df['curvetext'] = '.' + df['curvetext'].astype(str)
df['search'] = df['testtext']+df['curvetext']
del df['testtext']
del df['curvetext']

#loop through the files and create a dataframe with the columns time and force, the curno and the tstno

##for test ######CHANGE THE DATA FRAME BACK TO DF1 AT THE END #######################################################

df1 = df.iloc[0:3,:]
############################################################################################
for i, row in enumerate(df1.itertuples(),1):
    
    newdata = pd.read_csv(pathextract +row.search, sep = "\t", header=None)
    newdata.columns = ["Time", "Force"]
    newdata['TSTNO'] = df1.iloc[i-1, 0]
    newdata[' CURNO'] = df1.iloc[i-1, 2]
    #need to then combine the data with the previous data in one dataframe
    if i == 1:   
        masterdata = pd.DataFrame()
        masterdata = newdata
    else:
        masterdata = masterdata.append(newdata)
        
    writer = pd.ExcelWriter('output.xlsx')
    masterdata.to_excel(writer,'Sheet1')
    writer.save()