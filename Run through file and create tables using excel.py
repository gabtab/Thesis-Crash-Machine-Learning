import time
import openpyxl
import pandas as pd

#parse excel file into dataframe used for looping through sensor names data
wb = pd.ExcelFile('D:/College/Proposal 2/Crash data/Test Table with VCG for File Creation.xlsx')
df = wb.parse("Sheet1")
sensors = wb.parse("Sheet2")
#select the folder where all of the unzipped files are saved
pathextract = 'D:/College/Proposal 2/Crash data/All data zipped/Full files/'
pathinput = 'D:/College/Proposal 2/Crash data/Sensor Data/masterfile.csv'
errorpath = 'D:/College/Proposal 2/Crash data/Sensor Data/errors.csv'
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
#df1 = df.iloc[0:200,:]
############################################################################################
masterdata = pd.DataFrame()
errordata = pd.DataFrame()
k = 0
errors = []    
for j, row in enumerate(sensors.itertuples(),1):
                
                
    for i, row in enumerate(df.itertuples(),1):
        
        try:
            if df.iloc[i-1,5] == sensors.iloc[j-1,0]:
                newdata = pd.read_csv(pathextract +row.search, sep = "\t", header=None)
                newdata.columns = ["Time", "Force"]
                newdata['TSTNO'] = df.iloc[i-1, 0]
                newdata[' CURNO'] = df.iloc[i-1, 2]
                #need to then combine the data with the previous data in one dataframe
                if masterdata.empty:   
                    #masterdata = pd.DataFrame()
                    masterdata = newdata
                else:
                    masterdata = masterdata.append(newdata)
                    print "No. " , i , " Test " , df.iloc[i-1,0] , " Curno " , df.iloc[i-1,2]
            if (len(masterdata.index) / 1000000) == 1 :
                k = k + 1
                masterdata.to_csv(pathinput + str(k) )
                print "the number of files saved was " , len(masterdata.index)
                masterdata = masterdata[0:0]   
                
        except IOError as err:
                
                errors.append(err)
                pass         
masterdata.to_csv(pathinput + str(k) + str(len(masterdata.index)) )                   
