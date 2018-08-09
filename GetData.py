import os, sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath('GetData.py'))))
from config import v_user, v_password


import pandas as pd
import vertica_python

#import matplotlib.pyplot as plt
conn_info = {'host': 'devlx207',
             'port': 5433,
             'user': v_user,
             'password': v_password,
             'database': 'advana',
             # 10 minutes timeout on queries
             'read_timeout': 600,
             # default throw error on invalid UTF-8 results
             'unicode_error': 'strict',
             # SSL is disabled by default
             'ssl': True}
             
# simple connection, with manual close
connection = vertica_python.connect(**conn_info)
cur = connection.cursor()

# Open and read the file as a single buffer
fd=open('Stage1/TrainDataQueryS1.sql','r')
#fd=open('VarExp.sql','r')
sqlQuery = fd.read()
fd.close()

PolData = pd.read_sql(sqlQuery, connection)
#PolData.to_csv("~/Documents/Placement.Rate/ExpVar.csv")

PolData.to_csv("~/Documents/Placement.Rate/PRTrain.csv")
