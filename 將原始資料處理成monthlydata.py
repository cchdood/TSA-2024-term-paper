# importing csv module
import csv

# csv file name
filenames = [
"101年-臺北市A1及A2類交通事故明細.csv",
"102年-臺北市A1及A2類交通事故明細.csv",
"103年-臺北市A1及A2類交通事故明細.csv",
"104年-臺北市A1及A2類交通事故明細.csv",
"105年-臺北市A1及A2類交通事故明細.csv",
"106年-臺北市A1及A2類交通事故明細.csv",
"107年-臺北市A1及A2類交通事故明細.csv",
"108年-臺北市A1及A2類交通事故明細.csv",
"109年-臺北市A1及A2類交通事故明細.csv",
"110年-臺北市A1及A2類交通事故明細.csv",
"111年-臺北市A1及A2類交通事故明細.csv",
"112年-臺北市A1及A2類交通事故明細.csv"
]

# initializing the titles and rows list

# reading csv file
X = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,]

for y in range(12):

    fields = []
    rows = []

    with open(filenames[y], 'r') as file:

        data = csv.reader(file)

        # extracting field names through first row
        fields = next(data)

        # extracting each data row one by one
        for row in data:
            rows.append(row)

        for row in rows[1:]:
            X[12 * y + int(row[1])-1] = X[12 * y + int(row[1])-1] + 1


import matplotlib.pyplot as plt
import numpy as np
import datetime


xpoints = np.array([datetime.datetime(2012 + (t // 12), (t % 12) + 1, 1).isoformat()[:7] for t in range(12 * 12)])
ypoints = np.array(X)

plt.plot(xpoints, ypoints)

#step = 6  # 每隔 6 個月
#plt.xticks(ticks=np.arange(0, len(xpoints), step), labels=xpoints[::step], rotation=45)  # 旋轉 45 度，方便閱讀
#plt.tight_layout()

plt.show()
