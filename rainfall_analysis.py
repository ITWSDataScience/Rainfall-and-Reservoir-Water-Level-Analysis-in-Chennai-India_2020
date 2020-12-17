import csv
import matplotlib.pyplot as plt
from datetime import datetime

filename = 'chennai_reservoir_rainfall.csv'
fields = [] 
Date = []
POONDI = []
CHOLAVARAM = []
REDHILLS = []
CHEMBARAMBAKKAM = []

with open(filename, 'r') as csvfile: 
    csvreader = csv.reader(csvfile) 
    fields = next(csvreader)
    for row in csvreader:
        for val in row:
            if not val:
                print("There is a Null value")
        date = datetime.strptime(row[0], '%d-%m-%Y').strftime('%Y-%m-%d')
        Date.append(date)
        POONDI.append(float(row[1]))
        CHOLAVARAM.append(float(row[2]))
        REDHILLS.append(float(row[3]))
        CHEMBARAMBAKKAM.append(float(row[4]))
  
po = []
cho = []
red = []
chem = []
cnt = 0
while cnt < len(Date) :
    if int(Date[cnt][:4]) % 4 == 0:
        po.append(sum(POONDI[cnt:cnt+366]))
        cho.append(sum(CHOLAVARAM[cnt:cnt+366]))
        red.append(sum(REDHILLS[cnt:cnt+366]))
        chem.append(sum(CHEMBARAMBAKKAM[cnt:cnt+366]))
        cnt += 366
    else:
        po.append(sum(POONDI[cnt:cnt+365]))
        cho.append(sum(CHOLAVARAM[cnt:cnt+365]))
        red.append(sum(REDHILLS[cnt:cnt+365]))
        chem.append(sum(CHEMBARAMBAKKAM[cnt:cnt+365]))
        cnt += 365

d = []
for i in range(len(po)):
    d.append(2004+i)

#rainfall on each reservoir over repective years

plt.bar(d, po)
plt.locator_params(axis="x", nbins=17)
plt.xlabel("Year")
plt.ylabel("mm (unit for measuring rain)")
plt.title("Chennai Reservoir Rianfall for POONDI")
plt.show()

plt.bar(d, cho)
plt.locator_params(axis="x", nbins=17)
plt.xlabel("Year")
plt.ylabel("mm (unit for measuring rain)")
plt.title("Chennai Reservoir Rianfall for CHOLAVARAM")
plt.show()

plt.bar(d, red)
plt.locator_params(axis="x", nbins=17)
plt.xlabel("Year")
plt.ylabel("mm (unit for measuring rain)")
plt.title("Chennai Reservoir Rianfall for REDHILLS")
plt.show()

plt.bar(d, chem)
plt.locator_params(axis="x", nbins=17)
plt.xlabel("Year")
plt.ylabel("mm (unit for measuring rain)")
plt.title("Chennai Reservoir Rianfall for CHEMBARAMBAKKAM")
plt.show()


total = []
for i in range(len(po)):
    t = po[i]+cho[i]+red[i]+chem[i]
    total.append(t)

#total rainfall over repective years

plt.bar(d, total)
plt.locator_params(axis="x", nbins=17)
plt.xlabel("Year")
plt.ylabel("Million Cubic Feet")
plt.title("Total Rainfall Over Respective Years")
plt.show()
