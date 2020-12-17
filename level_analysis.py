import csv
import matplotlib.pyplot as plt
from datetime import datetime

filename = 'chennai_reservoir_levels.csv'
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

#each reservoir's levels by itself

plt.bar(d, po)
plt.locator_params(axis="x", nbins=17)
plt.xlabel("Year")
plt.ylabel("Million Cubic Feet")
plt.title("Chennai Reservoir Level for POONDI")
plt.show()

plt.bar(d, cho)
plt.locator_params(axis="x", nbins=17)
plt.xlabel("Year")
plt.ylabel("Million Cubic Feet")
plt.title("Chennai Reservoir Level for CHOLAVARAM")
plt.show()

plt.bar(d, red)
plt.locator_params(axis="x", nbins=17)
plt.xlabel("Year")
plt.ylabel("Million Cubic Feet")
plt.title("Chennai Reservoir Level for REDHILLS")
plt.show()

plt.bar(d, chem)
plt.locator_params(axis="x", nbins=17)
plt.xlabel("Year")
plt.ylabel("Million Cubic Feet")
plt.title("Chennai Reservoir Level for CHEMBARAMBAKKAM")
plt.show()


total = []
for i in range(len(po)):
    t = po[i]+cho[i]+red[i]+chem[i]
    total.append(t)

#total levels
    
plt.bar(d, total)
plt.locator_params(axis="x", nbins=17)
plt.xlabel("Year")
plt.ylabel("Million Cubic Feet")
plt.title("Chennai Reservoirs Total Levels")
plt.show()


#each reservoir's levels vs. total levels

plt.bar(d, total, color='skyblue', label='Total levels')
plt.bar(d, po, color='coral', label='Poondi levels')
plt.locator_params(axis="x", nbins=17)
plt.xlabel("Year")
plt.ylabel("Million Cubic Feet")
plt.title("Poondi Levels vs. Total")
plt.legend()
plt.show()

plt.bar(d, total, color='skyblue', label='Total levels')
plt.bar(d, cho, color='coral', label='Cholavaram levels')
plt.locator_params(axis="x", nbins=17)
plt.xlabel("Year")
plt.ylabel("Million Cubic Feet")
plt.title("Cholavaram Levels vs. Total")
plt.legend()
plt.show()

plt.bar(d, total, color='skyblue', label='Total levels')
plt.bar(d, red, color='coral', label='Redhills levels')
plt.locator_params(axis="x", nbins=17)
plt.xlabel("Year")
plt.ylabel("Million Cubic Feet")
plt.title("Redhills Levels vs. Total")
plt.legend()
plt.show()

plt.bar(d, total, color='skyblue', label='Total levels')
plt.bar(d, chem, color='coral', label='Chembarambakkam levels')
plt.locator_params(axis="x", nbins=17)
plt.xlabel("Year")
plt.ylabel("Million Cubic Feet")
plt.title("Chembarambakkam Levels vs. Total")
plt.legend()
plt.show()


#population growth over year from 2004-2020

p = [7286857,7476986,7672348,7872815,
     8078807,8289599,8506193,8728447,
     8956508,9190528,9430663,9677072,
     9929919,10189373,10455606,10711243,10971108]

plt.scatter(d, p)
plt.plot(d, p)
plt.locator_params(axis="x", nbins=17)
plt.xlabel("Year")
plt.ylabel("Population")
plt.title("Population Growth")
plt.show()


#water usage supply from the four reservoirs

plt.bar(d, red, label='Redhills')
plt.bar(d, chem, label='Chembarambakkam')
plt.bar(d, po, label='Poondi')
plt.bar(d, cho, label='Cholavaram')
plt.locator_params(axis="x", nbins=17)
plt.xlabel("Year")
plt.ylabel("Million Cubic Feet")
plt.title("Water Usage For Each Reservoirs")
plt.legend()
plt.show()

#water usage per person

upp = []
for i in range(len(total)):
    tmp = total[i]/p[i]
    upp.append(float(tmp))

plt.plot(d, upp)
plt.locator_params(axis="x", nbins=17)
plt.xlabel("Year")
plt.ylabel("Million Cubic Feet")
plt.title("Water Usage Per Person Over Years")
plt.show()
