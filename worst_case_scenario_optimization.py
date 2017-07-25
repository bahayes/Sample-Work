# -*- coding: utf-8 -*-
"""
Created on Mon Nov 28 11:28:12 2016

@author: bahayes
"""

"""
    This program was created for the purpose of analyzing the worst case scenario
    of this problem. It offers the user a number of options. As currently constructed
    the program will get a buffer value and a bundle limit value from the user. It will
    assume that the actual cashflow for each stock is the minimum possible cashflow and
    then optimize the problem. 
"""
from gurobipy import *
import MySQLdb as mySQL

def getDBData(commandString, connection):
    cursor = connection.cursor()
    cursor.execute(commandString)
    results = list(cursor.fetchall())
    cursor.close()
    return results

def putResultsData(insertList, connection):
    cursor = connection.cursor()
    cursor.executemany("CALL spPutResults(%s,%s)", insertList)
    connection.commit()
    cursor.close()

cnx = mySQL.connect(user='put user name here', passwd='password goes here',
                    host='localhost', db='netlife')

#investmentcost is a list of tuples (id,cost)
cost = getDBData("CALL spGetInvestment;", cnx)
#requirements is a list of tuples (yr,req)
requirements = getDBData("CALL spGetRequirements;", cnx)
#cashflow is a list of tuples (id, yr, cashflow)
cashflow = getDBData("CALL spGetCashflow;", cnx)
cash = {}
for thiscash in cashflow:
    cash[(thiscash[0],thiscash[1])] = thiscash[2]
del cashflow

b = input("please enter a buffer amount: ")
max_bundle = input("please enter the maximum number of bundles: ")
#with b=0, 128 is the smallest no. of bundles we can limit to
#with b=1600000, smallest no. of bundles is 136
m = Model("netlife")
m.ModelSense = GRB.MINIMIZE

#Decision variables
dvar = {}
for i in range(len(cost)):
    dvar[cost[i][0]] = m.addVar(vtype=GRB.BINARY, obj = cost[i][1],lb=0,name="x_"+str(cost[i][0]))

m.update()

#Constraints
#cash flow must cover payout requirements, cash carries over from year to year
for j in range(len(requirements)):
    if j == 0:
        m.addConstr(quicksum(cash[i,j]*dvar[i] for (i,k) in cash.keys() if k == j)-requirements[j][1], GRB.GREATER_EQUAL,b, name = "initial cash flow constraint")
    else:
        m.addConstr(quicksum(cash[i,j]*dvar[i] for (i,k) in cash.keys() if k == j)+1.015*(quicksum(cash[i,j-1]*dvar[i] for (i,k) in cash.keys() if k == j)-requirements[j-1][1])-requirements[j][1],GRB.GREATER_EQUAL,b, name = "cash flow constraint")


#impose a limit to number of bundles you can buy
m.addConstr(quicksum(dvar[i] for i in range(len(cost))),GRB.LESS_EQUAL,max_bundle, name = "bundle limit")

m.update()

m.optimize()

print "Results"
print
buybundles = []
for var in range(len(cost)):
    if dvar[var].x > 0:
#        print dvar[var].varName, " ", dvar[var].x
        buybundles.append([dvar[var].varName,dvar[var].x])

"""
    Un-comment the following code to print out the cash flows and requirements for each year
"""
#print cash flows
#for j in range(len(requirements)):
#    if j == 0:
#        print "available cash in year ", j, " ", quicksum(cash[i,j]*dvar[i] for (i,k) in cash.keys() if k == j).getValue()
#        print "requirement ", requirements[j][1]
#    else:
#        print "available cash in year ", j, " ", quicksum(cash[i,j-1]*dvar[i]+cash[i,j]*dvar[i] for (i,k) in cash.keys() if k == j).getValue()-requirements[j-1][1]
#        print "requirement ", requirements[j][1]

print "number of bundles: ",len(buybundles)
print
print "objective value ", m.ObjVal


cnx.close()
