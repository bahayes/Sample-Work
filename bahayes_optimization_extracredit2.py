# -*- coding: utf-8 -*-
"""
Created on Fri Oct 07 18:19:49 2016

@author: bahayes
"""

import gurobipy
from gurobipy import *
# create Gurobi model
m = Model("model_extracredit")
m.ModelSense = GRB.MINIMIZE
m.setParam('TimeLimit',7200) 

#let's set up our lists
capacity = [10,20,5,15,10]
demand = [5,4,7,8,5,5]
cost = [[500,350,250,1300,1000000000,750],[600,200,500,1000000000,850,900],[250,1000000000,175,300,500,400],[1000000000,875,1000,1100,900,1000000000],[1000,450,10000000000,900,300,800]]
cap = range(len(capacity))
dem = range(len(demand))

#now for some variables
variables = []
for i in cap:
    variables.append([])
    for j in dem:
        variables[i].append(m.addVar(obj=cost[i][j],name="x%d.%d" % (i,j), lb = 0.0))

m.update()

#time for some constraints
for i in cap:
    m.addConstr(quicksum(variables[i][j] for j in dem), GRB.LESS_EQUAL, capacity[i],"Capacity%d" % i)
    
for j in dem:
    m.addConstr(quicksum(variables[i][j] for i in cap),GRB.GREATER_EQUAL,demand[j],"Demand%d" % j)

m.update()

# Optimize the model
m.optimize()

for i in cap:
    for j in dem:
      print variables[i][j].varName, "=", variables[i][j].x, "(", variables[i][j].lb,",",variables[i][j].ub,")"
      

# alternate printout of results
#for var in m.getVars():
#    print "Variable Name = %s, Optimal Value = %s, Lower Bound = %s, Upper Bound = %s" % (var.varName, var.x,var.lb,var.ub)
    
