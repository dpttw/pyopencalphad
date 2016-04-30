# array in Fortran starts from 0, in python starts from 0
# To avoid confusion for Python user, the phase and element index will +1
#

import os
import numpy as np

# &&&& tq function &&&&
from liboctqpy import pytq as ptq

# ============= initialize
def tqini():
	ptq('ini',0,1.0,' ')
	return None

# ============= read database
def tqrfil(filename):
	ptq('tqrfil',0,1.0,filename)
	return None

# ============= read database & read element
def tqrpfil(filename,elements):
	if type(elements) is str:
		no_element = 1
		file_ele = filename + ' ' + elements
	elif (type(elements) is tuple) or (type(elements) is list):
		no_element = len(elements)
		file_ele = filename + ' ' + ' '.join(elements)
	else:
		print "=== Composition inputs error ==="
		return None

	ptq('tqrpfil',no_element,1.0,file_ele)

	return None

# ============= change phase status
def tqphsts(phase,status,value):

	phase_index = get_phase_index(phase)
	if phase_index == -5:
		return None

# nystat:-4 hidden, -3 suspended, -2 dormant, -1,0,1 entered, 2 fix
	if status[0].lower() == "h":
		status_index = -4
	elif status[0].lower() == "s":
		status_index = -3
	elif status[0].lower() == "d":
		status_index = -2
	elif status[0].lower() == "e":
		status_index = 0
	elif status[0].lower() == "f":
		status_index = 2
	else:
		print "ERROR: incorrect phase status"
		return None

	phase_prop = [phase_index,status_index]
	ptq('tqphsts',phase_prop,value,' ')
	return None

# ============= get composition name
def tqgcom():
	dum, int_out, doub_out, char_out = ptq('tqgcom',0,1.,' ')
	elem = ["".join(char_out[i]).split()[0] for i in range(int_out[0])]
	return elem

# ============= get # phase
def tqgnp():
	dum, int_out, doub_out, char_out = ptq('tqgnp',0,1.,' ')
	return int_out[0]

# ============= get phase name
def tqgpn():
	dum, int_out, doub_out, char_out = ptq('tqgpn',0,1.,' ')
	phase = ["".join(char_out[i]).split()[0] for i in range(int_out[0])]
	return phase

# ============= get phase index
def tqgpi(phase):
	dum, int_out, doub_out, char_out = ptq('tqgpi',0,1.,phase)
	return int_out[0]

# ============= set conditions
def tqsetc(condition,element,value):
	element_index = 0
	if type(element) is str:
		element_index = get_element_index(element)
		if element_index == -5:
			return None
	print "element",element,element_index 
	dum, int_out, doub_out, char_out = ptq('tqsetc',element_index,value,condition)
	return None

# ============= eq calculation
def tqce():
	dum, int_out, doub_out, char_out = ptq('tqce',0,0.,' ')
	return None

# ============= retrive the data
def tqgetv(condition,phase,element):
	# phase name to phase index
	phase_index = get_phase_index(phase)
	if phase_index == -5:
		return None
	# element name to element index
	element_index = get_element_index(element)
	if element_index == -5:
		return None
	#print "phase",phase,phase_index,"    element:",element,element_index
	#print "phase index: ",phase_index
	i_var = [phase_index,element_index]
	dum, int_out, doub_out, char_out = ptq('tqgetv',i_var,0.,condition)
	#print doub_out
	return doub_out[0]

# ============= retrive the sublattice information
# input: phase name
# output: 
#    no_sublattice           - number of sublattice
#    no_component_sublattice - number of component in each sublattice
#    ele_names               - component names in each sublattice
#    composition             - composition of component in each sublattice
#    no_sites_sublattice     - number of sites in each sublattice
#    moles_atom              - mole atoms of this phase
#    net_charge              - net charge of the phase
# 
def tqgphc(phase):
	# phase name to phase index
	phase_index = get_phase_index(phase)
	if phase_index == -5:
		return None

	i_var = phase_index
	dum, int_out, doub_out, char_out = ptq('tqgphc',i_var,0.,' ')

	no_sublattice = int_out[0]
	#print no_sublattice
	no_component_sublattice = int_out[1:1+no_sublattice]
	#print no_component_sublattice


	count_index = 0
	element_index = []
	composition = []
	for i in range(no_sublattice):
		element_index.append(int_out[count_index+1+no_sublattice:count_index+1+no_sublattice+no_component_sublattice[i]])
		composition.append(list(doub_out[count_index:count_index+no_component_sublattice[i]]))
		count_index = count_index+no_component_sublattice[i]

	#print composition
	#print element_index

	element_index[:] = [x - 1 for x in element_index]
	#print element_index

	sys_ele_names = tqgcom()
	sys_ele_names.insert(0,'VA')
	#print sys_ele_names

	ele_names = [[sys_ele_names[i] for i in element_index[j]] for j in range(no_sublattice)]
	#print ele_names 

	no_sites_sublattice = doub_out[count_index:count_index+no_sublattice]
	#print no_sites_sublattice

	moles_atom = doub_out[count_index+no_sublattice]
	net_charge = doub_out[count_index+no_sublattice+1]
	#print moles_atom,net_charge

	
	return (no_sublattice,no_component_sublattice,ele_names,composition,no_sites_sublattice,moles_atom,net_charge)

# ============= reset errors
def tqrseterr():
	ptq('tqreset',0.,0.,' ')
	return None


# ************
def get_phase_index(phase):
	no_phase = tqgnp()
	phase_names = tqgpn()
	#print "py",phase_names,len(phase_names)

	phase_index = -5
	if phase[0].lower() == "*":
		phase_index = -1
	else:
		if type(phase) is str:
			'''
			for i in range(no_phase):
				if phase_names[i].find(phase.upper()) == 0:
					phase_index = i + 1
					break
			'''
			full_name = [fn for fn in phase_names if phase in fn]
			#print "PY:  ",full_name
			if full_name:
				phase_index = phase_names.index(full_name[0]) + 1
				#print full_name,phase_index
		else:
			print "ERROR: incorrect phase name"	

	if phase_index == -5:
		print "ERROR: cannot find the phase"	

	return phase_index

# ************
def get_element_index(element):
	ele_names = tqgcom()
	element_index = -5
	if element.upper() == "*":
		element_index = -1
	elif type(element) is str:
		if element.upper() == "NA":
			element_index = -1
		else:
			full_name = [fn for fn in ele_names if element in fn]
			element_index = ele_names.index(full_name[0]) + 1
	else:
		print "ERROR: incorrect element name"

	return element_index


