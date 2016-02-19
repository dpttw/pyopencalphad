import os
import numpy as np

# &&&& tq function &&&&
from liboctqpy import pytq as ptq

# ============= initialize
vari = [0]
string = 'str'
ptq('ini',vari,1.0,string)

# ============= read database
string = 'FENI.TDB'
ptq('tqrfil',vari,1.0,string)

# ============= get composition
dum, int_out, doub_out, char_out = ptq('tqgcom',vari,1.0,string)

no_ele = np.trim_zeros(int_out)
print no_ele,type(no_ele)

ele_name = "".join(char_out).split()
print ele_name,type(ele_name)

# ============= get # phase
dum, int_out, doub_out, char_out = ptq('tqgnp',vari,1.0,string)

no_phase = np.trim_zeros(int_out)
print no_phase,type(no_phase)










