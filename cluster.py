#!/usr/bin/env python

# This program attempts to cluster traces
import sys
import os.path
import pdb
import numpy as np
import collections
from matplotlib import pyplot
from scipy.cluster.vq import vq, kmeans, whiten


# numpy elements map to the following in index order
object_ops = [
    
    'GETFIELD_GC_PURE_OP',
    'GETFIELD_RAW_PURE_OP',
    
    'GETINTERIORFIELD_GC_OP',
    'RAW_LOAD_OP',
    'GETFIELD_GC_OP',
    'GETFIELD_RAW_OP',
    


    'RAW_STORE_OP',
    'SETFIELD_GC_OP',
    'ZERO_PTR_FIELD_OP' # only emitted by the rewrite, clears a pointer field
                        # at a given constant offset, no descr
    ]


array_ops = ['ARRAYLEN_GC_OP',
             'GETARRAYITEM_GC_OP',
             'GETARRAYITEM_RAW_OP',
             'GETARRAYITEM_GC_PURE_OP',
             'GETARRAYITEM_RAW_PURE_OP',
             'SETARRAYITEM_GC_OP',
             'SETARRAYITEM_RAW_OP',
             'SETINTERIORFIELD_GC_OP',
             'SETINTERIORFIELD_RAW_OP',
             'ZERO_ARRAY_OP']

int_ops = ['INCREMENT_DEBUG_COUNTER_OP',
           'INT_LT_OP',
           'INT_LE_OP',
           'INT_EQ_OP',
           'INT_NE_OP',
           'INT_GT_OP',
           'INT_GE_OP',
           'UINT_LT_OP',
           'UINT_LE_OP',
           'UINT_GT_OP',
           'UINT_GE_OP',
           'INT_ADD_OP',
           'INT_SUB_OP',
           'INT_MUL_OP',
           'INT_FLOORDIV_OP',
           'UINT_FLOORDIV_OP',
           'INT_MOD_OP',
           'INT_AND_OP',
           'INT_OR_OP',
           'INT_XOR_OP',
           'INT_RSHIFT_OP',
           'INT_LSHIFT_OP',
           
           'UINT_RSHIFT_OP',
           'INT_SIGNEXT_OP',
           'INT_IS_ZERO_OP',
           'INT_IS_TRUE_OP',
           'INT_NEG_OP',
           'INT_INVERT_OP',
           'INT_FORCE_GE_ZERO_OP',
           'INT_ADD_OVF_OP',
           'INT_SUB_OVF_OP',
           'INT_MUL_OVF_OP']

float_ops = [ 'FLOAT_ADD_OP',
              'FLOAT_SUB_OP',
              'FLOAT_MUL_OP',
              'FLOAT_TRUEDIV_OP',
              'FLOAT_NEG_OP',
              'FLOAT_ABS_OP']

alloc_ops = ['NEW_OP',             #-> GcStruct, gcptrs inside are zeroed (not the rest)
             'NEW_WITH_VTABLE_OP',  #-> GcStruct with vtable, gcptrs inside are zeroed
             'NEW_ARRAY_OP',       #-> GcArray, not zeroed. only for arrays of primitives
             'NEW_ARRAY_CLEAR_OP', #-> GcArray, fully zeroed
             'NEWSTR_OP',           #-> STR, the hash field is zeroed
             'NEWUNICODE_OP']      #-> UNICODE, the hash field is zeroed]

string_ops = [ 
    'UNICODELEN_OP',
    'UNICODEGETITEM_OP',
    'STRLEN_OP',
    'COPYSTRCONTENT_OP',     
    'COPYUNICODECONTENT_OP',
    'STRGETITEM_OP']
guard = "GUARD:"
jump = "JUMP_OP"

counts = np.zeros(8)
prog_vecs = collections.OrderedDict()
for path in sys.argv[1:]:
    print path
    # create vector for classes
    prog_vec = np.zeros(7)
    with open(path, 'r') as f:
        for line in f:
            line = line.split()
            index = 99
            if line[0] in object_ops:
                index = 0
            elif line[0] in array_ops:
                index = 1
            elif line[0] in int_ops:
                index = 2
            elif line[0] in float_ops:
                index = 3
            elif line[0] in alloc_ops:
                index = 4
            elif line[0] == guard:
                index = 5
            elif line[0] == jump:
                index = 6
            else:
                continue
            counts[index] += 1
            prog_vec[index] = int(line[1])
    total = np.sum(prog_vec)
    func = lambda x: x / float(total)
    vfunc = np.vectorize(func)
                # add to global list
    prog_vecs[os.path.basename(path)] = vfunc(prog_vec)


features = np.array(prog_vecs.values())

whitened = whiten(features)
initial = [kmeans(features,i) for i in range(1,10)]
pyplot.plot([var for (cent,var) in initial])
pyplot.show()
    
centroids,_ =  kmeans(whitened, 3)

assignment,cdist = vq(whitened,centroids)

pdb.set_trace()
