import argparse

from itertools import izip

import matplotlib.pyplot as plt
import numpy as np
from scipy.optimize import curve_fit

import trace_parser
import trace as trace_utils

def main():
    parser = argparse.ArgumentParser(description="The DIFFER")
    parser.add_argument("filenames", metavar="<file>", nargs = '+')
    parser.add_argument("--model", "-m",  default="cmw")
    parser.add_argument( "--title", "-t")

    args = parser.parse_args()

    #times = [ float(item) for item in args.times.split()]
    model = [211,34,590,9937,14]
    programs = trace_parser.parse_files(args.filenames)
    times = []
    costs = []
    xcols = []
    for i in xrange(0, len(programs), 2):
        
        warmup_counts = programs[i].hashed_counts()
        task_counts = programs[i+1].hashed_counts()
        task_time = programs[i+1].net_time()
#        tracing = programs[1].tracing_time()
        assert len(warmup_counts) == len(task_counts)
        

        diff = {key:(task_counts[key] - warmup_counts[key]) for key in task_counts if (task_counts[key] - warmup_counts[key]) > 0}
        print "TRACE COUNT: ", len(diff)
        dot = lambda x,y: sum(a*b for a,b in izip(x,y))
        cost = dot(programs[1].diff_class_counts(diff), model)
        times.append(task_time)
        costs.append(task_time)
        xpos = program.name.find("x")
        tasksize = program.name[xpos+1:]
        xcols.append(int(tasksize))

    plt.ylabel("$k$")
    plt.title(args.title)
    plt.xlabel("chunk size")
    ys = [time/cost for time,cost in izip(times, costs)]
    plt.plot(xcols, ys, 'xg')
    plt.show()
    
        
    # print "COUNTER DIFF"
    # for key, value in diff.iteritems():
    #     print "%(key)d: %(value).0f" % locals()  
    # print "TIME: %(time)g microseconds" % {"time":task_time}
    # print "DIFF COSTS: %(cost)g" % {"cost":cost}
    # print "TRACING: %(tracing)g" % {"tracing": tracing}
    

if __name__ == "__main__":
    main()
