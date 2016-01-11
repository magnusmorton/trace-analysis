import argparse
import re

from itertools import izip

import matplotlib.pyplot as plt
import numpy as np
from scipy.optimize import curve_fit

import trace_parser
import trace as trace_utils
import search

k_means_max_chunk = 102400

chunksize_re = re.compile(r".*x(\d*)task")
chunk_re = re.compile(r".*x(\d*)x.*")

cm0 = [0,0,0,0,0]
cmC = [1,1,1,1,1]
cmW = [0,0,4.884e-4,4.797e-3,4.623e-4]

def output(ysW, ysC, ys0, name):
    with open(name + ".dat", 'w') as f:
        f.write("k W %(k)d\n" % {"k":min(ysW)})
        f.write("k C %(k)d\n" % {"k":min(ysC)})
        f.write("k 0 %(k)d\n" % {"k":min(ys0)})
        f.write("range: %(r)d\n" % {"r":max(ysW) - min(ysW)})
    

def chunks(programs, args):
    times = [{} for _ in xrange(3)]
    costsW = [{} for _ in xrange(3)]
    costsC = [{} for _ in xrange(3)]
    costs0 = [{} for _ in xrange(3)]

    trace_utils.Fragment.model = [0,0,0,0,0,0,0] 
    for i in xrange(0, len(programs), 4):
        
        warmup_counts = programs[i].hashed_counts()
        m_tasksize = chunk_re.match(programs[i+1].name)
        xcol = int(m_tasksize.group(1))
        
        print "NEW"
        for j in xrange(1,4):
            task_counts = programs[i+j].hashed_counts()
            task_time = programs[i+j].net_time()
            #assert len(warmup_counts) == len(task_counts)
        

            diff = {key:(task_counts[key] - warmup_counts[key]) for key in task_counts if (task_counts[key] - warmup_counts[key]) > 0}
            dot = lambda x,y: sum(a*b for a,b in izip(x,y))
            costW = dot(programs[i+j].diff_class_counts(diff), cmW)
            costC = dot(programs[i+j].diff_class_counts(diff), cmC)
            cost0 = programs[i+j].cost() - programs[i].cost()
            times[j-1][xcol] = task_time
            costsW[j-1][xcol] = costW
            costsC[j-1][xcol] = costC
            costs0[j-1][xcol] = cost0

        print programs[i+1].name
        

    xcols = [key for key in sorted(costsW[0])]
    plt.ylabel("$k$")
    plt.title(args.title)
    plt.xlabel("chunk size")
    if args.a:
        ys0 = [times[0][xcol]/costsW[0][xcol] for xcol in xcols]
        ys1 = [times[1][xcol]/costsW[1][xcol] for xcol in xcols]
        ys2 = [times[2][xcol]/costsW[2][xcol] for xcol in xcols]   

        print "k", min(ys0), min(ys1), min(ys2)
        plt.plot(xcols, ys0, '-xg', label="Chunk 0")
        plt.plot(xcols, ys1, '-.ob', label="Chunk 1")
        plt.plot(xcols, ys2, '--+r', label="Chunk 2")
        graph_util(args)
        plt.title(args.title + " 3 chunks")
        plt.savefig(args.title.replace(" ", "_")+ "irr.png")
    else:
        plt.xlabel(args.x)
        ysW = [times[1][xcol]/costsW[1][xcol] for xcol in xcols]
        ysC = [times[1][xcol]/costsC[1][xcol] for xcol in xcols]
        ys0 = [times[1][xcol]/costs0[1][xcol] for xcol in xcols]
        print xcols
        timesout = [times[1][xcol] for xcol in xcols]
        print timesout
        print ysW
            
        print "k w", min(ysW)
        print "k", min(ysC)
        print "k", min(ys0)
        
        print "variance:", max(ysW) - min(ysW)
        output(ysW, ysC, ys0, args.title.replace(" ", "_"))
        plt.subplot(311)
        plt.plot(xcols, ysW, '-xg', label="$CM_W$")
        graph_util(args)

        plt.subplot(312)
        plt.plot(xcols, ysC, '-.ob', label="$CM_C$")
        graph_util(args)
        
        plt.subplot(313)
        plt.plot(xcols, ys0, '--+r', label="$CM_0$")
        graph_util(args)

        plt.suptitle(args.title)
        plt.savefig(args.title.replace(" ", "_")+ ".png")


def graph_util(args):
    if args.title.lower() != "fibonacci":
        plt.xscale("log")
    plt.legend()
    # xticks, _ = plt.xticks()
    # # shift half a step to the left
    # # x0 - (x1 - x0) / 2 = (3 * x0 - x1) / 2
    # xmin = (xticks[0] - xticks[1])/2.
    # # shaft half a step to the right
    # xmax = (xticks[-1] - xticks[-2])/2.
    # plt.xlim(xmin, xmax)
    # plt.xticks(xticks)

    yticks, _ = plt.yticks()
    # shift half a step to the left
    # x0 - (x1 - x0) / 2 = (3 * x0 - x1) / 2
    ymin = (3*yticks[0] - yticks[1])/2.
    # shaft half a step to the right
    ymax = (3*yticks[-1] - yticks[-2])/2.
    plt.ylabel("$k$")
    plt.xlabel(args.x)
    plt.ylim(ymin, ymax)
    plt.yticks(yticks)
    
def main():
    parser = argparse.ArgumentParser(description="The DIFFER")
    parser.add_argument("filenames", metavar="<file>", nargs = '+')
    parser.add_argument("--model", "-m",  default="cmw")
    parser.add_argument( "--title", "-t")
    parser.add_argument( "-x")
    parser.add_argument( "-c", action="store_true")
    parser.add_argument( "-a", action="store_true")
    
    args = parser.parse_args()

    #times = [ float(item) for item in args.times.split()]
    print args
    if args.model == "cm0":
        model = [0,0,0,0,0]
        argstr = "$CM_0$"
    elif args.model == "cmc":
        model = [1,1,1,1,1]
        argstr = "$CM_C$"
    elif args.model == "cmw":
        model = [0,0,4.884e-4,4.797e-3,4.623e-4]
        argstr = "$CM_W$"
    else:
        model = [int(num) for num in args.model.split(",")]
    programs = trace_parser.parse_files(args.filenames)
    
    if args.c:
        return chunks(programs, args)
    times = {}
    costs0 = {}
    costsC = {}
    costsW = {}

    for i in xrange(0, len(programs), 2):
        print i
        warmup_counts = programs[i+1].hashed_counts()
        task_counts = programs[i].hashed_counts()
        task_time = programs[i].net_time()
        #        tracing = programs[1].tracing_time()
        print "NEW"
        print programs[i].name
        print programs[i+1].name
        print len(warmup_counts)
        print len(task_counts)
        #assert len(warmup_counts) == len(task_counts)
        
        trace_utils.Fragment.model = [0,0,0,0,0,0,0] 
        diff = {key:(task_counts[key] - warmup_counts[key]) for key in task_counts if key in warmup_counts and (task_counts[key] - warmup_counts[key]) > 0}
        print "TRACE COUNT: ", len(diff)
        print diff
        dot = lambda x,y: sum(a*b for a,b in izip(x,y))
        costW = dot(programs[i].diff_class_counts(diff), cmW)
        costC = dot(programs[i].diff_class_counts(diff), cmC)
        cost0 = programs[i].cost() - programs[i+1].cost()
       
        # costsW.append(costW)
        # costsC.append(costC)
        # costs0.append(cost0)
        m_tasksize = chunksize_re.match(programs[i].name)
        xcol = int(m_tasksize.group(1))
        if args.title.lower() == "k-means":
            xcol = k_means_max_chunk /  xcol
        times[xcol] = task_time
        costsW[xcol] = costW
        costsC[xcol] = costC
        costs0[xcol] = cost0
        #xcols.append(int(m_tasksize.group(1)))
    xcols = [key for key in sorted(costsW)]
    print times
    print xcols
    plt.ticklabel_format(style='sci', axis='y', scilimits=(0,0))
    plt.ylabel("$k$")
    
    plt.title(args.title)

    if "stride" in args.title.lower():
        xcols = xcols[0:-1]
    ysW = [times[xcol]/costsW[xcol] for xcol in xcols]
    ysC = [times[xcol]/costsC[xcol] for xcol in xcols]
    ys0 = [times[xcol]/costs0[xcol] for xcol in xcols]
    # ysC = [time/cost for time,cost in izip(times, costsC)]
    # ys0 = [time/cost for time,cost in izip(times, costs0)]
    print "k w", min(ysW)
    print "k c", min(ysC)
    print "k 0", min(ys0)
    print "variance:", max(ysW) - min(ysW)
    output(ysW, ysC, ys0, args.title.replace(" ", "_"))
    plt.subplot(311)
    plt.plot(xcols, ysW, '-xg', label="$CM_W$")
    graph_util(args)

    plt.subplot(312)
    plt.plot(xcols, ysC, '-.ob', label="$CM_C$")
    graph_util(args)

    plt.subplot(313)
    plt.plot(xcols, ys0, '--+r', label="$CM_0$")
    graph_util(args)

    plt.suptitle(args.title)
    plt.savefig(args.title.replace(" ", "_") + ".png")
    #plt.show()
    
        
    # print "COUNTER DIFF"
    # for key, value in diff.iteritems():
    #     print "%(key)d: %(value).0f" % locals()  
    # print "TIME: %(time)g microseconds" % {"time":task_time}
    # print "DIFF COSTS: %(cost)g" % {"cost":cost}
    # print "TRACING: %(tracing)g" % {"tracing": tracing}
    

if __name__ == "__main__":
    main()
