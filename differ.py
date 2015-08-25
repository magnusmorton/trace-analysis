import argparse
from itertools import izip

import trace_parser
import trace as trace_utils

def main():
    parser = argparse.ArgumentParser(description="The DIFFER")
    parser.add_argument("filenames", metavar="<file>", nargs = '+')
    parser.add_argument("--model", "-m",  default="cmw")
    parser.add_argument( "--times", "-t")

    args = parser.parse_args()

    #times = [ float(item) for item in args.times.split()]
    model = [211,34,590,9937,14]
    programs = trace_parser.parse_files(args.filenames)
    warmup_counts = programs[0].hashed_counts()
    task_counts = programs[1].hashed_counts()
    task_time = programs[1].average_time()
    assert len(warmup_counts) == len(task_counts)

    diff = {key:(task_counts[key] - warmup_counts[key]) for key in task_counts if (task_counts[key] - warmup_counts[key]) > 0}

    dot = lambda x,y: sum(a*b for a,b in izip(x,y))
    cost = dot(programs[1].diff_class_counts(diff), model)
    print "COUNTER DIFF"
    for key, value in diff.iteritems():
        print "%(key)d: %(value).0f" % locals()  
    print "TIME: %(time)g microseconds" % {"time":task_time}
    print "DIFF COSTS: %(cost)g" % {"cost":cost}
    

if __name__ == "__main__":
    main()
