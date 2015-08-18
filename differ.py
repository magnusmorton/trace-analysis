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

    times = [ float(item) for item in args.times.split()]

    programs = trace_parser.parse_files(args.filenames)
    seq_counts = programs[0].counts
    chunked_counts = programs[1].counts

    assert len(seq_counts) == len(chunked_counts)
    
    

if __name__ == "__main__":
    main()
