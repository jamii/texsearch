#!/usr/bin/env python
import sys
import re
import search
import urllib

search_term = re.compile(r'searchTerm=([^&]*)&')

def replay_log_file(filename): 
    for log in open(filename):
        match = search_term.search(log)
        if match:
            st = urllib.unquote(match.group(1))
            result = search.search(st, searchTimeout="55.0", limit="10000")
            yield (result['time'], st)

if __name__ == '__main__':
    for time, st in replay_log_file(sys.argv[1]):
        print time, st
