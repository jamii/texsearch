#!/usr/bin/env python
import sys
import re
import search
import urllib

search_term_re = re.compile(r'searchTerm=([^&]*)&')

def replay_log_file(filename):
    search_terms = set()

    for log in open(filename):
        match = search_term_re.search(log)
        if match:
            search_term = urllib.unquote(match.group(1))
            search_terms.add(search_term)

    for search_term in search_terms:
        result = search.search(search_term, searchTimeout="55000.0", limit="10000")
        yield (result['time'], search_term)

if __name__ == '__main__':
    for time, search_term in replay_log_file(sys.argv[1]):
        print time, search_term
