#!/bin/env python
import urllib

def search(searchTerm, searchTimeout, limit, server):
  url = "http://%s/documents/_external/index?searchTerm=\"%s\"&searchTimeout=%d&limit=%d" % (server, urllib.quote(searchTerm), searchTimeout, limit)
  return urllib.urlopen(url).read()

import sys, getopt

if __name__ == '__main__':
  opts, args = getopt.getopt(sys.argv[1:], "", ["searchTerm=","searchTimeout=","limit=","server="])
  searchTerm = None
  searchTimeout = 20
  limit = 2500
  server = '192.87.127.133:5986'

  for opt, arg in opts:
    if opt == "--searchTerm":
      searchTerm = arg
    elif opt == "--searchTimeout":
      searchTimeout = float(arg)
    elif opt == "--limit":
      limit = int(arg)
    elif opt == "--server":
      server = arg

  if searchTerm:
    print "Searching for %s" % searchTerm
    print search(searchTerm, searchTimeout, limit, server)
