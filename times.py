#!/bin/env python
import os, sys, httplib, urllib, socket
import random
import couchdb.client
from db import couchdb_server, port
import time

rand = random.Random()

def runTime(doi):
  db = couchdb_server['documents']
  eqnID, searchTerm = rand.choice(db[doi]['source'].items())
  try:
    url = "http://localhost:%s/documents/_external/index?searchTerm=\"%s\"&searchTimeout=60&limit=10000" % (port, urllib.quote(searchTerm))
    startTime = time.time()
    resultsFile = urllib.urlopen(url)
    endTime = time.time()
    print endTime-startTime
  except KeyboardInterrupt, e:
    raise e
  except Exception, e:
    pass

def runTimes(n):
  db = couchdb_server['documents']
  dois = list(db)
  for i in xrange(0,n):
    doi = None
    source = None
    while not source:
      try:
        doi = rand.choice(dois)
        source = db[doi]['source']
      except socket.error:
        pass # Connection refused, probably because someone restarted the server
    runTime(doi)
    sys.stdout.flush()

import getopt

if __name__ == '__main__':
  opts, args = getopt.getopt(sys.argv[1:], "", ["n="])
  for opt, arg in opts:
    if opt == "--n":
      runTimes(int(arg))
  print "Ok"

