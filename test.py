#!/bin/env python
import os, sys, httplib, urllib, socket
from xml.dom import minidom
from util import decodeDoi
import random
from preprocessor import PlainProcessor, parseLaTeX
import couchdb.client
from db import couchdb_server, port
import time

rand = random.Random()

def pruneNode(node):
  if node.childNodes:
    if len(node.childNodes)>2:
      start = rand.randint(0, len(node.childNodes)-1)
      end = rand.randint(0, len(node.childNodes)-1)
      if start>end:
        start, end = end, start
      elif start == end and end < len(node.childNodes):
        end = end+1
      elif start == end and start > 0:
        start = start-1
      try:
        del node.childNodes[end:len(node.childNodes)]
        del node.childNodes[0:start] 
      except AttributeError:
        pass # Some types of nodes dont support deletion

  return node

# Return a random (and syntacically correct) substring of a latex string
def substring(latex):
  node = parseLaTeX("\\begin{document} $$ " + latex + " $$ \\end{document}")
  pruneNode(node)
  result = PlainProcessor().process(node).dumps()
  return result

# Search for a substring of an existing equation and check that the parent article is included in the results
def runTest(doi,transform):
  db = couchdb_server['documents']
  eqnID, source = rand.choice(db[doi]['source'].items())
  results = None
  searchTerm = None
  try:
    searchTerm = transform(source)
    url = "http://localhost:%s/documents/_external/index?searchTerm=\"%s\"&searchTimeout=20&limit=10000" % (port, urllib.quote(searchTerm))
    startTime = time.time()
    resultsFile = urllib.urlopen(url)
    endTime = time.time()
    results = minidom.parse(resultsFile)
    if results.getElementsByTagName("LatexParseError"):
      print "Latex parse error on doi: %s and eqnID: %s (%fs)" % (decodeDoi(doi), eqnID, endTime-startTime)
      return False
    if results.getElementsByTagName("TimedOut"):
      print "Timed out on doi: %s and eqnID: %s (%fs)" % (decodeDoi(doi), eqnID, endTime-startTime)
      return False
    if results.getElementsByTagName("LimitExceeded"):
      print "Limit exceeded on doi: %s and eqnID: %s (%fs)" % (decodeDoi(doi), eqnID, endTime-startTime)
      return False
    for result in results.getElementsByTagName("result"):
      if result.attributes.get('doi').value == decodeDoi(doi):
        for eqn in result.getElementsByTagName("equation"):
          if eqn.attributes.get('id').value == eqnID:
            print "Passed on doi: %s and eqnID: %s (%fs)" % (decodeDoi(doi), eqnID, endTime-startTime)
            return True
    print "Failed on doi: %s and eqnID: %s (%fs)" % (doi, eqnID, endTime-startTime)
    print searchTerm
    return False
  except KeyboardInterrupt, e:
    raise e
  except Exception, e:
    print "Error on doi: %s and eqnID: %s (%fs)" % (decodeDoi(doi), eqnID, 0)
    print e
    try:
      print "Searchterm: %s" % searchTerm
    except UnicodeEncodeError:
      pass
    return False

def runTests(n,transform):
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
    runTest(doi,transform)
    sys.stdout.flush()

import getopt

if __name__ == '__main__':
  opts, args = getopt.getopt(sys.argv[1:], "", ["simple=","substring="])
  for opt, arg in opts:
    if opt == "--simple":
      runTests(int(arg),lambda x: x)
    if opt == "--substring":
      runTests(int(arg),substring)
  print "Ok"

