#!/bin/env python
import os, sys, httplib, urllib
from xml.dom import minidom
from util import decodeDoi
import random
from preprocessor import PlainProcessor, parseLaTeX
import couchdb.client
from db import couchdb_server, port

rand = random.Random()

def pruneNode(node):
#  if node.childNodes:
#    start = rand.randint(0, len(node.childNodes)-1)
#    end = rand.randint(0, len(node.childNodes)-1)
#    if start>end:
#      start, end = end, start
#    pruneNode(node.childNodes[start])
#    pruneNode(node.childNodes[end])
#    del node.childNodes[end:len(node.childNodes)]
#    del node.childNodes[0:start] 

  # For now just use the whole node
  return node

# Return a random (and syntacically correct) substring of a latex string
def substring(eqnID, latex):
  try:  
    node = parseLaTeX("\\begin{document} $$ " + latex + " $$ \\end{document}")
    pruneNode(node)
    result = PlainProcessor().process(node).dumps()
    return result
  except KeyboardInterrupt, e:
    raise e
  #except Exception, e:
  # print "Note: Pruner failed on equation %s : %s" % (eqnID, e)
  # return None

# Search for a substring of an existing equation and check that the parent article is included in the results
def testSubstring(doi):
  db = couchdb_server['documents']
  eqnID, source = rand.choice(db[doi]['source'].items())
  searchTerm = substring(eqnID, source)
  url = "http://localhost:%s/documents/_external/index?searchTerm=\"%s\"&searchTimeout=20&limit=2500" % (port, urllib.quote(searchTerm))
  print urllib.urlopen(url).read()
  resultsFile = urllib.urlopen(url)
  results = minidom.parse(resultsFile)
  for result in results.getElementsByTagName("result"):
    if result.attributes.get('doi').value == decodeDoi(doi):
      print "Passed on doi: %s and eqnID %s" % (doi, eqnID)
      return True
  print "Failed on doi: %s and eqnID %s" % (doi, eqnID)
  return False

def runTest(n):
  db = couchdb_server['documents']
  dois = list(db)
  for i in xrange(0,n):
    doi = rand.choice(dois)
    while not db[doi]['source']:
      doi = rand.choice(dois)
    testSubstring(doi)

import getopt

if __name__ == '__main__':
  opts, args = getopt.getopt(sys.argv[1:], "", ["n="])
  docs = []
  n = 100
  server = "localhost:5984"
  for opt, arg in opts:
    if opt == "--n":
      n = int(arg)
  runTest(n)
  print "Ok"

