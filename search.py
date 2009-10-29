#!/bin/env python
import urllib
import time
from xml.dom import minidom
from db import port, couchdb_server
from util import encodeDoi

def parseResults(results):
  db = couchdb_server['documents']
  for result in results.getElementsByTagName("result"):
    doi = result.attributes.get('doi').value
    source = db[encodeDoi(doi)]['source']
    eqns = [(eqn.attributes.get('id').value, eqn.attributes.get('distance').value) for eqn in result.getElementsByTagName("equation")]
    yield (doi, [(eqnID, distance, source[eqnID]) for (eqnID, distance) in eqns])

def search(searchTerm, searchTimeout="20.0", limit="2500", precision="0.7"):
  response = {}

  url = "http://localhost:%s/documents/_external/index?searchTerm=%s&searchTimeout=%s&limit=%s&precision=%s" % (port, urllib.quote(searchTerm), searchTimeout, limit, precision)
  startTime = time.time()
  results = urllib.urlopen(url).read()
  endTime = time.time()
  
  response['time'] = endTime - startTime
  if results == "<LimitExceeded/>" or results == "<TimedOut/>" or results == "<QueryParseError/>":
    response['error'] = results
  else:
    response['results'] = list(parseResults(minidom.parseString(results)))

  return response
    
import sys
import simplejson as json

def requests():
  line = sys.stdin.readline()
  while line:
    yield json.loads(line)
    line = sys.stdin.readline()

def main():
  for request in requests():
    try: 
      query = request['query']
      response = {'code':200, 'json':search(**query)}
    except Exception, e:
      response = {'code':200, 'body':('Error: ' + str(e)), 'headers':{'Content-type':'text/plain'}} # Internal server error

    sys.stdout.write("%s\n" % json.dumps(response))
    sys.stdout.flush()

if __name__ == "__main__":
    main()
