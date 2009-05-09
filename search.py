#!/bin/env python

import simplejson as json
import sys, httplib, urllib
from preprocessor import preprocess
import re

headers = {"Content-type": "application/json"}

def search(latex,limit):
  conn = httplib.HTTPConnection("localhost:5984")
  url = "/documents/_external/index?%s" % urllib.urlencode({'latex':latex, 'limit':limit})
  conn.request("GET", url)
  response = conn.getresponse()
  if response.status != 200:
    # What status codes are acceptable here?
    raise IOError
  result = response.read()
  conn.close()
  return result

def lookup(ids):
  conn = httplib.HTTPConnection("localhost:5984")
  conn.request("POST", "/documents/_design/demo/_view/results", "{\"keys\": %s}" % ids, headers)
  response = conn.getresponse()
  if response.status != 200:
    # What status codes are acceptable here?
    raise IOError
  result = response.read()
  conn.close()
  return result

def requests():
  line = sys.stdin.readline()
  while line:
    yield json.loads(line)
    line = sys.stdin.readline()

def test():
  latex = preprocess("$$(h_b^2)$$")
  ids = search(json.dumps(latex), 10)
  results = re.sub("\s+", "", lookup(ids))
  print results

def main():
  for request in requests():
    try:
      query = request['query']
      ids = search(json.dumps(preprocess(query['latex'])), query['limit'])
      results = re.sub("\s+", "", lookup(ids))
      code = 200 # OK
    except KeyError, e:
      results = {'error': str(e)}
      code = 400 # Bad request
    #except Exception, e:
      #results = {'error': str(e)}
      #code = 500 # Internal server error
    sys.stdout.write("{\"code\":%s, \"json\":%s}\n" % (code,results))
    sys.stdout.flush()

if __name__ == "__main__":
    main()
