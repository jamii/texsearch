#!/bin/env python

import simplejson as json
import sys, httplib
from preprocessor import preprocess

headers = {"Content-type": "application/json"}

def search(query):
  conn = httplib.HTTPConnection("localhost:5984")
  conn.request("GET", "/documents/_external/index", json.dumps(query), headers)
  response = conn.getresponse()
  if response.status != 200:
    # What status codes are acceptable here?
    raise IOError
  conn.close()
  return json.loads(response.read())

def lookup(ids):
  conn = httplib.HTTPConnection("localhost:5984")
  conn.request("POST", "documents/_design/demo/_view/results", json.dumps({'keys': ids}), headers)
  response = conn.getresponse()
  if response.status != 200:
    # What status codes are acceptable here?
    raise IOError
  conn.close()
  return json.loads(response.read())

def requests():
  line = sys.stdin.readline()
  while line:
    yield json.loads(line)
    line = sys.stdin.readline()

def main():
  for request in requests():
    try
      query = request['query']
      ids = search({'latex' : preprocess(query['latex']), 'limit' : query['limit']})
      results = lookup(ids)
      code = 200 # OK
    except KeyError, e:
      results = {'error': str(e)}
      code = 400 # Bad request
    except Exception, e:
      results = {'error': str(e)}
      code = 500 # Internal server error
    sys.stdout.write("%s\n" % json.dumps({"code": code, "json": results, "headers": headers}))
    sys.stdout.flush()

if __name__ == "__main__":
    main()
