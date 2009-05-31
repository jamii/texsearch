#!/bin/env python

import simplejson as json
import sys, httplib, urllib
from preprocessor import preprocess
import re
from util import expectResponse, decodeDoi

def search(query):
  conn = httplib.HTTPConnection("localhost:5984")

  url = "/documents/_external/index?%s" % urllib.urlencode(query)
  conn.request("GET", url)
  result = expectResponse(conn,200)

  conn.close()
  return json.loads(result)

def lookupSources(results):
  conn = httplib.HTTPConnection("localhost:5984")

  responses = []
  for result in results:
    doi = result[0]
    conn.request("GET", "/documents/%s" % doi)
    sources = json.loads(expectResponse(conn,200))['source']
    response = (decodeDoi(doi), [sources[id] for id in result[1]])
    responses.append(response)

  conn.close()

  return responses

def requests():
  line = sys.stdin.readline()
  while line:
    yield json.loads(line)
    line = sys.stdin.readline()

def main():
  for request in requests():
    try:
      query = request['query']
      query['latex'] = json.dumps(preprocess(query['latex']))
      results = search(query)
      response = lookupSources(results)
      code = 200 # OK
    except KeyError, e:
      response = {'error': str(e)}
      code = 400 # Bad request
    except Exception, e:
      response = {'error': str(e)}
      code = 500 # Internal server error
    sys.stdout.write("%s\n" % json.dumps({'code':code, 'json':response}))
    sys.stdout.flush()

if __name__ == "__main__":
    main()
