#!/bin/env python

import simplejson as json
import sys, httplib, urllib
from preprocessor import preprocess
import re
from util import expectResponse

headers = {"Content-type": "application/json"}

def search(latex,limit):
  conn = httplib.HTTPConnection("localhost:5984")
  url = "/documents/_external/index?%s" % urllib.urlencode({'latex':latex, 'limit':limit})
  conn.request("GET", url)
  result = expectResponse(conn,200)
  conn.close()
  return result

def lookup(ids):
  conn = httplib.HTTPConnection("localhost:5984")
  conn.request("POST", "/documents/_all_docs?include_docs=true", "{\"keys\": %s}" % ids, headers)
  result = json.loads(expectResponse(conn,200))
  conn.close()

  #values = [row['value'] for row in result['rows']]
  return result['rows']

def requests():
  line = sys.stdin.readline()
  while line:
    yield json.loads(line)
    line = sys.stdin.readline()

def main():
  for request in requests():
    try:
      query = request['query']
      ids = search(json.dumps(preprocess(query['latex'])), query['limit'])
      results = lookup(ids)
      code = 200 # OK
    except KeyError, e:
      results = {'error': str(e)}
      code = 400 # Bad request
    except Exception, e:
      results = {'error': str(e)}
      code = 500 # Internal server error
    sys.stdout.write("%s\n" % json.dumps({'code':code, 'json':results}))
    sys.stdout.flush()

if __name__ == "__main__":
    main()
