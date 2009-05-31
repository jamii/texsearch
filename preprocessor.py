#!/bin/env python

import string, re
from plasTeX import TeXFragment
from plasTeX.DOM import Node

# Ignore useless tags
ignoreSet = frozenset([
 'displaymath'
,'bgroup'
,'math'
,'text'
,'nulldelimiterspace'
,'vphantom'
,'hphantom'
,'hfill'
,'vfill'
,'hbox'
,'align'
,'aligned'
,'gathered'
,'active::&'
,'\\'
,'#document'
,'document'
,'left'
,'right'
,'rm'
,'par'])

# Transform a PlasTeX DOM into a LaTeX string
def dump(node):
  # Short circuit text nodes
  if node.nodeType == Node.TEXT_NODE:
    return "{%s}" % unicode(node)
  elif node.nodeName in ignoreSet:
    # Ignore node and move on to children
    return " ".join([ dump(child) for child in node.childNodes ])
  else:
    """ Rendering method for all non-text nodes """
    children = []

    # See if we have any attributes to dump
    if node.hasAttributes():
      for key, value in node.attributes.items():
        # If the key is 'self' these nodes are the same as the child nodes
        # If the key is '*modifier*' we dont care about it
        if key == 'self' or key == '*modifier*':
          continue
        if value.__class__ is TeXFragment:
          children.append(dump(value))
        else:
          children.append(unicode(value))

    # Dump child nodes
    for child in node.childNodes:
      children.append(dump(child))

    node.nodeName + " ".join["{%s}" % child for child in children])

from plasTeX.TeX import TeX

class PreprocessorError(Exception):
  pass

def preprocess(string):
  # Instantiate a TeX processor and parse the input text
  tex = TeX()
  tex.disableLogging()
  tex.input(string)
  return dump(tex.parse())

import simplejson as json

def requests():
  line = sys.stdin.readline()
  while line:
    yield json.loads(line)
    line = sys.stdin.readline()

def main():
  headers = {"Content-type": "text/plain"}
  for request in requests():
    try:
      query = request['query']
      response = preprocess("\\begin{document}$$"+query['latex']+"$$\\end{document}")
      code = 200 # OK
    except KeyError, e:
      response = 'Error: ' + str(e)
      code = 400 # Bad request
    except Exception, e:
      response = 'Error: ' + str(e)
      code = 500 # Internal server error
    sys.stdout.write("%s\n" % json.dumps({'code':code, 'headers': headers, 'body':response}))
    sys.stdout.flush()

if __name__ == "__main__":
    main()