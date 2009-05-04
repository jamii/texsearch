#!/bin/env python

import string, re
from plasTeX import TeXFragment
from plasTeX.DOM import Node

class Renderer(dict):
    def render(self,node,output):
        # Short circuit text nodes
        if node.nodeType == Node.TEXT_NODE:
            self.textDefault(node,output)
        else:
          # Locate the rendering callable, and call it with the
          # current object (i.e. `child`) as its argument.
          func = self.find(node.nodeName, self.default)
          func(node,output)

    def renderChildren(self, node, output):
        # Render all child nodes
        for child in node.childNodes:
            self.render(child,output)

    def default(self, node, output):
        """ Rendering method for all non-text nodes """
        children = []

        # See if we have any attributes to render
        if node.hasAttributes():
          for key, value in node.attributes.items():
              # If the key is 'self' these nodes are the same as the child nodes
              # If the key is '*modifier*' we dont care about it
              if key == 'self' or key == '*modifier*':
                  continue
              if value.__class__ is TeXFragment:
                  result = self.renderChildren(value,children)
              else:
                  result = children.append(unicode(value))

        # Invoke rendering on child nodes
        self.renderChildren(node,children)

        output.append({node.nodeName : children})

    def textDefault(self, node, output):
        """ Rendering method for all text nodes """
        text = re.sub("\s+", "", unicode(node))
        if text:
          output.append(text)

    def find(self, key, default=None):
        if self.has_key(key):
            return self[key]

        self[key] = default
        return default

    def ignore(self, node, output):
        for child in node.childNodes:
            self.render(child,output)

renderer = Renderer()
# Get rid of some extraneous tags
renderer['displaymath'] = renderer.ignore
renderer['bgroup'] = renderer.ignore
renderer['math'] = renderer.ignore
renderer['text'] = renderer.ignore
renderer['nulldelimiterspace'] = renderer.ignore
renderer['vphantom'] = renderer.ignore
renderer['aligned'] = renderer.ignore
renderer['gathered'] = renderer.ignore
renderer['active::&'] = renderer.ignore
renderer['\\'] = renderer.ignore
renderer['#document'] = renderer.ignore
renderer['document'] = renderer.ignore

# Pair up left + right

from plasTeX.TeX import TeX

def processTeX(string):
    # Instantiate a TeX processor and parse the input text
    tex = TeX()
    tex.input(string)
    document = tex.parse()

    return document

import os, sys, httplib
from xml.dom import minidom
try:
    # Python 2.6
    import json
except:
    # Prior to 2.6 requires simplejson
    import simplejson as json
# Bulk process a xml document
def processXml(fileName):
    conn = httplib.HTTPConnection("localhost:5984")
    headers = {"Content-type": "application/json"}

    docs = minidom.parse(fileName)
    for item in docs.childNodes[0].childNodes:
        if item.nodeName == u'result':
            doi = item.childNodes[0].childNodes[0].wholeText
            for node in item.childNodes[1:]:
                source = node.childNodes[0].wholeText

                content = []
                renderer.render(processTeX("\\begin{document}"+source+"\\end{document}"),content)

                doc = json.dumps({'doi': doi, 'source': source, 'content': content})
                conn.request("POST", "/documents", doc, headers)
                response = conn.getresponse()
                if response.status != 201:
                    # What status codes are acceptable here?
                    raise IOError
                response.read() # Have to read before next request
    conn.close()

def search(query):
    conn = httplib.HTTPConnection("localhost:5984")
    headers = {"Content-type": "application/json"}
    conn.request("GET", "/documents/_external/index", json.dumps(query), headers)
    response = conn.getresponse()
    if response.status != 200:
        # What status codes are acceptable here?
        raise IOError
    conn.request("POST", "documents/_all_docs?include_docs=true", {keys : response.read()}, headers)
    response = conn.getresponse()
    if response.status != 200:
        # What status codes are acceptable here?
        raise IOError
    return response.read()

def requests():
    line = sys.stdin.readline()
    while line:
        yield json.loads(line)
        line = sys.stdin.readline()

def respond(request):
    try:
      query = request['query']
      latex = query['latex']
      parseResults = []
      renderer.render(processTeX("\\begin{document}"+latex+"\\end{document}"),parseResults)
      response = json.dumps({'json':parseResults, 'code':200}) # OK
      #query['latex'] = json.dumps(parseResults)
      #response = "{\"json\":%s, \"code\":200}" % search(query)
    except KeyError:
      response = json.dumps({'json':{'error':'Bad request'}, 'code':400}) # Bad request
    #except Exception, exception:
      #response = json.dumps({'json':{'error':unicode(exception)}, 'code':500}) # Internal server error
    sys.stdout.write("%s\n" % response)
    sys.stdout.flush()

def main():
    for request in requests():
        respond(request)

if __name__ == "__main__":
    main()