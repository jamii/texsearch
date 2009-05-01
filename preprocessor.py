#!/bin/env python

import string, re
from plasTeX import TeXFragment
from plasTeX.DOM import Node
from xml.sax import saxutils

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

import os, sys
from xml.dom import minidom

def processIndex():
    # Bulk process a xml docuement
    out = open("/home/jamie/texsearch/index.ml","w")
    out.write("let index = Mtree.make_index \"")
    out.write("<results>")
    doc = minidom.parse("exs.xml")
    for item in doc.childNodes[0].childNodes:
        if item.nodeName == u'result':
            for eqnode in item.childNodes[1:]:
                textnode = eqnode.childNodes[0]
                tex = textnode.wholeText
                result = []
                renderer.render(processTeX("\\begin{document}"+tex+"\\end{document}"),result)
                out.write(json.dumps(result))
                out.write("\n")
    out.write("</results>")
    out.write("\"")
    out.close()

try:
    # Python 2.6
    import json
except:
    # Prior to 2.6 requires simplejson
    import simplejson as json

def queries():
    # 'for line in sys.stdin' won't work here
    line = sys.stdin.readline()
    while line:
        yield json.loads(line)
        line = sys.stdin.readline()

def respond(query):
    # Add response codes
    try:
      latex = query['query']['latex']
      response = []
      renderer.render(processTeX("\\begin{document}"+latex+"\\end{document}"),response)
      code = 200 # OK
    except KeyError:
      response = {'error':'Bad request'}
      code = 400 # Bad request
    except Exception, exception:
      response = {'error':unicode(exception)}
      code = 500 # Internal server error
    sys.stdout.write("%s\n" % json.dumps({'json':response, 'code':code}))
    sys.stdout.flush()

def main():
    for query in queries():
        respond(query)

if __name__ == "__main__":
    main()