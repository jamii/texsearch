#!/bin/env python

""" Runs regression tests against texsearch and reports failures by email. In production is run by cron every 15 minutes """

import urllib
from xml.dom import minidom
import smtplib
from email.MIMEMultipart import MIMEMultipart
from email.MIMEText import MIMEText
import time
import os.path as path
from popen2 import popen2

ports = [5985, 5984]
searchTerms = open("searchTerms").read().splitlines()

user = 'latex_operations@springer.com'

# Reporting levels
errorGroup = ['jamiiecb@googlemail.com'] # ['latex_operations@springer.com']
infoGroup = ['jamiiecb@googlemail.com']

def searchURL(port,searchTerm):
  return ("http://localhost:%d/documents/_external/index?searchTerm=%s&precision=0.66&limit=10000" % (port,urllib.quote(searchTerm)))

def countResults(resultString):
  try:
    dom = minidom.parseString(resultString)
    if dom.getElementsByTagName("results"):
      results = dom.getElementsByTagName("result") + dom.getElementsByTagName("Chapter") + dom.getElementsByTagName("Article")
      return len(results)
  except Exception, e:
    pass

  # Not a correct result string
  return None

def readResults(port, i):
  file = open(("%s/%s" % (port, i)), 'r')
  results = int(file.read())
  file.close()

  return results

def writeResults(port, i, results):
  file = open(("%s/%s" % (port, i)), 'w')
  file.write(str(results))
  file.close

def init():
  for port in ports:
    for i in range(0,len(searchTerms)):
      url = searchURL(port, searchTerms[i])
      resultString = urllib.urlopen(url).read()
      results = countResults(resultString)
      writeResults(port, i, results)

def test():
  info = []
  errors = []

  for port in ports:
    for i in range(0,len(searchTerms)):
      url = searchURL(port, searchTerms[i])
      
      try:
        resultString = urllib.urlopen(url).read()
        results = countResults(resultString)
        expectedResults = readResults(port, i)
        if results == None:
          # Didnt get a correct result string
          errors.append(("Url: %s\n%s" % (url, resultString)))
        elif results == expectedResults:
          # Uninteresting
          pass
        elif results > expectedResults:
          # No of results may increase when adding content
          writeResults(port, i, results) 
          info.append(("Url: %s\nExpected %d results, got %d results" % (url, expectedResults, results)))
        else:
          # No of results should never decrease
          writeResults(port, i, results) 
          errors.append(("Url: %s\nExpected %d results, got %d results" % (url, expectedResults, results)))
      except Exception, e:
        # Most likely connection refused or http 500
        errors.append(("Url: %s\n%s" % (url, str(e))))

  return (info, errors)

def mail(to, subject, text):
  print subject
  print text

  msg = MIMEMultipart()
  msg['From'] = user
  msg['To'] = to
  msg['Subject'] = subject
  msg.attach(MIMEText(text))

  mailServer = smtplib.SMTP('smtp.springer-sbm.com')
  mailServer.sendmail(user, to, msg.as_string())
  mailServer.close()

def top():
  pout, pin = popen2("top -b -n 1")
  return pout.read()

def reportErrors(errors):
  subject = ("TeXsearch error report: %s" % time.asctime())
  text = "\n\n".join(errors + [top()])
  for e in errorGroup:  
    mail(e, subject, text)

def reportInfo(info):
  subject = ("TeXsearch info report: %s" % time.asctime())
  text = "\n\n".join(info + [top()])
  for i in infoGroup:
    mail(i, subject, text)
    
import sys, getopt

if __name__ == '__main__':
  opts, args = getopt.getopt(sys.argv[1:], "", ["init","test"])
  for opt, arg in opts:
    if opt == "--init":
      init()
    if opt == "--test":
      info, errors = test()
      if errors:
        reportErrors(errors)
      if info:
        reportInfo(info)
