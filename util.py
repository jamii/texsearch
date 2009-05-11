class UnexpectedResponse(Exception):
  __init__(self,expected,got):
    self.expected = expected
    self.got = got

  __str__(self):
    return "UnexpectedResponse: Expected code %s, got code %s"

def expectResponse(conn,code):
  response = conn.getresponse()
  if response.status != code:
    print "Unexpected response from database"
    raise UnexpectedResponse()
  return response.read()