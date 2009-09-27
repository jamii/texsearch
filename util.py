def encodeDoi(doi):
  return doi.replace("/","_")

def decodeDoi(doi):
  return doi.replace("_","/")
