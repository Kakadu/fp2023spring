def find_mismatches(d1, d2):
  mismatches  = []
  for (key, data) in d1.items():
    if data != d2[key]:
      mismatches.append(key)
  return mismatches

dict1 = { "brand": "Ford",  "model": "Mustang",  "year": 1964 }
dict2 = { "brand": "Toyota"
#,  "model": "Mustang",  "year": 1964
}

print(find_mismatches(dict1, dict2))