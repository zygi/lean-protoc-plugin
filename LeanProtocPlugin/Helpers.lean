import AssertCmd

def String.toCamelCase (s: String) (firstCap : Bool) : String := do
  let mut newString := ""
  let mut sawUnderscoreBefore := false
  for idx in [:s.length] do
    if s[idx] == '_' then
      sawUnderscoreBefore := true
      continue
    if s[idx].isLower && (sawUnderscoreBefore || (idx == 0 && firstCap)) then
      newString := newString.push s[idx].toUpper
    else
      newString := newString.push s[idx]
    
    sawUnderscoreBefore := false
  return newString

#assert (String.toCamelCase "_one_two__three_" true) == "OneTwoThree"
#assert (String.toCamelCase "one_two__three_" true) == "OneTwoThree"
#assert (String.toCamelCase "one_two__three_" false) == "oneTwoThree"

def filePathToPackage (s: String) : String :=
  let parts := (s.splitOn "/").map (·.toCamelCase true)
  parts.foldl (fun r s => r ++ s) "."