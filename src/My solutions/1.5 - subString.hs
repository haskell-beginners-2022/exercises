subString :: Int -> Int -> String -> String
subString start end str = 
  let safeStart = max start 0 
      safeEnd   = max (end + 1) 0
  in take (safeEnd - safeStart) (drop safeStart str)