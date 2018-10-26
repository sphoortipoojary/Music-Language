import Music

test f = do
  putStrLn $ "***Testing " ++ f
  showParsedExp f
  runFile f

main :: IO ()
main = do
   test "Inputmusic.txt"
