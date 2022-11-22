import System.Environment
main = do
    args <- getArgs
    --readFile (head args) >>= putStr . unlines . process'' . lines
    --readFile (head args) >>= putStr . show . process'''
    readFile (head args) >>= putStr . document

doc = "--hello\n" ++
      "--bye\n" ++
      "--bye\n" ++
      "--bye\n" ++
      "\n" ++
      "--bye\n" ++
      "hellobye\n" ++
      "hellobye\n" ++
      "hellobye\n" ++
      "hellobye\n" ++
      "--bye\n" ++
      "--bye\n" ++
      "--bye\n" ++
      "--bye\n" ++
      "see you"

data Category = Comment | Empty | Code
    deriving (Show, Eq)

process' :: [String] -> [(Category, String)]
process'  = map categLine

categLine :: String -> (Category, String)
categLine s | isComment s = (Comment, removeComment s)
            | isEmpty s = (Empty, s)
            | otherwise = (Code, s)

giveCateg :: (Category, String) -> Category
giveCateg (x,y) = x

isComment :: String -> Bool
isComment (x:y:xs) | [x,y] == "--" = True
                   | otherwise = False
isComment _ = False

isEmpty :: String -> Bool
isEmpty = (== "")

removeComment :: String -> String
removeComment l@(x:y:xs) = if [x,y] == "--" then xs else l
removeComment xs = xs

formBlocks :: [(Category, String)] -> [[(Category, String)]]
formBlocks [] = []
formBlocks xs = a: formBlocks b
    where
    cathead = giveCateg $ head xs
    (a, b) = break (\x -> (giveCateg x /= cathead)) xs

process'' :: String -> [[(Category, String)]]
process'' = formBlocks . process' . lines

type Block = (Category, [String])

process''' :: String -> [Block]
process''' = tagBlocks . process''

--document :: String -> [Documentation]
----
--document = markDownDoc . head  . mergeBlocks . process'''
document :: String -> String
document = markDownDocs . mergeBlocks . process'''

tagBlocks :: [[(Category, String)]] -> [Block]
tagBlocks = map tagBlock

tagBlock :: [(Category, String)] -> Block
tagBlock xs = (giveCateg $ head xs, map (\(a,b) -> b) xs)

data Documentation = CodeExplanation Block Block
    deriving Show

mergeBlocks :: [Block] -> [Documentation]
mergeBlocks (x:y:xs)
    | findcat x == Comment && findcat y == Code = (CodeExplanation x y) : mergeBlocks (y:xs)
    | otherwise = mergeBlocks (y:xs)
    where findcat = (\(x,y) -> x)
mergeBlocks (x:[]) = []
mergeBlocks []     = []

markDownDocs :: [Documentation] -> String
markDownDocs ds = concat $ map markDownDoc ds

markDownDoc :: Documentation -> String
markDownDoc (CodeExplanation explanation code) =
    makeMarkDownText (getData explanation) ++ space ++ makeCodeText (getData code)
    where
        getData = (\(x,y) -> y)
        space = "\n"

makeMarkDownText :: [String] -> String
makeMarkDownText ss =
    makeHeading (head ss) ++ (unlines $ tail ss)
    where
        makeHeading s = "## " ++ s

makeCodeText :: [String] -> String
makeCodeText ss =
    let
        top = "```Elm"
        bottom = "```"
    in
        unlines ([top] ++ ss ++ [bottom]) ++ "\n"
