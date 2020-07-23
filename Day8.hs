{-# LANGUAGE OverloadedStrings #-}

import Data.List (minimumBy, foldl1')
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

img :: Text
img = "221002222122020222021022222022222222222222222222221222222022220222112022220222022020222202222222220222222222221222212222222222222222222222222222022222220202222122121222022122222122222222222222222222222222222022221222102022221222122220222202222222220222222222222222202222222222222222222222222222222222221112222222220222021222222222222222222222222222220222222122221222122122221222222221222202222222220222222222220222202222222222222222222222222222222222222212222222220222221122222222222222222222222202221222222022221222002222220222022020222222222222222222222222220222202022222222222222222222222222222222222002222122221222020122222022222222222212222222222222022222220222002022220222222122222212222222222222222222222222212022222222222222222222222222122222221222222222220222122222222022222222222212222202220222122122222222012122220222022020222212222122221222222222221222222122222222222222222222222222222222222122222022120222022222222122222222222212222202222222222122220222102222221122122120222202222122221222222222222222202022202222222222222222222222022222222212222022220222120122222122222222222212222212220222222222221222222022220122222120222202222022221222222222220222212122222222222222222222222222022222220112222022120222121022222222222221222222222222220222122122220222222122220102222120222202222122222222222222222222212222222222222222222222122222022222222122222222121222222222222222222221222222222222220222222122221222112222220212022022222212222122021222222222221222222122212222222222222222122222122222222122222022221222222122222222222221222212222212221220022122220222202022222102222222222212222022220222222222220222212022212222222222222222022222022222222022222022222222120222222022222222222202222202222220122022222222022122221222202020222212222122221222222222221222222222222222222222222222022222122222220212222122222222122222222222222222222202222222221220222222211222022022222202222122222212222122021222222222220222222122202222222222222222122222222222222202222122122222120022222222222222222212222222220222222022202222222122222202012122222212222022022222222222222222222222202222222222222222022222022222220002222222122222120122222122222222222202222212222222122022202222102122220000222122222202222122120222222222222222222122212222222222222222022222122222221102222222121222222022220122222221222212212202221220222122201222002222220210012120222222222022222222222222222022222122222222222222222222222222122222222102222022122222121222220122222221222212212222221220022022212222112022220220112120222212222122021222222222222022222022202222222222222222222222122222222212222122022222122122222122222221222202212222221221122022201222022222221021102020222222222022020222222222222022222022212222222222222222222222022222220222222122222222220022221122222220222202222222220221022222222222122122220100122221222212222222021222222222221022212122222222222222222222122222122222221212222122120222222222221122222222222222212212221222222022220222022222222120112021222222222022022222222222221222222022222222222222222222122222022222220012222222122222120022220222222222222202202212222222122222202222122022222022022121222222222222122222222222222222212222202222222222222222222222222222222122222222021222222022220122222222222222212202220220022122201222012022221202022121222222222022222222222222222222222222222222222222222222122222222222221012222122020222121222222222202220222202222212222201122122221222112122220202002122222212222222221222022222220022212022212222222222222222122222222222220202222222120222220122221022202220222212202212221210022122211222002122222112212220222212222222221222122222221022222222202222222222222222222222222222221002222122020222121222220022212221222202212202221200022120220222102022220201122122222222222022022222022222021022202022222222222222122222122222122222220022222122020222121122220122202222222212212222222200122022212222222122222121102021222202222022020222022222021222202122222222222222122222022222222222220122222222221222021022222022222222222202212202220221122022200222202222221202112022222222222022020222022222020122212222222222222222122222022222222220221022222222020222121022221022212220222222212222220210122120220222212222222112002022222202222122022222222222022122220022222222220222122222122222222220221222222222122222221222221122202222222212202212221200022220210222012222220122212221222212222122022222022222221022201222212222222222122222222222022221221002222122020222220222222222202222222222202222221211022022202222122022220122022121222212222222221222122222222222200122202022221222022222022222222222220202222222020222022022221122222221222222202222220202022222221222202122221021202122222222222222122222022222022222221022212022220222022222022222122220221002222122121222122222222222222222222200222202221200022221212222012022222202012122222222222020221222022222221222200022222122220222222222222222222221221022222122122222120122220122202222222211202212221200222120201222012022220020012020222212222021220222222222121022201022212122220222222222022222122221221112222022122222220022222022212220222212212202220002222220220222102222222201212011222222222122022222122222021022211222202222220222022222222222122222220122222022221222120222221022202221222221222202222000022220200222112122221122202020222212222021120222022222020122210122212122222222120222122222222222221012222122222122120122220022212222222212212202222101122211202222122222220102212102222202222122021222222222120122202022212122221222121222022222122222221212222222222022021122222222212221222200202202222021022202210222102022220211002222222212222021121222222222122122210122222122220222220222121222222202220212222122222022022122220022222220222202222222220210122202212222222121212202112210222202222222022222022222221122201022022122220222022222121222022201222002222122222122120022222222222220222221202212222001122111202222002120220212022000222202222121020222022222021022220022122022220222020222222222022200220012222122022222121222222222222222222201222222221000222222212222122020221201022121222202222120121222222222221222202222002022220222221222020222022211022112222022220022221122221022222221222220212222221022022000221222012020210200112102222220222221220222222222221222222222202222220222122222200222122220220222222022020112022122221122222221222222212222222211022110220222212220202221202020212201122022021222022222220022222122122022220222220222222222122211221222222222121012121022220222212221222211202222221120022102202222222021221210202212202212222120022222022222020222220222122122220222120222211222222221222112222222210002021222221022202221222201212202221220202222202222102221200202102111212202122221220222022222020222200222112122212222122222100222122220220212222222122022120022221022222221222220222212220112122100201222022222200100002222222212222020122222222222222022210222122222211222221222222222222202022012222222202212220222120022222221222022202222220112002102202222222022221002122102212222022020020222022222122122212222022022212222022222120222022212112022222122111122122222121222202221222020212202220012222111221222112120210121122220212202222220022022122222220222222022222122212222221222201222122201012122222122210202122022020222202220222102222222220010202200221022012120210002122010212210022122222022122222222022201222002222221222222222011222122212121012222122202112221022220120202220222122202210222220212121210222012121210001102012222200122120121122122222022022201122102122200222220222111222022200022212222022002112221122222021222222222220202212220120012200212122122220220011022102202210022021022022022222220122210222122122212222120222120222222211111222222222102012221222221221212222222011202222220220122200201122112122210212202100202202222222122122022222222222211022012122222222222222122222022211002202222222212202021122222220212022222010222210220011022200222222222221220111102011202220022221221122022222020122201222002222221222122222102222222211000212222122002022121022120020212220222121212202221202012102221122202022221002212010202200122220122222222221121222220222222122201222222222022222222220122112222122201002122022220220202122222121212220220111012211222022002021220020022112202202022222121122222220221222212222112122220222122222012222122212101122222022021022020022121220202221222112202202220101022100220222202021222122212001202222022222222022222221222222202122202222221222222222110222122212021102222222221202221122122021222022222202202200222112222000221022112021210011022222222202022020122122222221220022200122222120202222121222222222222202102222222222110122021022022021202120222112212210222110202011202022012022221100102221202220122022022022122220022222200222202220210222221222121222022200212122222222001122021022021121222021222102212211221000022021220122022222220021002201222201222221221222122221020222200022202020202222022222212222022201012102222222012212220122221020212222222022212211220211202210212122222020011200102001222211122220122122222222220022222122212221202222221222000222220222202122222222020022020022021021202022222122222201222001012010202022102122220022122201212200122120020022122221222022212022222020202222121222121222122200200212222022200012220022120122202120222102222202221212122110212222022020022211102102222220222022020222122222121022220022102220200222120222221222020201010122222122002212021222021122222020222210202201222122102110221122012221111102022112202220222122221122222220220222212222112220211202022222210222020200002212222122020112022022221122202221222021212120222222021011210122212120220121022110212220222120022022122220220122221122012122202202221222102222221221212222222222010012220122221122222120222221202010220112020112211122222021020010212011222222222120122122122221222022211222212122210202220222110222120202120012222122222202222122022021212022222202212222222212102011220122002221000202122100212220122021121220222222022122202222022120211212020222211222002211000012222022001122022122120021222222222011222010222110210200220122222021011000222020212201222022222120222221222022201122112021202222222222020222101200022212222222001102222122222021222020222120202210222110020020210222212222210011012022212221122222220120022222121022211022102121211222021222111222112220002012222222102022020022121120202122222021222022222000000202200022022022122221122112222220022022220221022222022122220022022221222222020222212222202220000102222222120122120222220221222021222220202112220101111222201222212220102121012111202210022120120120222221020022221022012122220212120222120222202222022102222022102112121222121021102222222202222111221022202111201022012120012220102001222202120222122220122221020222211022002020211212121222110222020211021002222222100222221202110222022222222111222020220220211022211222022021200022012021212222222221021120022222020222211022202120220202020222201222201212222022222022000002220212110220022020222121212121222002212211221022002222112110122112212200120022220220122222222222221222222022211222021222210222220221221122222022112012220202212121222212222210212120222220221020212122222222122212212111202210221221222121222220220122222022022121200222122222201222100211122202222122111202122202121221002012222010202211220020010102212222112022201002112121202202120220220121022221020012212222112220200202022222220222110222110012222022010122221122200012212200222110212010222111221211110122212221202221012022222202221222021120222222121102212122221021221220120222211222111222220202222222212202121012021010202010222020222210221221200010111122202022020220222011202212022020120221222222021002202122020221201210022222112222222211001212222222120022222122102112012022222022212002220221010212121222112121020121010101202221220122222122222222222212220022012220202212220222220222211222112202222222100102221212200001102210222122222000221001001222110222122122211001200120222220122020120222122021120212211222102120200210222222011222021222101202222022021212220122000210112112222021202212222102200220101222102020210200211221222220121120021222022222220002201122121121220211221222202222112200122022222022201222220202122112012002222212202010220210201202212122022221121001220020222202222120120221222121021102202122112222210202120222101222001210010122222022210122120112111101212110222110212221222120000201021022122222102221212001222211122220020220222121122202210222001021221211020222222220121211210022222022220222221122100020112011222012212002221111102011102202002220212221002000202201121020221122222020221122200222212120201200222222222221112200201102222022011102021022021101002111212222202221221100122022101012002122212011100121222202022021022221222020021112202022000120211220020220110220000212001222222222000012222222112210222022212010202210222200012222112212102221221001200101222221222221121220222220120002220222002221210220121220102220122201020212222120002222120212012110102120202020212100220100001010111102202222111220202200212202220120020221022221020112212022120121211200121222001222020201101012222122002022121012201021112122202221202202221101120120212021002120122212100220222212222121120022022120222102201222020020210202120220000220101221211002222220121012121202211222222112222110202001221010100001200122022120201002210002212200222120021021122221121122211022202220202201220221012220210200020102222022010212220112122021222010202101202222222121121202202222202222220211020012212200222221021020122120021202210022020102200212022222211222122211201112222120221202220222212002122000202012222212221021001000222202012222011001021211222111021222122022212100221112201122220202200221222222210220212222202202022222212012020122011111012020212102222010221211121111102211022121212202000011212202222220222021112120020222220222020021200220220221112221020222010012022122000112000112211210122001202111222121220012000020021110212220210100211012222011021122122020202100221012202022022202220201120220000222120200221202122020221022120222202121112100212120212102221122021001120022102021201022110000212200022122221020222000120022202122111212211201221220101220222211021212022211200222011112012221022120202102212122022012201010011000222021211022110222202221021220020020022020220012210122120022201222120222211222122220121112122120000222022102022112222222212012212122122021200222200010202221202000222000202012022022020222002010021022221022022010221202220220020222222200220122122200022022221012122220002212202201202200021001221210111000022222201110221022220020122120022021102201121102220122211201210212122220011120100221212122122220011112120112112110112012202120202210222002112200012011222022011211021201220221121221122022202122101002222222122201201201122222102212221211201222022020102002010212221020022210202110202102020002110222212012222122100122010012022211222021122121202001110222210022211110212202220221212102112202010112222120102112121112121102012221222212022020021000012022111001012221002021022201012100220021120120022220222002221022210120212211002220100200010202010002222212020002112002111212012001222012122222111010001000102000122021020111122010100000221121020221122201000112210022112012222211122220000102200200122211000120212021002220110220200210010001220022100210112221200210200100122202211202002112202002110211210012202100021210110012101012021101101212002112"

width = 25
height = 6

mergePixel :: Char -> Char -> Char
mergePixel '2' c = c
mergePixel a _ = a

mergeLayer :: Text -> Text -> Text
mergeLayer = Text.zipWith mergePixel 

mergeLayers :: [Text] -> Text
mergeLayers = foldl1' mergeLayer

draw :: Text -> IO ()
draw layer = mapM_ Text.putStrLn lines
    where
        lines = Text.chunksOf width (Text.map drawPixel layer)

drawPixel :: Char -> Char
drawPixel '0' = ' '
drawPixel '1' = '■'

layers :: Text -> [Text]
layers = Text.chunksOf (width*height)

fewest0 :: [Text] -> Text
fewest0 = minimumBy cmp
    where 
        cmp a b = Text.count "0" a `compare` Text.count "0" b

findCorruption :: Text -> Int
findCorruption layer = (Text.count "1" layer) * (Text.count "2" layer)

part1 :: Text -> Int
part1 input = findCorruption . fewest0 $ layers input

part2 :: IO ()
part2 = draw $ mergeLayers $ layers img
