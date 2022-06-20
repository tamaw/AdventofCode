{-# LANGUAGE OverloadedStrings, NamedFieldPuns, ViewPatterns #-}

import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Char (isAlpha)
import Data.HashMap.Strict (HashMap, singleton)
import qualified Data.HashMap.Strict as H

data Ingredient = Ingredient {
                                count :: Int,
                                element :: Text
                             }
                deriving Show

data Recipe = Recipe [Ingredient] Ingredient
                deriving Show

-- 2 NZGK, 9 XQJK, 18 WRKJ => 9 KTWV
-- walk result
readRecipes :: FilePath -> IO [Recipe]
readRecipes file = do
    recipesIo <- T.readFile file
    case parseRecipes recipesIo of
        Left err -> error err
        Right recipes -> return recipes

parseRecipes :: Text -> Either String [Recipe]
parseRecipes input = mapM (parseOnly $ parseRecipe <* endOfInput) $ T.lines input

parseIngredient :: Parser Ingredient
parseIngredient = do
    skipSpace
    n <- decimal <?> "count"
    skipSpace
    chemical <- takeWhile1 isAlpha <?> "element"
    return $ Ingredient n chemical

parseRecipe :: Parser Recipe
parseRecipe = do
    ingredients <- parseIngredient `sepBy` (char ',')
    skipSpace
    _ <- string "=>"
    reaction <- parseIngredient
    return $ Recipe ingredients reaction

recipeFor :: Text -> [Recipe] -> Recipe
recipeFor result recipes = case foundRecipes of
                            [recipe] -> recipe
                            _ ->  error "404 Recipe not found"
    where
        foundRecipes =  filter (\(Recipe _ (Ingredient { element })) -> element == result) recipes

walk :: Int -> [Recipe] -> Int
walk count recipes = go recipes $ singleton "FUEL" count
    where
        go rs toCook
            | H.null toCook = error "404 ORE not found"
            | [("ORE", n)] <- H.toList toCook = n
            | otherwise = cookNext

            where
                (cooked@(Ingredient demandN result), newBag) = removeFromBag rs toCook
                remainingRecipes = filter (\(Recipe _ (Ingredient _ e)) -> e /= result) rs
                cookNext = go remainingRecipes $ addToBag (map (scale timesToCook) ingredients) newBag
                Recipe ingredients (Ingredient producesN _) = recipeFor result rs
                timesToCook = demandN `div` producesN + (if demandN `mod` producesN > 0 then 1 else 0)
                scale m (Ingredient n e) = Ingredient (n * m) e

addToBag :: [Ingredient] -> HashMap Text Int -> HashMap Text Int
addToBag ingredients rs = H.unionWith (+) (H.fromList kvps) rs
    where
        kvps = map (\(Ingredient n e) -> (e, n)) $ ingredients

removeFromBag :: [Recipe] -> HashMap Text Int -> (Ingredient, HashMap Text Int)
removeFromBag rs b = (removeMe, H.delete result b)
    where
        removeMe = Ingredient demandN result
        -- ((result, demandN) : _) = filter (\(k, _) -> k /= "A") $ H.toList b
        --((result, demandN) : _) = H.toList b
        (result, demandN) = chooseNextIngredient rs b

-- Say the bag contains [("A", 7), ("E", 1)]
-- "E" is ready to cook - return ("E", 1)

chooseNextIngredient :: [Recipe] -> HashMap Text Int -> (Text, Int)
chooseNextIngredient rs bag = result
    where
        (result : _) = filter isReadyToCook $ H.toList bag
        isReadyToCook (element, _) = not $ any (recipeConsumes element) rs -- (\(Recipe ingredients product) rs -> any (\Ingredient c e) ingredients )
        recipeConsumes :: Text -> Recipe -> Bool
        recipeConsumes i (Recipe ingredients _) = any (\(Ingredient _ e) -> i == e) ingredients

part2 :: IO Int
part2 = do
    recipes <- readRecipes "day14.txt"
    let answer = find_ 0 oneTrillion (\c -> walk c recipes)
    return answer

find_ :: Int -> Int -> (Int -> Int) -> Int
find_ lowerBound upperBound f
    | lowerBound >= upperBound = current -1
    | actual <= max = find_ (current + 1) upperBound f
    | actual >= max = find_ lowerBound current f
    where
        actual :: Int
        actual = f current
        current :: Int
        current = (lowerBound + upperBound) `div` 2
        max = oneTrillion

oneTrillion :: Int
oneTrillion = 1000000000000

-- [Ingredient {count = 7, element = "A"},
-- Ingredient {count = 1, element = "E"},
-- Ingredient {count = 10, element = "ORE"},
-- Ingredient {count = 7, element = "A"},
-- Ingredient {count = 1, element = "D"},
-- Ingredient {count = 10, element = "ORE"},
-- Ingredient {count = 7, element = "A"},
-- Ingredient {count = 1, element = "C"},
-- Ingredient {count = 10, element = "ORE"},
-- Ingredient {count = 7, element = "A"},
-- Ingredient {count = 1, element = "B"},
-- Ingredient {count = 10, element = "ORE"},
-- Ingredient {count = 1, element = "ORE"}]

-- 1 FUEL
-- Remove 1 FUEL, add 7 A, 1 E
-- 1 FUEL -> 7 A, 1 E
-- Remove 1 E, add 7 A, 1 D
-- 7 A, 1 E -> 14 A, 1 D
-- Remove 1 E, add 7 A, 1 C
-- 14 A, 1 D -> 21 A, 1 C
-- 21 A, 1 C -> 28 A, 1 B
-- 28 A, 1 B -> 29 A
-- 29 A -> 30 ORE
-- "FUEL" -> Recipe [7 A, 1 E] (1, FUEL)
-- "A" -> Recipe [10 ORE] (10, A)

exampleRecipe :: [Recipe]
exampleRecipe  = case fmap concat $ mapM parseRecipes eg2 of
        Left err -> error err
        Right recipes -> recipes
    where
        eg1 :: [Text]
        eg1 =
            [
                "10 ORE => 10 A",
                "1 ORE => 1 B",
                "7 A, 1 B => 1 C",
                "7 A, 1 C => 1 D",
                "7 A, 1 D => 1 E",
                "7 A, 1 E => 1 FUEL"
            ]
        eg2 =
            [   "9 ORE => 2 A",
                "8 ORE => 3 B",
                "7 ORE => 5 C",
                "3 A, 4 B => 1 AB",
                "5 B, 7 C => 1 BC",
                "4 C, 1 A => 1 CA",
                "2 AB, 3 BC, 4 CA => 1 FUEL"
            ]
