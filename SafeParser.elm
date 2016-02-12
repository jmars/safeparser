module SafeParser where

{-| A simple parser combinator library.

@docs Parser, ParseResult

#Running the parser
@docs parse, getParseResult

#Core functions
@docs map, or, andMap, andThen, and

#Combinators
@docs succeed, satisfy, empty, symbol, token, choice, optional, many, some, separatedBy, end

#Writing recursive grammars
@docs recursively

#Core functions (infix operators)
@docs (<*), (*>), (<$)

#Error handling
@docs error

-}

import Lazy as L
import String

{-| Parser type -}
type ParseResult result
  = Fail Int String (List String)
  | Done String result

{-| Parser type -}
type Parser result
  = Direct (String -> ParseResult result)
  | Delayed (L.Lazy (String -> ParseResult result))

{-| Parse a `String` using a parser, return first result -}
parse : Parser result -> String -> ParseResult result
parse p =
  case p of
    Direct f -> f
    Delayed d -> L.force d

{-| For realizing otherwise inexpressible recursive grammars. For
example, while

    bbbba = (symbol 'a') `or` (symbol 'b' *> bbbba)

will fail at runtime with a non-termination issue, the replacement

    bbbba = (symbol 'a') `or` (symbol 'b' *> recursively (\() -> bbbba))

is safe.
-}
recursively : (() -> Parser result) -> Parser result
recursively t = Delayed << L.lazy <| \() -> parse (t ())

{-| Parse a `String` using a parser, return first result -}
getParseResult : Parser result -> String -> Result String result
getParseResult p xs =
  case parse p xs of
    Fail line rest err -> Err <|
      toString err ++ "\n\nAt line: " ++ toString line ++ "\n\nRemaining: " ++ rest
    Done _ result -> Ok result

{-| Parser that always succeeds without consuming input -}
succeed : result -> Parser result
succeed b = Direct <| \xs -> Done xs b

{-| Parser that satisfies a given predicate -}
satisfy : (Char -> Bool) -> Parser Char
satisfy p = Direct <| \xs ->
  case String.uncons xs of
    Nothing -> Fail 0 "" ["Expected a token, not EOF."]
    Just (x, xs') ->
      if p x
      then Done xs' x
      else Fail 0 xs' ["Expected a char, got something else."]

{-| Parser that always fails -}
empty : Parser result
empty = Direct (\ xs -> Fail 0 xs ["Always failing parser."])

{-| Parses a symbol -}
error : Parser a -> String -> Parser a
error p e = Direct <| \xs ->
  case parse p xs of
    Fail line rest err -> Fail line rest (e :: err)
    a -> a

{-| Parses a symbol -}
symbol : Char -> Parser Char
symbol c =
  satisfy (\ x -> x == c )
  `error`
  "Expected '" ++ String.fromChar c ++ "' but got something else."

{-| Map a function over the result of the parser
      -- Counts the amount of digits
      count : Parser Int
      count = map length (many digit)
-}
map : (result -> result2) -> Parser result -> Parser result2
map f p = Direct <| \ xs ->
  case parse p xs of
    Done line result -> Done line (f result)
    Fail line rest err -> Fail line rest err

{-| Sequence two parsers

    type Date = Date Int Int Int
    date =
        map Date year
        |> andMap month
        |> andMap day
-}
andMap : Parser result -> Parser (result -> result2) -> Parser result2
andMap q p = Direct <| \xs ->
  case parse p xs of
    Done rest f ->
      case parse q rest of
        Done rest result -> Done rest (f result)
        Fail line rest err -> Fail line rest err
    Fail line rest err -> Fail line rest err

{-| Choice between two parsers

      oneOrTwo = symbol '1' `or` symbol '2'
-}
or : Parser result -> Parser result -> Parser result
or p q = Direct <| \xs ->
  case parse p xs of
    Done rest result -> Done rest result
    Fail line rest err -> parse (error q "or") xs

{-| Sequence two parsers (infix version)

    type Date = Date Int Int Int
    date = Date `map` year `and` month `and` day
-}
and : Parser (result -> result2) -> Parser result -> Parser result2
and p q = (error p "and") |> andMap (error q "and")

{-| Combine a list of parsers -}
choice : List (Parser result) -> Parser result
choice ps = error (List.foldr or empty ps) "choice"

{-| Parses an optional element -}
optional : Parser result -> result -> Parser result
optional p x = error (p `or` succeed x) "optional"

{-| Parses one or more occurences of a parser -}
some : Parser result -> Parser (List result)
some p = flip error "some" <| Direct <| \xs ->
  case parse p xs of
    Done rest result -> map ((::) result) (many p) |> flip parse rest
    Fail line rest' err -> Fail line xs err

{-| Parses zero or more occurences of a parser -}
many : Parser result -> Parser (List result)
many p =
  let
  many' rest acc =
    case parse p rest of
      Done rest result -> many' rest (result :: acc)
      Fail line rest' err -> Done rest (List.reverse acc)
  in
  error (Direct <| \xs -> many' xs []) "many"

{-| Sequence two parsers, but pass the result of the first parser to the second parser.
    This is useful for creating context sensitive parsers like XML.

    tag = openTag
        |> andThen (tagLiteral)
-}
andThen : Parser result -> (result -> Parser result2) -> Parser result2
andThen p f = flip error "andThen" <| Direct <| \xs ->
  case parse p xs of
    Done rest result -> parse (f result) rest
    Fail line rest err -> Fail line rest err

{-| Succeeds when input is empty -}
end : Parser ()
end = Direct <| \xs ->
  case xs of
    "" -> parse (succeed ()) xs
    _  -> Fail 0 xs ["Expected end of input, got: " ++ xs]

{-| Parses a sequence of the first parser, separated by the second parser
```
naturals = separatedBy Number.natural (symbol ',')
```
 -}
separatedBy : Parser result -> Parser result2 -> Parser (List result)
separatedBy p s = many (p <* (andMap s |> \ _ -> succeed ()))
  `error`
  "separatedBy"

{-| Parses a token of symbols -}
token : String -> Parser String
token xs =
  case String.uncons xs of
    Nothing -> succeed ""
    Just (x, xs) ->
      symbol x
      |> map String.cons
      |> andMap (token xs)
      `error`
      "Expected: " ++ xs ++ ", got something else."

{-| Variant of `map` that ignores the result of the parser -}
(<$) : result -> Parser x -> Parser result
(<$) f p = map (always f) p

{-| Variant of `and` that ignores the result of the parser at the right -}
(<*) : Parser result -> Parser x -> Parser result
(<*) p q = map always p |> andMap q

{-| Variant of `and` that ignores the result of the parser at the left -}
(*>) : Parser x -> Parser result -> Parser result
(*>) p q = map (flip always) p |> andMap q

infixl 0 `error`
infixl 4 `and`
infixr 3 `or`
infixl 4 `map`