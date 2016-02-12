import Check exposing (..)
import Check.Investigator exposing (
  Investigator, investigator, rangeInt, float,
  char, lowerCaseChar, upperCaseChar, list
  )
import SafeParser exposing (parse, separatedBy, symbol, getParseResult)
import SafeParser.Char as PC
import SafeParser.Number as PN
import Result.Extra exposing (isOk)
import Shrink
import String
import Graphics.Element exposing (Element, show, down, flow)

parserSuite =
  suite "Parser"
    [ claim
        "Digit parsing"
      `that`
        (getParseResult PN.digit << toString)
      `is`
        Ok
      `for`
        (rangeInt 0 9)
    , claim
        "Natural parsing"
      `that`
        (getParseResult PN.natural << toString)
      `is`
        Ok
      `for`
        (rangeInt 0 1000000)
    , claim
        "Integer parsing"
      `that`
        (getParseResult PN.integer << toString)
      `is`
        Ok
      `for`
        (rangeInt -1000000 1000000)
    , claim
        "Float parsing"
      `that`
        (getParseResult PN.float << (\n -> let s = toString n
                                  in if String.contains "." s
                                        then s
                                        else s ++ ".0"))
      `is`
        Ok
      `for`
        float
    , claim
        "Lower parsing"
      `true`
        (isOk << getParseResult PC.lower << String.fromChar)
      `for`
        lowerCaseChar
    , claim
        "Lower parsing"
      `false`
        (isOk << getParseResult PC.lower << String.fromChar)
      `for`
        upperCaseChar
    , claim
        "Upper parsing"
      `true`
        (isOk << getParseResult PC.upper << String.fromChar)
      `for`
        upperCaseChar
    , claim
        "Upper parsing"
      `false`
        (isOk << getParseResult PC.upper << String.fromChar)
      `for`
        lowerCaseChar
    , claim
        "List"
      `true`
        (isOk << getParseResult (PC.bracketed (separatedBy PN.integer (symbol ','))) << toString)
       `for`
       list (rangeInt -1000000000 10000000000)
    ]

result =
    quickCheck parserSuite

main =
    display result


display : Evidence -> Element
display evidence =
  case evidence of
  Unit unitEvidence ->
    show unitEvidence
  Multiple name evidences ->
    flow down (List.map show evidences)
