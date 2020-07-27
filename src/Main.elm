module Main exposing (main, intValue)
import Browser
import Html exposing (Html, button, div, form, input, text, li)
import Html.Attributes exposing (placeholder, value, type_, disabled, class, id)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Lazy exposing (lazy, lazy2)
import Html.Keyed exposing (ul)


-- MAIN

main : Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type alias Solution = List Int

type alias Result =
  {
    sum : Int,
    numCells : Int,
    requiredDigits : List Int,
    forbiddenDigits : List Int,
    solutions : List (Bool, Solution)
  }

type alias Results = List (Int, Result)

type alias UI =
  {
    sumField : String,
    numCellsField : String,
    requiredDigitsField : String,
    forbiddenDigitsField : String
  }

setSumField : String -> UI -> UI
setSumField s ui = { ui | sumField = s }
setNumCellsField : String -> UI -> UI
setNumCellsField s ui = { ui | numCellsField = s }
setRequiredDigitsField : String -> UI -> UI
setRequiredDigitsField s ui = { ui | requiredDigitsField = s }
setForbiddenDigitsField : String -> UI -> UI
setForbiddenDigitsField s ui = { ui | forbiddenDigitsField = s }

uiIsInvalid : UI -> Bool
uiIsInvalid ui =
  String.isEmpty ui.sumField ||
  String.isEmpty (String.trim ui.numCellsField)

type alias Model =
  {
    ui : UI,
    results : Results,
    nextResult : Int
  }

modifyUI : (UI -> UI) -> Model -> Model
modifyUI transform model = { model | ui = transform model.ui }

modifyResults : (Results -> Results) -> Model -> Model
modifyResults transform model = { model | results = transform model.results }

toggleResultSolution : Int -> Solution -> Results -> Results
toggleResultSolution num sol results =
  case results of
    [] -> []
    (n, r)::rs ->
      if num == n
      then (num, toggleSolution sol r) :: rs
      else (n, r)::toggleResultSolution num sol rs


toggleSolution : Solution -> Result -> Result
toggleSolution sol result =
  let toggle (active, s) = if sol == s then (not active, s) else (active, s)
  in
    { result | solutions = List.map toggle result.solutions }

addResult : Maybe Result -> Model -> Model
addResult result model = 
  case result of
    Just r -> { model | results = (model.nextResult, r)::model.results,
                        nextResult = model.nextResult + 1 }
    Nothing -> model
initialUI : UI
initialUI = UI "" "" "" ""

init : Model
init =
  {
    ui = initialUI,
    results = [],
    nextResult = 0
  }

intValue : String -> Int
intValue s = String.toInt s |> Maybe.withDefault 0

--parseNumCells : String -> List Int
--parseNumCells s = String.words s |> List.map intValue
parseNumCells : String -> Int
parseNumCells s = intValue s

parseDigits : String -> List Int
parseDigits s =
  String.toList s
  |> List.sort -- It should already be sorted.
  |> List.map (\c -> Char.toCode c - Char.toCode '0')



computeResult : UI -> Maybe Result
computeResult ui = 
  if uiIsInvalid ui
  then Nothing
  else
    let sum = intValue ui.sumField
        numCells = parseNumCells ui.numCellsField
        requiredDigits = parseDigits ui.requiredDigitsField
        forbiddenDigits = parseDigits ui.forbiddenDigitsField
        sols = computeSolutions sum numCells requiredDigits forbiddenDigits
    in Just
      {
        sum = sum,
        numCells = numCells,
        requiredDigits = requiredDigits,
        forbiddenDigits = forbiddenDigits,
        solutions = List.map (\sol -> (True, sol)) sols
    }

computeSolutions : Int -> Int -> List Int -> List Int -> List Solution
computeSolutions totalSum totalCells requiredDigits forbiddenDigits =
  if List.any (\d -> List.member d forbiddenDigits) requiredDigits
  then []
  else
    let initialCandidates =
          List.filter (\d -> not (List.member d requiredDigits || List.member d forbiddenDigits))
            (List.range 1 9)
        initialSum = totalSum - List.sum requiredDigits
        initialCells = totalCells - List.length requiredDigits
        f sum cells candidates =
          if cells == 0 then
            if sum == 0 then [[]] else []
          else if cells == 1 then
            if List.member sum candidates then [[sum]] else []
          else if sum <= 0 then
            []
          else
            case candidates of
              first::rest ->
                if first >= sum then
                  []
                else
                let withHead = f (sum - first) (cells - 1) rest
                    withoutHead = f sum cells rest
                in
                   List.map (\sol -> first::sol) withHead ++ withoutHead
              _ -> []
    in
      List.map (\sol -> List.sort (requiredDigits ++ sol))
        (f initialSum initialCells initialCandidates)




-- UPDATE

type Msg
  = ChangeSum String
  | ChangeNumCells String
  | ChangeRequiredDigits String
  | ChangeForbiddenDigits String
  | Compute
  | ClearUI
  | ToggleSolution Int Solution

positiveDigits : String -> String
positiveDigits = String.filter (\c -> Char.isDigit c && c /= '0')

toggleDigits : List Char -> List Char
toggleDigits lst =
  case lst of
    x::y::rest -> if x == y
                  then toggleDigits rest
                  else x::toggleDigits (y::rest)
    _ -> lst

digitsFieldFilter : String -> String
digitsFieldFilter str =
  positiveDigits str |> String.toList
                     |> List.sort
                     |> toggleDigits
                     |> String.fromList

intFieldFilter : String -> String
intFieldFilter s = String.filter Char.isDigit s

--intsFieldFilter : String -> String
--intsFieldFilter s = String.filter (\c -> Char.isDigit c || c == ' ') s

update : Msg -> Model -> Model
update msg model =
  case msg of
    ChangeSum newContent ->
      modifyUI (setSumField <| intFieldFilter newContent) model
    ChangeNumCells newContent ->
      modifyUI (setNumCellsField <| intFieldFilter newContent) model
    ChangeRequiredDigits newContent ->
      modifyUI (setRequiredDigitsField <| digitsFieldFilter newContent) model
    ChangeForbiddenDigits newContent ->
      modifyUI (setForbiddenDigitsField <| digitsFieldFilter newContent) model
    Compute -> addResult (computeResult model.ui) model
    ClearUI -> { model | ui = initialUI }
    ToggleSolution num sol -> modifyResults (toggleResultSolution num sol) model

-- VIEW

viewUI : UI -> Html Msg
viewUI ui =
  form [onSubmit Compute]
    (List.intersperse (text " ")
      [
        input [placeholder "Sum", value ui.sumField, onInput ChangeSum] [],
        input [placeholder "Cage size(s)", value ui.numCellsField, onInput ChangeNumCells] [],
        input [placeholder "Required digits", value ui.requiredDigitsField, onInput ChangeRequiredDigits] [],
        input [placeholder "Forbidden digits", value ui.forbiddenDigitsField, onInput ChangeForbiddenDigits] [],
        button [type_ "submit", disabled (uiIsInvalid ui)] [text "Compute"],
        button [type_ "button", onClick ClearUI] [text "Clear"]
      ]
    )

digitsString : List Int -> String
digitsString ds = String.join "" (List.map String.fromInt ds)

viewResults : List (Int, Result) -> Html Msg
viewResults results =
  ul [id "results"]
    (List.map (\(num, result) -> (String.fromInt num, lazy2 viewResult num result)) results)

viewResult : Int -> Result -> Html Msg
viewResult num result =
  let contents =
        String.fromInt result.sum ++ " in "
        ++ String.fromInt result.numCells ++ " cells"
        ++ (if List.isEmpty result.requiredDigits
            then ""
            else " using " ++ digitsString result.requiredDigits)
        ++ (if List.isEmpty result.forbiddenDigits
            then ""
            else " without " ++ digitsString result.forbiddenDigits)
        ++ ":"
      sols = List.map (\(active, sol) ->
                        Html.span
                          [
                            onClick (ToggleSolution num sol),
                            class (if active then "active" else "inactive")
                          ]
                          [text (digitsString sol)])
                result.solutions
  in
    li []
      (text contents :: sols)

view : Model -> Html Msg
view model =
  div []
    [
      Html.node "link" [Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css"] [],
      lazy viewUI model.ui,
      lazy viewResults model.results
    ]
