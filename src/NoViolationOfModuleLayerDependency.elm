module NoViolationOfModuleLayerDependency exposing (rule, ModuleLayer(..), ModuleLayerDependency(..))


import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


rule : ModuleLayerDependency -> Rule
rule layerRule =
    Rule.newModuleRuleSchema "NoViolationOfModuleLayerDependency" 0
        |> Rule.withModuleDefinitionVisitor (moduleDefinitionVisitor layerRule)
        |> Rule.withImportVisitor (importVisitor layerRule)
        |> Rule.fromModuleRuleSchema


type alias Context = Int


moduleDefinitionVisitor : ModuleLayerDependency -> Node Module -> Context -> (List (Error {}), Context)
moduleDefinitionVisitor layerRule node _ =
    ( []
    , Node.value node
        |> Module.moduleName
        |> layerNumber layerRule
    )

importVisitor : ModuleLayerDependency -> Node Import -> Context -> (List (Error {}), Context)
importVisitor layerRule node moduleLayerNumber =
    let
        importingModuleName =
            Node.value node |> .moduleName |> Node.value

        importingModuleNameAsString =
            importingModuleName
                |> List.intersperse "."
                |> List.foldl (\x acc -> acc ++ x) ""
    in
    if isViolatedWithLayerRule layerRule moduleLayerNumber importingModuleName then
        ( [ Rule.error
            { message = "Dependency violation of module layer is detected!"
            , details =
                [ "This module is layer number " ++ String.fromInt moduleLayerNumber ++ "."
                , "But the module is importing \"" ++ importingModuleNameAsString ++ "\" layer number " ++ (String.fromInt <| layerNumber layerRule importingModuleName) ++ "."
                ]
            }
            (Node.range node)
            ]
        , moduleLayerNumber
        )

    else
        ( []
        , moduleLayerNumber
        )


type alias  ModuleName
    = List String


type ModuleLayer
    = ModuleLayer (List ModuleName)
    | DefaultLayer


type ModuleLayerDependency
    = ModuleLayerDependency (List ModuleLayer)


isViolatedWithLayerRule : ModuleLayerDependency -> Int -> List String -> Bool
isViolatedWithLayerRule moduleLayerRule moduleLayerNumber importingModuleName =
    moduleLayerNumber < layerNumber moduleLayerRule importingModuleName


layerNumber : ModuleLayerDependency -> List String -> Int
layerNumber ((ModuleLayerDependency layers) as moduleLayerRule) moduleName =
    let
        fold (index, layer) acc =
            case acc of
                Just _ ->
                    acc

                Nothing ->
                    if isInLayer layer moduleName then
                        Just index

                    else
                        Nothing
    in
    layers
        |> List.indexedMap (\index layer -> (index, layer))
        |> List.foldl fold Nothing
        |> Maybe.withDefault (defaultLayerNumber moduleLayerRule)


defaultLayerNumber : ModuleLayerDependency -> Int
defaultLayerNumber (ModuleLayerDependency layers) =
    layers
        |> List.indexedMap (\i x -> (i, x))
        |> List.foldl (\(index, layer) acc ->
                if layer == DefaultLayer then
                    Just index

                else
                    acc
            ) Nothing
        |> Maybe.withDefault (List.length layers)


isInLayer : ModuleLayer -> List String -> Bool
isInLayer layer moduleName =
    case layer of
        ModuleLayer layerModuleNames ->
            List.any (\layerModuleName -> isMatch layerModuleName moduleName) layerModuleNames

        DefaultLayer ->
            False


isMatch : ModuleName -> List String -> Bool
isMatch names moduleName =
    let
        isMatch_ namesX namesY =
            case (namesX, namesY) of
                ([], _) ->
                    True

                (x :: xs, y :: ys) ->
                    if x == y then
                        isMatch_ xs ys

                    else
                        False

                (_ :: _, []) ->
                    False
    in
    isMatch_ names moduleName