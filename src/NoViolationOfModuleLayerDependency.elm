module NoViolationOfModuleLayerDependency exposing (rule, ModuleLayer(..), ModuleLayerDependency(..))

{-| If you defined a layer dependency rule like `A <- B`, elm-review-module-layer-dependency will detect an error with below importing.
   ```elm
   module A

   import B.Data
   ```

   Module A depends on module B (`A -> B`) is a violation of defined rule.

# Rule
@docs rule

# Type
@docs ModuleLayer, ModuleLayerDependency
-}

import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)

{-|
## Example configuration
    config : List Rule
    config =
        [ NoViolationOfModuleLayerDependency.rule moduleLayerRule
        ]


    moduleLayerRule : ModuleLayerDependency
    moduleLayerRule =
        ModuleLayerDependency
            [ infraLayer
            , applicationLayer
            , DefaultLayer
            , adapterLayer
            ]


    adapterLayer : ModuleLayer
    adapterLayer =
        ModuleLayer
            [ [ "Adapter" ]
            , [ "Main" ]
            ]


    applicationLayer : ModuleLayer
    applicationLayer =
        ModuleLayer
            [ [ "Application" ]
            ]


    infraLayer : ModuleLayer
    infraLayer =
        ModuleLayer
            [ [ "Infra" ]
            ]
-}
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
            Node.value node
                |> .moduleName
                |> Node.value
    in
    if isViolatedWithLayerRule layerRule moduleLayerNumber importingModuleName then
        ( [ Rule.error
            { message = "Found import to upper layer!"
            , details =
                [ "This module is layer number " ++ String.fromInt moduleLayerNumber ++ "."
                , "But the module is importing \"" ++ moduleNameAsString importingModuleName ++ "\" layer number " ++ (String.fromInt <| layerNumber layerRule importingModuleName) ++ "."
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


moduleNameAsString : ModuleName -> String
moduleNameAsString =
    List.intersperse "."
        >> List.foldl (\x acc -> acc ++ x) ""

{-| Provide module name to define rule.
## Example

A module `Aaa.Bbb.Ccc` define as ModuleLayer.
    ModuleLayer
        [ [ "Aaa", "Bbb", "Ccc" ]
        ]
-}
type ModuleLayer
    = ModuleLayer (List ModuleName)
    | DefaultLayer


{-| Provide layer to define rule.
## Example

Define ModuleLayerDependency use `firstLayer: ModuleLayer` and `secondLayer: ModuleLayer`.
    [ firstLayer
    , secondLayer
    ]

It means dependency like `firstLayer <- secondLayer`.
If imported secondLayer module in firstLayer, elm-review will detect an error.
-}
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
            List.any (\layerModuleName -> isMatchWith layerModuleName moduleName) layerModuleNames

        DefaultLayer ->
            False


isMatchWith : ModuleName -> ModuleName -> Bool
isMatchWith names moduleName =
    let
        isMatchWith_ namesX namesY =
            case (namesX, namesY) of
                ([], _) ->
                    True

                (x :: xs, y :: ys) ->
                    if x == y then
                        isMatchWith_ xs ys

                    else
                        False

                (_ :: _, []) ->
                    False
    in
    isMatchWith_ names moduleName
