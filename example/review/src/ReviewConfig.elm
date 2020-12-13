module ReviewConfig exposing (config)

import NoViolationOfModuleLayerDependency exposing (ModuleLayer(..), ModuleLayerDependency(..))
import Review.Rule exposing (Error, Rule)


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
