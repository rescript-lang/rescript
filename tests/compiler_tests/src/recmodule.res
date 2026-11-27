type params = {id: string}
type req = {params: params}

module Entity = {
  type light = {
    id: string,
    name: string,
  }
}

module UseCase = {
  module type Layer = {
    let getLight: string => promise<unit>
  }

  module type Deps = {
    let presentLight: Entity.light => promise<unit>
  }

  module MakeLayer = (Deps: Deps): Layer => {
    let getLight = id => {
      Deps.presentLight({
        id,
        name: "Light 1",
      })
    }
  }
}

module Adapter = {
  module type Layer = {
    let handleGetLight: req => promise<unit>
    let presentLight: Entity.light => promise<unit>
  }

  module type Deps = {
    let presentJson: ('a, ~status: int) => promise<unit>
  }

  module MakeLayer = (Deps: Deps, UC: UseCase.Layer): Layer => {
    let presentLight = light => light->Deps.presentJson(~status=200)

    let handleGetLight = req => {
      UC.getLight(req.params.id)
    }
  }
}

module Infra = {
  module type Layer = {
    let presentJson: ('a, ~status: int) => promise<unit>
    let routes: unit => array<(string, req => promise<unit>)>
  }

  module type Deps = {
    let handleGetLight: req => promise<unit>
  }

  module MakeLayer = (Deps: Deps): Layer => {
    let presentJson = (json, ~status) => assert(false)

    let routes = () => [("/lights", Deps.handleGetLight)]
  }
}

module App = {
  module rec I: Infra.Layer = Infra.MakeLayer(A)
  and A: Adapter.Layer = Adapter.MakeLayer(I, U)
  and U: UseCase.Layer = UseCase.MakeLayer(A)
}
