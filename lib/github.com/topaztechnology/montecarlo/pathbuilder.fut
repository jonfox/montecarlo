import "brownians"

module mk_pathbuilder(T: float) = {
  type t = T.t

  let build [n][m] (brownians: [n][m]t) (prices: [m]t) (vols: [m]t) (times: [m]t): [n][m]t =
    let paths = replicate n (replicate m (T.f32 0.0))
    let tensor = loop (paths) for i < n do
      let path = map4 (\b p v t ->
        let risk_adj = T.exp((T.f32 (-0.5)) T.* v T.* v T.* t)
        in p T.* T.exp(v T.* b) T.* risk_adj
      ) brownians[i] prices vols times
      in paths with [i] = path
    
    in tensor
}