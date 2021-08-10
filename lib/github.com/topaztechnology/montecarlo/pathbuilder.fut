import "../../diku-dk/cpprandom/random"
import "../../diku-dk/sobol/sobol"
import "../../diku-dk/sobol/sobol-dir-21201"
import "../../diku-dk/statistics/statistics"
import "brownianbridge"

module mk_path_builder(T: real)(E: rng_engine) = {
  type t = T.t

  module rand_dist = normal_distribution T E
  type rng = rand_dist.engine.rng
  local let std_moments = { mean = (T.f32 0.0), stddev = (T.f32 1.0)}

  -- module stats = mk_statistics t
  -- local let uniform_to_gaussian (x: t): t = stats.normal_cdf_inv (T.f32 1.0) (T.f32 0.0) x
  
  module bridge = mk_brownian_bridge T

  let build_path [m] (times: [m]t) (n: i64) (seed: i32): [n][m]t =
    let generate_variates (start_rng: rng): ([m]t, rng) =
      let rngs = rand_dist.engine.split_rng m start_rng
      let (rngs, xs) = unzip (map (rand_dist.rand std_moments) rngs)
      let end_rng = rand_dist.engine.join_rng rngs
      in (xs, end_rng)

    let bridge_params = bridge.build times
    let rng_state = rand_dist.engine.rng_from_seed [seed]

    let paths = replicate n (replicate m (T.f32 0.0))
    let (tensor, _) = loop (paths, rng_state) = (paths, rng_state) for i < n do
      let (variates, rng_state) = generate_variates rng_state
      let bridged = bridge.transform bridge_params variates
      in (paths with [i] = bridged, rng_state)
    
    in tensor
}




module path_builder = mk_path_builder f64 pcg32
let main [m] (times: [m]f64) (n: i64): [n][m]f64 =
  path_builder.build_path times n 123
