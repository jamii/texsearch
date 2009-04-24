open Tree
module CacheMap = Map.Make (
struct
  type t = (element forest * element forest)
  let compare = compare
end)

let with_cache f =
  let cache = ref CacheMap.empty in
  let rec cached a b =
    try
      CacheMap.find (a,b) !cache
    with Not_found ->
      let result = f cached a b in
      cache := CacheMap.add (a,b) result !cache;
      result in
  cached
