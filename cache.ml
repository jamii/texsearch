let with_cache f =
    let cache = Hashtbl.create 5000 in
    let rec cached a b =
        try
            Hashtbl.find cache (a,b)
        with Not_found ->
            let result = f cached a b in
            Hashtbl.add cache (a,b) result;
            result in
    cached
