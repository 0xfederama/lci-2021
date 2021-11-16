let mingle p q =
    let la = String.length p in
    let lb = String.length q in
    let total = la + lb in
    let s = Bytes.create total in
    let rec m i =
      (* j = index for inputs *)
      let j = i / 2 in
      if i > (total - 1) then s
      else let () =
        (* i = index for output *)
        s.[i] <- p.[j];
        s.[i+1] <- q.[j];
        in m (i + 2); in
    m 0;;
  
print_bytes (mingle "abc" "xyz");;
print_string ("");;