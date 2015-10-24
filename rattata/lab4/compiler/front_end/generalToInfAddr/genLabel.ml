let counter = ref 1
let create () =
  let t = !counter in
  let () = counter := !counter + 1 in
  t
