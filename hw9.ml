let rec take n s =  match n with
  | 0 -> []
  | n -> match (force s.tl) with
    | None -> s.hd::[]
    | Some tl -> s.hd :: (take (n - 1) tl)

let rec map f s = match (force s.tl) with
  | None -> ({
      hd = f s.hd;
      tl = Susp(fun () -> None)
    })
  | Some tl' -> ({
      hd = f s.hd;
      tl = Susp(fun () -> Some(map f tl'))
    })

let rec append s1 s2 = match (force s1.tl) with
  | None -> ({
      hd = s1.hd;
      tl = s2
    })
  | Some tl' -> ({
      hd = s1.hd;
      tl = Susp(fun () -> Some(append tl' s2))
    })
  
let rec interleave x l = 
  {
    hd = x::l;
    tl = Susp( fun() -> match l with 
        | [] -> None
        | h1::t1 -> Some (map (fun l -> h1::l) (interleave x t1)))
  }

let rec flatten s = match (force s.tl) with
  | None -> s.hd
  | Some tl' -> append (s.hd) (Susp(fun () -> Some(flatten tl')))


let rec permute l = match l with
  | [] -> { hd=[];tl=Susp(fun() -> None)}
  | h::tl -> flatten (map (interleave h) (permute tl))

let rec hailstones n = match n mod 2 with
  | 0 -> {hd = n; tl = Susp(fun () -> Some(hailstones (n/2)))}
  | 1 -> {hd = n; tl = Susp(fun () -> Some(hailstones (3*n+1)))}