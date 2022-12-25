type 'a ltree = LNode of 'a * (unit -> 'a ltree) * (unit -> 'a ltree)

let rec layer_tree n = 
  LNode(n, (fun () -> layer_tree (n+1)), (fun () -> layer_tree (n+1)));;


let rec interval_tree (l, h) =
  LNode((l, h), (fun () -> interval_tree (l, (l+.h)/.2.)), (fun () -> interval_tree ((l+.h)/.2., h)));;


let rec rational_tree (n, d)=
  LNode((n, d), (fun () -> rational_tree (n,d+1)), (fun () -> rational_tree (n+1,d)));;


type 'a btree = Empty | Node of 'a * 'a btree * 'a btree

let left t=
  match t with
  |LNode(a,l,r) -> l

let right t=
  match t with
  |LNode(a,l,r) -> r 
    
let root t=
  match t with
  |LNode(a,l,r) -> a


let rec top n t =
  match n with
  |0 -> Empty
  |x -> Node (root t, top (x-1) ((left t)()),  top (x-1) ((right t)()))  





let rec map f tree = 
  match tree with
  | LNode(x, l, r) -> LNode(f x, (fun() -> map f (l())), (fun()-> map f (r())));;
          




let rec helper p t =
  match t with
  | LNode(x, l, r)::rest ->
      if p x then LNode(x, l, r)
      else helper p (rest@[l(); r()])


let find p tree = helper p [tree];; 

