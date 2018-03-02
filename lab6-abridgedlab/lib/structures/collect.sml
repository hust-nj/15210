fun collect f s =
let
  val sorted = sort f s 
  val filtered = filterIdx (fn (idx,el)=>not(f(nth s idx)
in
  body
end