functor MkRangeCount(structure OrdTable : ORD_TABLE) : RANGE_COUNT =
struct
  structure OrdTable = OrdTable
  open OrdTable

  (* Ordered table type: *)
  type 'a table = 'a table
  type 'a seq = 'a Seq.seq
  type point = Key.t * Key.t

  (* Use this to compare x- or y-values. *)
  val compareKey : (Key.t * Key.t) -> order = Key.compare

  (* Define this yourself *)
  type countTable = unit table table  (*set table*)

  exception FindTheSameYCoordinate

  fun makeCountTable (S : point seq) : countTable =
    let
      val orderX = fromSeq S
      fun inserty(t,(_,y)) = insert (fn _ => raise FindTheSameYCoordinate) (y,()) t
    in iterih inserty (empty()) orderX
    end

  fun count (T : countTable)
                   ((xLeft, yHi) : point, (xRght, yLo) : point) : int  =
    let
      val lbound = #1(split (T, xLeft))
      val rbound = case split (T, xRght) of
        (t, NONE, _) => t
      | (t, SOME e, _) => insert (fn(x,_)=>x) (xRght, e) t
      fun ysize t = size (getRange t (yLo, yHi))
      fun opttot(SOME (_, v)) = v
        | opttot NONE = empty()
    in ysize(opttot(last rbound)) - ysize(opttot(last lbound))
    end
end