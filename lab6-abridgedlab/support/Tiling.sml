CM.autoload "support.cm";

structure myTable : TABLE = MkTreapTable(structure HashKey = MkPairElt (structure EltA = IntElt
                                                                        structure EltB = IntElt));

type vertex = int * int
type graph = vertex list * (vertex myTable.seq) myTable.table
fun toIntElt (x:int):IntElt.t = x

fun toGraph (lst:(int*int) list list) n:graph= case lst 
    of [] => ([],myTable.empty())
    | hd :: tl =>
    (
    case hd
    of [] => toGraph tl (n+1)
    | hd' :: tl' => 
        let 
            val vtx = List.tabulate (((#2hd')-(#1hd')+1),(fn x =>((#1hd'+x),n)))
            val nextGrp = toGraph (tl'::tl) n
            val nextTbl = #2(nextGrp)
            val initTbl = List.foldl (fn(nvtx,tbl)=>myTable.insert #2 (nvtx,myTable.Seq.empty()) tbl) nextTbl vtx
            val connectLeft = List.foldl (fn(nvtx,tbl)=>myTable.insert myTable.Seq.append
                                (nvtx,(case (myTable.find initTbl ((toIntElt(#1nvtx-1)),#2nvtx)) of NONE => myTable.Seq.empty()
                                    |_=> (myTable.Seq.singleton((toIntElt(#1nvtx-1)),#2nvtx)))) tbl) initTbl vtx
            val connectRight = List.foldl (fn(nvtx,tbl)=>myTable.insert myTable.Seq.append
                                (nvtx,(case (myTable.find connectLeft ((toIntElt(#1nvtx+1)),#2nvtx)) of NONE => myTable.Seq.empty()
                                    |_=> (myTable.Seq.singleton((toIntElt(#1nvtx+1)),#2nvtx)))) tbl) connectLeft vtx
            val toConnectDown = List.filter (fn(dvtx)=>(case myTable.find connectRight (#1dvtx,(toIntElt(#2dvtx+1))) of NONE => false 
                                    |_ => true)) vtx
            val toConnectUp = List.map (fn (a,b)=>(a,b+1)) toConnectDown
            val connectDown = List.foldl (fn(nvtx,tbl)=>myTable.insert myTable.Seq.append (nvtx,(myTable.Seq.singleton(#1nvtx,(toIntElt(#2nvtx+1))))) tbl) 
                                    connectRight toConnectDown
            val connectUp = List.foldl (fn(nvtx,tbl)=>myTable.insert myTable.Seq.append (nvtx,(myTable.Seq.singleton(#1nvtx,(toIntElt(#2nvtx-1))))) tbl)
                                connectDown toConnectUp
            in
                (vtx@(#1nextGrp),connectUp)
            end
    )

    fun getHelp (grid:graph) = SMLofNJ.Cont.callcc(fn k =>
        let 
            fun rmvoneElem a =
                let
                    val nodes = myTable.Set.fromSeq (myTable.Seq.fromList (#1a))
                    val degOne = (myTable.Seq.fromList (List.filter (fn(vtx) => ((myTable.Seq.length (valOf (myTable.find (#2a) vtx))) = 1)) (#1a)))
                in if ((myTable.Seq.length degOne) = 0) then a else 
                    (let
                        val rmvNodes = myTable.Seq.flatten (myTable.Seq.map (fn x=> (valOf (myTable.find (#2a) x))) degOne)
                        fun VtxCmp ((xa,xb),(ya,yb)) =
                            case Int.compare (xa,ya)
                                of EQUAL => Int.compare (xb,yb)
                                | ord => ord
                        val srtRmvNodes = myTable.Seq.sort VtxCmp rmvNodes
                        val _ = myTable.Seq.map (fn (a,b) => if a=0 then () else (case VtxCmp ((myTable.Seq.nth srtRmvNodes (a-1)),b) of EQUAL => SMLofNJ.Cont.throw k false | _ => ()))
                                    (myTable.Seq.enum srtRmvNodes)
                        val realRmv = myTable.Seq.append (degOne,srtRmvNodes)
                        val toMod = myTable.Seq.flatten (myTable.Seq.map (fn x => case x of (a,b) =>
                                        myTable.Seq.fromList [(x,(a+1,b)),(x,(a-1,b)),(x,(a,b+1)),(x,(a,b-1))]) realRmv)
                        val lstRmv = myTable.Seq.toList (myTable.Set.toSeq (myTable.Set.difference (nodes,myTable.Set.fromSeq realRmv)))
                        val hlfRmv = myTable.Seq.iter (fn (tbl,(trmv,idx)) => case myTable.find tbl idx of NONE => tbl 
                            |SOME mseq => myTable.insert #2 (idx,myTable.Seq.filter (fn x => case VtxCmp (trmv,x) of EQUAL => false |_=>true) mseq) tbl) (#2a) toMod
                        val fullRmv = (myTable.erase (hlfRmv,(myTable.Set.fromSeq realRmv)))
                    in rmvoneElem (lstRmv,fullRmv) end)
                end
            val TwoLeast = rmvoneElem grid
        in true end)