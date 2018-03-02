structure Tests =
struct

  (* Do not remove the following line! *)
  val corpus = TextIO.inputAll (TextIO.openIn "corpus.txt")

  val testsChoose : (((string * int) list) * real) list  = [
    ([("test", 10)], 0.5),
    ([("test", 2), ("awesome", 2)], 0.5),
    ([("yay", 1), ("woah", 2), ("oh", 3), ("yup", 4)], 0.47),
    ([("hello",1),("goodbye",999)],0.0),
    ([("awesome",4),("wow",888)],1.0),
    (* a long tests*)
    ([("a",34),("b",42),("c",23),("d",12),("e",43),("f",73),("g",14),("h",9),("i",23),("j",3),("k",63),
      ("l",4),("m",46),("n",34),("o",32),("p",24),("q",15),("r",25),("s",54),("t",42),("u",24),("v",52),
      ("w",34),("x",25),("y",36),("z",23)],0.55),
    (* an empty sequence*)
    ([], 0.1),
    (* p < 0*)(* need to rase Exn Range, but there are some bugs in reference*)
    ([("neg",13),("ative",24)],~0.1),
    (* p > 1 *)
    ([("ex",5),("out",2)],1.1)
  ]

  (* You must add test kgrams for the corpus in corpus.txt as part of task 5.5
   * You may edit corpus.txt -- it will be handed in.
   *
   * You may also add other tests, which use other corpi (corpuses?), but those
   * corpuses will not be submitted. *)
  val testsKGramStats : ((string * int) * (string list)) list = [
    ((corpus, 50),
        ["direction",
         "time",
         "direction of time",
         "would write",
         "What Eddington says about",
         "British Museum",
         "write all the books in the British Museum",
          (* more tests cases*)
         "all the books in the British" ,
         "What Arthur Eddington says about the infinite monkey theorem and the direction",
         "then the world may be rendered",
          (*empty case*)
         ""
         ])
  ]


end
