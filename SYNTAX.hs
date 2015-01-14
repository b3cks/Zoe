module SYNTAX where
import ZRTE
import ZPOST
import LIB


g1 = eval
  (
    Func ("D__Dropbox_Zoe_Source_SYNTAX_zoe",4,5)
    (\ x -> 
      
      let
        g2 = eval
          (x
          )

      in

      g2
    )
  )

mytree = eval
  (
    Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",11,7)
    (
      Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",11,7)
      (node
      )
      (
        Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",17,7)
        (
          Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",17,7)
          (node
          )
          (
            Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",23,7)
            (leaf
            )
            (Numprim ("D__Dropbox_Zoe_Source_SYNTAX_zoe",28,7)2
            )
          )
        )
        (
          Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",32,7)
          (
            Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",32,7)
            (node
            )
            (
              Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",38,7)
              (leaf
              )
              (Numprim ("D__Dropbox_Zoe_Source_SYNTAX_zoe",43,7)2
              )
            )
          )
          (
            Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",47,7)
            (leaf
            )
            (Numprim ("D__Dropbox_Zoe_Source_SYNTAX_zoe",52,7)5
            )
          )
        )
      )
    )
    (
      Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",58,7)
      (leaf
      )
      (Numprim ("D__Dropbox_Zoe_Source_SYNTAX_zoe",63,7)3
      )
    )
  )

mylist = eval
  (
    Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",10,9)
    (
      Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",10,9)
      (consl
      )
      (Numprim ("D__Dropbox_Zoe_Source_SYNTAX_zoe",16,9)5
      )
    )
    (
      Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",19,9)
      (
        Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",19,9)
        (consl
        )
        (Numprim ("D__Dropbox_Zoe_Source_SYNTAX_zoe",25,9)2
        )
      )
      (
        Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",28,9)
        (
          Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",28,9)
          (consl
          )
          (Numprim ("D__Dropbox_Zoe_Source_SYNTAX_zoe",34,9)3
          )
        )
        (nil
        )
      )
    )
  )
sumlist =
  Func ("D__Dropbox_Zoe_Source_SYNTAX_zoe",9,11)
  (\ l -> 
    
    (FoldPacket("D__Dropbox_Zoe_Source_SYNTAX_zoe",10,12) l list (
      Namstup ("D__Dropbox_Zoe_Source_SYNTAX_zoe",18,12) [
        Tuple ("D__Dropbox_Zoe_Source_SYNTAX_zoe",20,12) [Nam ("D__Dropbox_Zoe_Source_SYNTAX_zoe",20,12)"nil" ,Numprim ("D__Dropbox_Zoe_Source_SYNTAX_zoe",26,12)0],
        Tuple ("D__Dropbox_Zoe_Source_SYNTAX_zoe",27,12) [Nam ("D__Dropbox_Zoe_Source_SYNTAX_zoe",29,12)"consl" ,
          Func ("D__Dropbox_Zoe_Source_SYNTAX_zoe",39,12)
          (\ x -> 
            Func ("D__Dropbox_Zoe_Source_SYNTAX_zoe",41,12)
            (\ xs -> 
              
              Add ("D__Dropbox_Zoe_Source_SYNTAX_zoe",47,12)
              [
                x
                ,xs
              ]
            )
          )]
      ])
    )
  )

temp = eval
  (
    Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",9,14)
    (
      Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",9,14)
      (insert
      )
      (Numprim ("D__Dropbox_Zoe_Source_SYNTAX_zoe",16,14)5
      )
    )
    (mylist
    )
  )
insert =
  Func ("D__Dropbox_Zoe_Source_SYNTAX_zoe",8,16)
  (\ e -> 
    Func ("D__Dropbox_Zoe_Source_SYNTAX_zoe",10,16)
    (\ ys -> 
      
      (FoldPacket("D__Dropbox_Zoe_Source_SYNTAX_zoe",10,17) ys list (
        Namstup ("D__Dropbox_Zoe_Source_SYNTAX_zoe",21,17) [
          Tuple ("D__Dropbox_Zoe_Source_SYNTAX_zoe",9,18) [Nam ("D__Dropbox_Zoe_Source_SYNTAX_zoe",9,18)"consl" ,
            Func ("D__Dropbox_Zoe_Source_SYNTAX_zoe",20,18)
            (\ x -> 
              Func ("D__Dropbox_Zoe_Source_SYNTAX_zoe",22,18)
              (\ xs -> 
                
                Namstup ("D__Dropbox_Zoe_Source_SYNTAX_zoe",29,18) [
                  Tuple ("D__Dropbox_Zoe_Source_SYNTAX_zoe",31,18) [Nam ("D__Dropbox_Zoe_Source_SYNTAX_zoe",31,18)"exs" ,
                    Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",38,18)
                    (
                      Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",38,18)
                      ((CompareOp ("D__Dropbox_Zoe_Source_SYNTAX_zoe",41,18)[e,Lt ("D__Dropbox_Zoe_Source_SYNTAX_zoe",43,18) (x)])
                      )
                      (
                      Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",53,18)
                      (
                        Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",53,18)
                        (consl
                        )
                        (e
                        )
                      )
                      (
                        Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",62,18)
                        (
                          Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",62,18)
                          (consl
                          )
                          (x
                          )
                        )
                        ((NamstupSelector ("D__Dropbox_Zoe_Source_SYNTAX_zoe",71,18)(xs) (Nam("D__Dropbox_Zoe_Source_SYNTAX_zoe",75,18) "oxs"))
                        )
                      )
                      )
                    )
                    (
                    Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",88,18)
                    (
                      Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",88,18)
                      (consl
                      )
                      (x
                      )
                    )
                    ((NamstupSelector ("D__Dropbox_Zoe_Source_SYNTAX_zoe",97,18)(xs) (Nam("D__Dropbox_Zoe_Source_SYNTAX_zoe",101,18) "exs"))
                    )
                    )],
                  Tuple ("D__Dropbox_Zoe_Source_SYNTAX_zoe",110,18) [Nam ("D__Dropbox_Zoe_Source_SYNTAX_zoe",112,18)"oxs" ,
                    Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",119,18)
                    (
                      Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",119,18)
                      (consl
                      )
                      (x
                      )
                    )
                    ((NamstupSelector ("D__Dropbox_Zoe_Source_SYNTAX_zoe",128,18)(xs) (Nam("D__Dropbox_Zoe_Source_SYNTAX_zoe",132,18) "oxs"))
                    )]
                ]
              )
            )],
          Tuple ("D__Dropbox_Zoe_Source_SYNTAX_zoe",140,18) [Nam ("D__Dropbox_Zoe_Source_SYNTAX_zoe",9,19)"nil" ,
            Namstup ("D__Dropbox_Zoe_Source_SYNTAX_zoe",15,19) [
              Tuple ("D__Dropbox_Zoe_Source_SYNTAX_zoe",17,19) [Nam ("D__Dropbox_Zoe_Source_SYNTAX_zoe",17,19)"exs" ,
                Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",23,19)
                (
                  Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",23,19)
                  (consl
                  )
                  (e
                  )
                )
                (nil
                )],
              Tuple ("D__Dropbox_Zoe_Source_SYNTAX_zoe",34,19) [Nam ("D__Dropbox_Zoe_Source_SYNTAX_zoe",36,19)"oxs" ,nil]
            ]]
        ])
      )
    )
  )
insert1 =
  Func ("D__Dropbox_Zoe_Source_SYNTAX_zoe",9,22)
  (\ e -> 
    Func ("D__Dropbox_Zoe_Source_SYNTAX_zoe",11,22)
    (\ ys -> 
      
      (FoldPacket("D__Dropbox_Zoe_Source_SYNTAX_zoe",10,23) ys list (
        Namstup ("D__Dropbox_Zoe_Source_SYNTAX_zoe",21,23) [
          Tuple ("D__Dropbox_Zoe_Source_SYNTAX_zoe",9,24) [Nam ("D__Dropbox_Zoe_Source_SYNTAX_zoe",9,24)"consl" ,
            Func ("D__Dropbox_Zoe_Source_SYNTAX_zoe",20,24)
            (\ x1 -> 
              Func ("D__Dropbox_Zoe_Source_SYNTAX_zoe",23,24)
              (\ zoefpD__Dropbox_Zoe_Source_SYNTAX_zoe_23_24@
                  (Tuple _
                    [x21
                      ,x22
                    ])->
                  
                  Tuple ("D__Dropbox_Zoe_Source_SYNTAX_zoe",36,24)
                  [
                  Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",38,24)
                  (
                    Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",38,24)
                    ((CompareOp ("D__Dropbox_Zoe_Source_SYNTAX_zoe",41,24)[e,Lt ("D__Dropbox_Zoe_Source_SYNTAX_zoe",43,24) (x1)])
                    )
                    (
                    Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",54,24)
                    (
                      Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",54,24)
                      (consl
                      )
                      (e
                      )
                    )
                    (
                      Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",63,24)
                      (
                        Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",63,24)
                        (consl
                        )
                        (x1
                        )
                      )
                      (x22
                      )
                    )
                    )
                  )
                  (
                  Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",84,24)
                  (
                    Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",84,24)
                    (consl
                    )
                    (x1
                    )
                  )
                  (x21
                  )
                  )
                  ,
                  Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",104,24)
                  (
                    Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",104,24)
                    (consl
                    )
                    (x1
                    )
                  )
                  (x22
                  )]
              )
            )],
          Tuple ("D__Dropbox_Zoe_Source_SYNTAX_zoe",119,24) [Nam ("D__Dropbox_Zoe_Source_SYNTAX_zoe",9,25)"nil" ,
            Tuple ("D__Dropbox_Zoe_Source_SYNTAX_zoe",15,25)
            [
            Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",16,25)
            (
              Apply("D__Dropbox_Zoe_Source_SYNTAX_zoe",16,25)
              (consl
              )
              (e
              )
            )
            (nil
            )
            ,nil]]
        ])
      )
    )
  )

tup = eval
  (
    Namstup ("D__Dropbox_Zoe_Source_SYNTAX_zoe",7,28) [
      Tuple ("D__Dropbox_Zoe_Source_SYNTAX_zoe",8,28) [Nam ("D__Dropbox_Zoe_Source_SYNTAX_zoe",8,28)"a" ,Numprim ("D__Dropbox_Zoe_Source_SYNTAX_zoe",10,28)1],
      Tuple ("D__Dropbox_Zoe_Source_SYNTAX_zoe",11,28) [Nam ("D__Dropbox_Zoe_Source_SYNTAX_zoe",12,28)"b" ,Numprim ("D__Dropbox_Zoe_Source_SYNTAX_zoe",14,28)2]
    ]
  )

tup1 = eval
  ((NamstupUpdate ("D__Dropbox_Zoe_Source_SYNTAX_zoe",8,30)(tup)(
    Namstup ("D__Dropbox_Zoe_Source_SYNTAX_zoe",15,30) [
      Tuple ("D__Dropbox_Zoe_Source_SYNTAX_zoe",16,30) [Nam ("D__Dropbox_Zoe_Source_SYNTAX_zoe",16,30)"c" ,Numprim ("D__Dropbox_Zoe_Source_SYNTAX_zoe",18,30)2]
    ]))
  )


(Tuple _
  [
    (Tuple _
      [t1
        ,t2
      ])
    ,tt@
      (
        (Tuple _
          [t3
            ,t4
          ])
      )
  ]) = eval
  (
    
    Tuple ("D__Dropbox_Zoe_Source_SYNTAX_zoe",24,42)
    [
    Tuple ("D__Dropbox_Zoe_Source_SYNTAX_zoe",25,42)
    [Numprim ("D__Dropbox_Zoe_Source_SYNTAX_zoe",26,42)1
    ,Numprim ("D__Dropbox_Zoe_Source_SYNTAX_zoe",28,42)2]
    ,
    Tuple ("D__Dropbox_Zoe_Source_SYNTAX_zoe",31,42)
    [Numprim ("D__Dropbox_Zoe_Source_SYNTAX_zoe",32,42)3
    ,Numprim ("D__Dropbox_Zoe_Source_SYNTAX_zoe",34,42)4]]
  )

f1 = eval
  (
    Func ("D__Dropbox_Zoe_Source_SYNTAX_zoe",9,44)
    (\ l@
    (Listprim _
      (x1
        :tail_D__Dropbox_Zoe_Source_SYNTAX_zoe_11_44
      )) -> 
      let
        x2 = (Listprim ("D__Dropbox_Zoe_Source_SYNTAX_zoe",15,44) tail_D__Dropbox_Zoe_Source_SYNTAX_zoe_11_44)
      in
        x1
    )
  )

f2 = eval
  (
    Func ("D__Dropbox_Zoe_Source_SYNTAX_zoe",4,46)
    (\ l@
      (
        (Tuple _
          [x1
            ,x2
          ])
      ) -> 
      x2
    )
  )

f3 = eval
  (
    Func ("D__Dropbox_Zoe_Source_SYNTAX_zoe",4,48)
    (\ 
    (Listprim _
      (e1
        :e2
        :tail_D__Dropbox_Zoe_Source_SYNTAX_zoe_4_48
      )) -> 
      let
        e3 = (Listprim ("D__Dropbox_Zoe_Source_SYNTAX_zoe",11,48) tail_D__Dropbox_Zoe_Source_SYNTAX_zoe_4_48)
      in
        e3
    )
  )

y = eval
  (
    Neg ("D__Dropbox_Zoe_Source_SYNTAX_zoe",5,52)
    (
      
      Add ("D__Dropbox_Zoe_Source_SYNTAX_zoe",8,52)
      [
        Numprim ("D__Dropbox_Zoe_Source_SYNTAX_zoe",8,52)5
        ,Numprim ("D__Dropbox_Zoe_Source_SYNTAX_zoe",10,52)1
      ]
    )
  )
