module FS where
import ZRTE
import ZPOST

list = Ftype("D__Dropbox_Zoe_Source_FS_zoe<cons><nil>",1,7)

cons = 
  Func ("D__Dropbox_Zoe_Source_FS_zoe<cons>",13,7)
    (\ zoearg_D__Dropbox_Zoe_Source_FS_zoe_1_7_1 ->
    Func ("D__Dropbox_Zoe_Source_FS_zoe<cons>",13,7)
      (\ zoearg_D__Dropbox_Zoe_Source_FS_zoe_1_7_2 ->
        Func ("D__Dropbox_Zoe_Source_FS_zoe<cons>",13,7)
          (\ zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_7_1 ->
          Func ("D__Dropbox_Zoe_Source_FS_zoe<cons>",13,7)
            (\ zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_7_2 ->
            Apply("D__Dropbox_Zoe_Source_FS_zoe<cons>",13,7)
            (
              Apply("D__Dropbox_Zoe_Source_FS_zoe<cons>",13,7)
              (zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_7_1
              )
              (
              Tcheck ("D__Dropbox_Zoe_Source_FS_zoe<cons>",13,7)zoearg_D__Dropbox_Zoe_Source_FS_zoe_1_7_1 (Nums ("D__Dropbox_Zoe_Source_FS_zoe",18,7))
              )
            )
            (
              Apply("D__Dropbox_Zoe_Source_FS_zoe<cons>",13,7)
              (
                Apply("D__Dropbox_Zoe_Source_FS_zoe<cons>",13,7)
                (Tcheck ("D__Dropbox_Zoe_Source_FS_zoe<cons>",13,7) zoearg_D__Dropbox_Zoe_Source_FS_zoe_1_7_2 list
                )
                (zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_7_1
                )
              )
              (zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_7_2
              )
            )
          )
        )
    )
  )

nil = 
  Func ("D__Dropbox_Zoe_Source_FS_zoe<nil>",24,7)
    (\ zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_7_1 ->
    Func ("D__Dropbox_Zoe_Source_FS_zoe<nil>",24,7)
      (\ zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_7_2 ->
      zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_7_2
      )
    )


mylist = eval
  (
    Apply("D__Dropbox_Zoe_Source_FS_zoe",10,9)
    (
      Apply("D__Dropbox_Zoe_Source_FS_zoe",10,9)
      (cons
      )
      (Numprim ("D__Dropbox_Zoe_Source_FS_zoe",15,9)4
      )
    )
    (
      Apply("D__Dropbox_Zoe_Source_FS_zoe",18,9)
      (
        Apply("D__Dropbox_Zoe_Source_FS_zoe",18,9)
        (cons
        )
        (Numprim ("D__Dropbox_Zoe_Source_FS_zoe",23,9)7
        )
      )
      (
        Apply("D__Dropbox_Zoe_Source_FS_zoe",26,9)
        (
          Apply("D__Dropbox_Zoe_Source_FS_zoe",26,9)
          (cons
          )
          (Numprim ("D__Dropbox_Zoe_Source_FS_zoe",31,9)9
          )
        )
        (
          Apply("D__Dropbox_Zoe_Source_FS_zoe",34,9)
          (
            Apply("D__Dropbox_Zoe_Source_FS_zoe",34,9)
            (cons
            )
            (Numprim ("D__Dropbox_Zoe_Source_FS_zoe",39,9)11
            )
          )
          (nil
          )
        )
      )
    )
  )

gethead = eval
  (
    Func ("D__Dropbox_Zoe_Source_FS_zoe",9,11)
    (\ l -> 
      
      (FoldPacket("D__Dropbox_Zoe_Source_FS_zoe",13,11) l list (
        Namstup ("D__Dropbox_Zoe_Source_FS_zoe",26,11) [
          Tuple ("D__Dropbox_Zoe_Source_FS_zoe",5,12) [Nam ("D__Dropbox_Zoe_Source_FS_zoe",5,12)"cons" ,
            Func ("D__Dropbox_Zoe_Source_FS_zoe",14,12)
            (\ h -> 
              Func ("D__Dropbox_Zoe_Source_FS_zoe",16,12)
              (\ t -> 
                h
              )
            )],
          Tuple ("D__Dropbox_Zoe_Source_FS_zoe",23,12) [Nam ("D__Dropbox_Zoe_Source_FS_zoe",5,13)"nil" ,Strprim ("D__Dropbox_Zoe_Source_FS_zoe",11,13)"nil"]
        ])
      )
    )
  )

insert = eval
  (
    Func ("D__Dropbox_Zoe_Source_FS_zoe",8,18)
    (\ e -> 
      Func ("D__Dropbox_Zoe_Source_FS_zoe",10,18)
      (\ ys -> 
        (NamstupSelector ("D__Dropbox_Zoe_Source_FS_zoe",5,19)(
        (FoldPacket("D__Dropbox_Zoe_Source_FS_zoe",5,19) ys list (
          Namstup ("D__Dropbox_Zoe_Source_FS_zoe",21,19) [
            Tuple ("D__Dropbox_Zoe_Source_FS_zoe",9,20) [Nam ("D__Dropbox_Zoe_Source_FS_zoe",9,20)"cons" ,
              Func ("D__Dropbox_Zoe_Source_FS_zoe",19,20)
              (\ x -> 
                Func ("D__Dropbox_Zoe_Source_FS_zoe",21,20)
                (\ xs -> 
                  
                  Namstup ("D__Dropbox_Zoe_Source_FS_zoe",21,21) [
                    Tuple ("D__Dropbox_Zoe_Source_FS_zoe",25,22) [Nam ("D__Dropbox_Zoe_Source_FS_zoe",25,22)"exs" ,
                      Apply("D__Dropbox_Zoe_Source_FS_zoe",36,22)
                      (
                        Apply("D__Dropbox_Zoe_Source_FS_zoe",36,22)
                        ((CompareOp ("D__Dropbox_Zoe_Source_FS_zoe",39,22)[e,Lt ("D__Dropbox_Zoe_Source_FS_zoe",41,22) (x)])
                        )
                        (
                        Apply("D__Dropbox_Zoe_Source_FS_zoe",39,23)
                        (
                          Apply("D__Dropbox_Zoe_Source_FS_zoe",39,23)
                          (cons
                          )
                          (e
                          )
                        )
                        (
                          Apply("D__Dropbox_Zoe_Source_FS_zoe",47,23)
                          (
                            Apply("D__Dropbox_Zoe_Source_FS_zoe",47,23)
                            (cons
                            )
                            (x
                            )
                          )
                          ((NamstupSelector ("D__Dropbox_Zoe_Source_FS_zoe",55,23)(xs) (Nam("D__Dropbox_Zoe_Source_FS_zoe",59,23) "oxs"))
                          )
                        )
                        )
                      )
                      (
                      Apply("D__Dropbox_Zoe_Source_FS_zoe",39,24)
                      (
                        Apply("D__Dropbox_Zoe_Source_FS_zoe",39,24)
                        (cons
                        )
                        (x
                        )
                      )
                      ((NamstupSelector ("D__Dropbox_Zoe_Source_FS_zoe",47,24)(xs) (Nam("D__Dropbox_Zoe_Source_FS_zoe",51,24) "exs"))
                      )
                      )],
                    Tuple ("D__Dropbox_Zoe_Source_FS_zoe",32,26) [Nam ("D__Dropbox_Zoe_Source_FS_zoe",25,27)"oxs" ,
                      Apply("D__Dropbox_Zoe_Source_FS_zoe",32,27)
                      (
                        Apply("D__Dropbox_Zoe_Source_FS_zoe",32,27)
                        (cons
                        )
                        (x
                        )
                      )
                      ((NamstupSelector ("D__Dropbox_Zoe_Source_FS_zoe",40,27)(xs) (Nam("D__Dropbox_Zoe_Source_FS_zoe",44,27) "oxs"))
                      )]
                  ]
                )
              )],
            Tuple ("D__Dropbox_Zoe_Source_FS_zoe",18,29) [Nam ("D__Dropbox_Zoe_Source_FS_zoe",9,30)"nil" ,
              Namstup ("D__Dropbox_Zoe_Source_FS_zoe",15,30) [
                Tuple ("D__Dropbox_Zoe_Source_FS_zoe",17,30) [Nam ("D__Dropbox_Zoe_Source_FS_zoe",17,30)"exs" ,
                  Apply("D__Dropbox_Zoe_Source_FS_zoe",23,30)
                  (
                    Apply("D__Dropbox_Zoe_Source_FS_zoe",23,30)
                    (cons
                    )
                    (e
                    )
                  )
                  (nil
                  )],
                Tuple ("D__Dropbox_Zoe_Source_FS_zoe",33,30) [Nam ("D__Dropbox_Zoe_Source_FS_zoe",35,30)"oxs" ,nil]
              ]]
          ])
        )) (Nam("D__Dropbox_Zoe_Source_FS_zoe",10,31) "exs"))
      )
    )
  )

c = eval
  (
    Func ("D__Dropbox_Zoe_Source_FS_zoe",7,35)
    (\ h -> 
      Func ("D__Dropbox_Zoe_Source_FS_zoe",9,35)
      (\ t -> 
        
        Concat ("D__Dropbox_Zoe_Source_FS_zoe",14,35)
        [
          (Showop ("D__Dropbox_Zoe_Source_FS_zoe",14,35)(h))
          ,
          Strprim ("D__Dropbox_Zoe_Source_FS_zoe",26,35)" "
          ,t
        ]
      )
    )
  )

n = eval
  (Strprim ("D__Dropbox_Zoe_Source_FS_zoe",5,36)""
  )

gettailservers = eval
  (
    Namstup ("D__Dropbox_Zoe_Source_FS_zoe",5,39) [
      Tuple ("D__Dropbox_Zoe_Source_FS_zoe",9,40) [Nam ("D__Dropbox_Zoe_Source_FS_zoe",9,40)"cons" ,
        Func ("D__Dropbox_Zoe_Source_FS_zoe",18,40)
        (\ h -> 
          Func ("D__Dropbox_Zoe_Source_FS_zoe",20,40)
          (\ t -> 
            Func ("D__Dropbox_Zoe_Source_FS_zoe",22,40)
            (\ g -> 
              
              Apply("D__Dropbox_Zoe_Source_FS_zoe",27,40)
              (
                Apply("D__Dropbox_Zoe_Source_FS_zoe",27,40)
                (g
                )
                (h
                )
              )
              (
                Apply("D__Dropbox_Zoe_Source_FS_zoe",32,40)
                (t
                )
                (c
                )
              )
            )
          )
        )],
      Tuple ("D__Dropbox_Zoe_Source_FS_zoe",37,40) [Nam ("D__Dropbox_Zoe_Source_FS_zoe",9,41)"nil" ,
        Func ("D__Dropbox_Zoe_Source_FS_zoe",17,41)
        (\ t -> 
          n
        )]
    ]
  )

gettail = eval
  (
    Func ("D__Dropbox_Zoe_Source_FS_zoe",9,44)
    (\ l -> 
      
      Apply("D__Dropbox_Zoe_Source_FS_zoe",13,44)
      (
        (FoldPacket("D__Dropbox_Zoe_Source_FS_zoe",13,44) l list (gettailservers)
        )
      )
      (
        Func ("D__Dropbox_Zoe_Source_FS_zoe",43,44)
        (\ h -> 
          Func ("D__Dropbox_Zoe_Source_FS_zoe",45,44)
          (\ t -> 
            t
          )
        )
      )
    )
  )

getelement = eval
  (
    Func ("D__Dropbox_Zoe_Source_FS_zoe",12,46)
    (\ l -> 
      Func ("D__Dropbox_Zoe_Source_FS_zoe",14,46)
      (\ i -> 
        
        Apply("D__Dropbox_Zoe_Source_FS_zoe",18,46)
        (
          (FoldPacket("D__Dropbox_Zoe_Source_FS_zoe",18,46) l list (
            Namstup ("D__Dropbox_Zoe_Source_FS_zoe",5,47) [
              Tuple ("D__Dropbox_Zoe_Source_FS_zoe",9,48) [Nam ("D__Dropbox_Zoe_Source_FS_zoe",9,48)"cons" ,
                Func ("D__Dropbox_Zoe_Source_FS_zoe",18,48)
                (\ x -> 
                  Func ("D__Dropbox_Zoe_Source_FS_zoe",20,48)
                  (\ xs -> 
                    Func ("D__Dropbox_Zoe_Source_FS_zoe",23,48)
                    (\ c -> 
                      
                      Apply("D__Dropbox_Zoe_Source_FS_zoe",28,48)
                      (
                        Apply("D__Dropbox_Zoe_Source_FS_zoe",28,48)
                        ((CompareOp ("D__Dropbox_Zoe_Source_FS_zoe",31,48)[i,Equal ("D__Dropbox_Zoe_Source_FS_zoe",33,48) (c)])
                        )
                        (x
                        )
                      )
                      (
                      Apply("D__Dropbox_Zoe_Source_FS_zoe",51,48)
                      (xs
                      )
                      (
                        Add ("D__Dropbox_Zoe_Source_FS_zoe",55,48)
                        [
                          c
                          ,Numprim ("D__Dropbox_Zoe_Source_FS_zoe",57,48)1
                        ]
                      )
                      )
                    )
                  )
                )],
              Tuple ("D__Dropbox_Zoe_Source_FS_zoe",64,48) [Nam ("D__Dropbox_Zoe_Source_FS_zoe",9,49)"nil" ,
                Func ("D__Dropbox_Zoe_Source_FS_zoe",17,49)
                (\ c -> 
                  Boolprim ("D__Dropbox_Zoe_Source_FS_zoe",22,49)False
                )]
            ])
          )
        )
        (Numprim ("D__Dropbox_Zoe_Source_FS_zoe",7,50)0
        )
      )
    )
  )

printl = eval
  (
    Func ("D__Dropbox_Zoe_Source_FS_zoe",8,52)
    (\ l -> 
      
      (FoldPacket("D__Dropbox_Zoe_Source_FS_zoe",5,53) l list (
        Namstup ("D__Dropbox_Zoe_Source_FS_zoe",18,53) [
          Tuple ("D__Dropbox_Zoe_Source_FS_zoe",9,54) [Nam ("D__Dropbox_Zoe_Source_FS_zoe",9,54)"cons" ,c],
          Tuple ("D__Dropbox_Zoe_Source_FS_zoe",17,54) [Nam ("D__Dropbox_Zoe_Source_FS_zoe",9,55)"nil" ,n]
        ])
      )
    )
  )

zfirst = eval
  (
    Func ("D__Dropbox_Zoe_Source_FS_zoe",8,58)
    (\ zoefpD__Dropbox_Zoe_Source_FS_zoe_8_58@
        (Tuple _
          [x
            ,_
          ])->
        x
    )
  )

zsecond = eval
  (
    Func ("D__Dropbox_Zoe_Source_FS_zoe",9,59)
    (\ zoefpD__Dropbox_Zoe_Source_FS_zoe_9_59@
        (Tuple _
          [_
            ,x
          ])->
        x
    )
  )

ztail = eval
  (
    Func ("D__Dropbox_Zoe_Source_FS_zoe",7,60)
    (\ list -> 
      
      Apply("D__Dropbox_Zoe_Source_FS_zoe",14,60)
      (zsecond
      )
      (
        Apply("D__Dropbox_Zoe_Source_FS_zoe",23,60)
        (
          Apply("D__Dropbox_Zoe_Source_FS_zoe",23,60)
          (list
          )
          (
            Func ("D__Dropbox_Zoe_Source_FS_zoe",30,60)
            (\ x -> 
              Func ("D__Dropbox_Zoe_Source_FS_zoe",32,60)
              (\ zoefpD__Dropbox_Zoe_Source_FS_zoe_32_60@
                  (Tuple _
                    [xs
                      ,_
                    ])->
                  
                  Tuple ("D__Dropbox_Zoe_Source_FS_zoe",42,60)
                  [
                  Concat ("D__Dropbox_Zoe_Source_FS_zoe",43,60)
                  [
                    
                    Listprim ("D__Dropbox_Zoe_Source_FS_zoe",43,60)
                    [
                    x
                    ]
                    ,xs
                  ]
                  ,xs]
              )
            )
          )
        )
        (
          Tuple ("D__Dropbox_Zoe_Source_FS_zoe",59,60)
          [
          Listprim ("D__Dropbox_Zoe_Source_FS_zoe",60,60)
          [
          ]
          ,
          Listprim ("D__Dropbox_Zoe_Source_FS_zoe",63,60)
          [
          ]]
        )
      )
    )
  )

zhead = eval
  (
    Func ("D__Dropbox_Zoe_Source_FS_zoe",7,61)
    (\ list -> 
      
      Apply("D__Dropbox_Zoe_Source_FS_zoe",14,61)
      (
        Apply("D__Dropbox_Zoe_Source_FS_zoe",14,61)
        (list
        )
        (
          Func ("D__Dropbox_Zoe_Source_FS_zoe",21,61)
          (\ x -> 
            Func ("D__Dropbox_Zoe_Source_FS_zoe",23,61)
            (\ xs -> 
              x
            )
          )
        )
      )
      (
        Listprim ("D__Dropbox_Zoe_Source_FS_zoe",32,61)
        [
        ]
      )
    )
  )

zreverse = eval
  (
    Func ("D__Dropbox_Zoe_Source_FS_zoe",10,63)
    (\ list -> 
      
      Apply("D__Dropbox_Zoe_Source_FS_zoe",17,63)
      (
        Apply("D__Dropbox_Zoe_Source_FS_zoe",17,63)
        (list
        )
        (
          Func ("D__Dropbox_Zoe_Source_FS_zoe",24,63)
          (\ x -> 
            Func ("D__Dropbox_Zoe_Source_FS_zoe",26,63)
            (\ xs -> 
              
              Concat ("D__Dropbox_Zoe_Source_FS_zoe",32,63)
              [
                xs
                ,
                Listprim ("D__Dropbox_Zoe_Source_FS_zoe",38,63)
                [
                x
                ]
              ]
            )
          )
        )
      )
      (
        Listprim ("D__Dropbox_Zoe_Source_FS_zoe",43,63)
        [
        ]
      )
    )
  )

zzip = eval
  (
    Func ("D__Dropbox_Zoe_Source_FS_zoe",6,64)
    (\ l1 -> 
      Func ("D__Dropbox_Zoe_Source_FS_zoe",9,64)
      (\ l2 -> 
        
        Apply("D__Dropbox_Zoe_Source_FS_zoe",14,64)
        (zfirst
        )
        (
          Apply("D__Dropbox_Zoe_Source_FS_zoe",22,64)
          (
            Apply("D__Dropbox_Zoe_Source_FS_zoe",22,64)
            (l1
            )
            (
              Func ("D__Dropbox_Zoe_Source_FS_zoe",27,64)
              (\ x -> 
                Func ("D__Dropbox_Zoe_Source_FS_zoe",29,64)
                (\ zoefpD__Dropbox_Zoe_Source_FS_zoe_29_64@
                    (Tuple _
                      [rs
                        ,ls
                      ])->
                    
                    Tuple ("D__Dropbox_Zoe_Source_FS_zoe",40,64)
                    [
                    Concat ("D__Dropbox_Zoe_Source_FS_zoe",41,64)
                    [
                      
                      Listprim ("D__Dropbox_Zoe_Source_FS_zoe",41,64)
                      [
                      
                      Tuple ("D__Dropbox_Zoe_Source_FS_zoe",42,64)
                      [x
                      ,
                      Apply("D__Dropbox_Zoe_Source_FS_zoe",46,64)
                      (zhead
                      )
                      (ls
                      )]
                      ]
                      ,rs
                    ]
                    ,
                    Apply("D__Dropbox_Zoe_Source_FS_zoe",64,64)
                    (ztail
                    )
                    (ls
                    )]
                )
              )
            )
          )
          (
            Tuple ("D__Dropbox_Zoe_Source_FS_zoe",76,64)
            [
            Listprim ("D__Dropbox_Zoe_Source_FS_zoe",77,64)
            [
            ]
            ,
            Apply("D__Dropbox_Zoe_Source_FS_zoe",81,64)
            (zreverse
            )
            (l2
            )]
          )
        )
      )
    )
  )

set = Ftype("D__Dropbox_Zoe_Source_FS_zoe<empty><single><union>",1,66)

empty = 
  Func ("D__Dropbox_Zoe_Source_FS_zoe<empty>",12,66)
    (\ zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_66_1 ->
    Func ("D__Dropbox_Zoe_Source_FS_zoe<empty>",12,66)
      (\ zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_66_2 ->
      Func ("D__Dropbox_Zoe_Source_FS_zoe<empty>",12,66)
        (\ zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_66_3 ->
        zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_66_1
        )
      )
    )

single = 
  Func ("D__Dropbox_Zoe_Source_FS_zoe<single>",18,66)
    (\ zoearg_D__Dropbox_Zoe_Source_FS_zoe_1_66_1 ->
      Func ("D__Dropbox_Zoe_Source_FS_zoe<single>",18,66)
        (\ zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_66_1 ->
        Func ("D__Dropbox_Zoe_Source_FS_zoe<single>",18,66)
          (\ zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_66_2 ->
          Func ("D__Dropbox_Zoe_Source_FS_zoe<single>",18,66)
            (\ zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_66_3 ->
            Apply("D__Dropbox_Zoe_Source_FS_zoe<single>",18,66)
            (
            zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_66_2
            )
            (Tcheck ("D__Dropbox_Zoe_Source_FS_zoe<single>",18,66)zoearg_D__Dropbox_Zoe_Source_FS_zoe_1_66_1 (Nums ("D__Dropbox_Zoe_Source_FS_zoe",27,66))
            )
          )
        )
      )
  )

union = 
  Func ("D__Dropbox_Zoe_Source_FS_zoe<union>",31,66)
    (\ zoearg_D__Dropbox_Zoe_Source_FS_zoe_1_66_1 ->
    Func ("D__Dropbox_Zoe_Source_FS_zoe<union>",31,66)
      (\ zoearg_D__Dropbox_Zoe_Source_FS_zoe_1_66_2 ->
        Func ("D__Dropbox_Zoe_Source_FS_zoe<union>",31,66)
          (\ zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_66_1 ->
          Func ("D__Dropbox_Zoe_Source_FS_zoe<union>",31,66)
            (\ zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_66_2 ->
            Func ("D__Dropbox_Zoe_Source_FS_zoe<union>",31,66)
              (\ zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_66_3 ->
              Apply("D__Dropbox_Zoe_Source_FS_zoe<union>",31,66)
              (
                Apply("D__Dropbox_Zoe_Source_FS_zoe<union>",31,66)
                (zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_66_3
                )
                (
                  Apply("D__Dropbox_Zoe_Source_FS_zoe<union>",31,66)
                  (
                    Apply("D__Dropbox_Zoe_Source_FS_zoe<union>",31,66)
                    (
                      Apply("D__Dropbox_Zoe_Source_FS_zoe<union>",31,66)
                      (Tcheck ("D__Dropbox_Zoe_Source_FS_zoe<union>",31,66) zoearg_D__Dropbox_Zoe_Source_FS_zoe_1_66_1 set
                      )
                      (zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_66_1
                      )
                    )
                    (zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_66_2
                    )
                  )
                  (zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_66_3
                  )
                )
              )
              (
                Apply("D__Dropbox_Zoe_Source_FS_zoe<union>",31,66)
                (
                  Apply("D__Dropbox_Zoe_Source_FS_zoe<union>",31,66)
                  (
                    Apply("D__Dropbox_Zoe_Source_FS_zoe<union>",31,66)
                    (Tcheck ("D__Dropbox_Zoe_Source_FS_zoe<union>",31,66) zoearg_D__Dropbox_Zoe_Source_FS_zoe_1_66_2 set
                    )
                    (zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_66_1
                    )
                  )
                  (zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_66_2
                  )
                )
                (zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_66_3
                )
              )
            )
          )
        )
    )
  )


set1 = eval
  (
    Apply("D__Dropbox_Zoe_Source_FS_zoe",8,68)
    (
      Apply("D__Dropbox_Zoe_Source_FS_zoe",8,68)
      (union
      )
      (
        Apply("D__Dropbox_Zoe_Source_FS_zoe",15,68)
        (
          Apply("D__Dropbox_Zoe_Source_FS_zoe",15,68)
          (union
          )
          (
            Apply("D__Dropbox_Zoe_Source_FS_zoe",22,68)
            (single
            )
            (Numprim ("D__Dropbox_Zoe_Source_FS_zoe",29,68)1
            )
          )
        )
        (
          Apply("D__Dropbox_Zoe_Source_FS_zoe",33,68)
          (single
          )
          (Numprim ("D__Dropbox_Zoe_Source_FS_zoe",40,68)2
          )
        )
      )
    )
    (
      Apply("D__Dropbox_Zoe_Source_FS_zoe",45,68)
      (single
      )
      (Numprim ("D__Dropbox_Zoe_Source_FS_zoe",52,68)3
      )
    )
  )

set2 = eval
  (
    Apply("D__Dropbox_Zoe_Source_FS_zoe",8,69)
    (
      Apply("D__Dropbox_Zoe_Source_FS_zoe",8,69)
      (union
      )
      (
        Apply("D__Dropbox_Zoe_Source_FS_zoe",15,69)
        (
          Apply("D__Dropbox_Zoe_Source_FS_zoe",15,69)
          (union
          )
          (
            Apply("D__Dropbox_Zoe_Source_FS_zoe",22,69)
            (single
            )
            (Numprim ("D__Dropbox_Zoe_Source_FS_zoe",29,69)5
            )
          )
        )
        (
          Apply("D__Dropbox_Zoe_Source_FS_zoe",33,69)
          (single
          )
          (Numprim ("D__Dropbox_Zoe_Source_FS_zoe",40,69)6
          )
        )
      )
    )
    (
      Apply("D__Dropbox_Zoe_Source_FS_zoe",45,69)
      (single
      )
      (Numprim ("D__Dropbox_Zoe_Source_FS_zoe",52,69)7
      )
    )
  )

uni = eval
  (
    Func ("D__Dropbox_Zoe_Source_FS_zoe",5,70)
    (\ s1 -> 
      Func ("D__Dropbox_Zoe_Source_FS_zoe",8,70)
      (\ s2 -> 
        
        Apply("D__Dropbox_Zoe_Source_FS_zoe",13,70)
        (
          Apply("D__Dropbox_Zoe_Source_FS_zoe",13,70)
          (union
          )
          (s1
          )
        )
        (s2
        )
      )
    )
  )

member = eval
  (
    Func ("D__Dropbox_Zoe_Source_FS_zoe",8,72)
    (\ s -> 
      Func ("D__Dropbox_Zoe_Source_FS_zoe",10,72)
      (\ e -> 
        
        (FoldPacket("D__Dropbox_Zoe_Source_FS_zoe",14,72) s set (
          Namstup ("D__Dropbox_Zoe_Source_FS_zoe",26,72) [
            Tuple ("D__Dropbox_Zoe_Source_FS_zoe",5,73) [Nam ("D__Dropbox_Zoe_Source_FS_zoe",5,73)"empty" ,Boolprim ("D__Dropbox_Zoe_Source_FS_zoe",13,73)False],
            Tuple ("D__Dropbox_Zoe_Source_FS_zoe",18,73) [Nam ("D__Dropbox_Zoe_Source_FS_zoe",5,74)"single" ,
              Func ("D__Dropbox_Zoe_Source_FS_zoe",16,74)
              (\ x -> 
                (CompareOp ("D__Dropbox_Zoe_Source_FS_zoe",21,74)[x,Equal ("D__Dropbox_Zoe_Source_FS_zoe",23,74) (e)])
              )],
            Tuple ("D__Dropbox_Zoe_Source_FS_zoe",28,74) [Nam ("D__Dropbox_Zoe_Source_FS_zoe",5,75)"union" ,
              Func ("D__Dropbox_Zoe_Source_FS_zoe",15,75)
              (\ s1 -> 
                Func ("D__Dropbox_Zoe_Source_FS_zoe",18,75)
                (\ s2 -> 
                  
                  Or ("D__Dropbox_Zoe_Source_FS_zoe",24,75)
                  [
                    s1
                    ,s2
                  ]
                )
              )]
          ])
        )
      )
    )
  )

zdata = Ftype("D__Dropbox_Zoe_Source_FS_zoe<pair><int><bool><zlist>",1,79)

pair = 
  Func ("D__Dropbox_Zoe_Source_FS_zoe<pair>",14,79)
    (\ zoearg_D__Dropbox_Zoe_Source_FS_zoe_1_79_1 ->
    Func ("D__Dropbox_Zoe_Source_FS_zoe<pair>",14,79)
      (\ zoearg_D__Dropbox_Zoe_Source_FS_zoe_1_79_2 ->
        Func ("D__Dropbox_Zoe_Source_FS_zoe<pair>",14,79)
          (\ zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_79_1 ->
          Func ("D__Dropbox_Zoe_Source_FS_zoe<pair>",14,79)
            (\ zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_79_2 ->
            Func ("D__Dropbox_Zoe_Source_FS_zoe<pair>",14,79)
              (\ zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_79_3 ->
              Func ("D__Dropbox_Zoe_Source_FS_zoe<pair>",14,79)
                (\ zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_79_4 ->
                Apply("D__Dropbox_Zoe_Source_FS_zoe<pair>",14,79)
                (
                  Apply("D__Dropbox_Zoe_Source_FS_zoe<pair>",14,79)
                  (zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_79_1
                  )
                  (
                    Apply("D__Dropbox_Zoe_Source_FS_zoe<pair>",14,79)
                    (
                      Apply("D__Dropbox_Zoe_Source_FS_zoe<pair>",14,79)
                      (
                        Apply("D__Dropbox_Zoe_Source_FS_zoe<pair>",14,79)
                        (
                          Apply("D__Dropbox_Zoe_Source_FS_zoe<pair>",14,79)
                          (Tcheck ("D__Dropbox_Zoe_Source_FS_zoe<pair>",14,79) zoearg_D__Dropbox_Zoe_Source_FS_zoe_1_79_1 zdata
                          )
                          (zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_79_1
                          )
                        )
                        (zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_79_2
                        )
                      )
                      (zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_79_3
                      )
                    )
                    (zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_79_4
                    )
                  )
                )
                (
                  Apply("D__Dropbox_Zoe_Source_FS_zoe<pair>",14,79)
                  (
                    Apply("D__Dropbox_Zoe_Source_FS_zoe<pair>",14,79)
                    (
                      Apply("D__Dropbox_Zoe_Source_FS_zoe<pair>",14,79)
                      (
                        Apply("D__Dropbox_Zoe_Source_FS_zoe<pair>",14,79)
                        (Tcheck ("D__Dropbox_Zoe_Source_FS_zoe<pair>",14,79) zoearg_D__Dropbox_Zoe_Source_FS_zoe_1_79_2 zdata
                        )
                        (zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_79_1
                        )
                      )
                      (zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_79_2
                      )
                    )
                    (zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_79_3
                    )
                  )
                  (zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_79_4
                  )
                )
              )
            )
          )
        )
    )
  )

int = 
  Func ("D__Dropbox_Zoe_Source_FS_zoe<int>",23,79)
    (\ zoearg_D__Dropbox_Zoe_Source_FS_zoe_1_79_1 ->
      Func ("D__Dropbox_Zoe_Source_FS_zoe<int>",23,79)
        (\ zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_79_1 ->
        Func ("D__Dropbox_Zoe_Source_FS_zoe<int>",23,79)
          (\ zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_79_2 ->
          Func ("D__Dropbox_Zoe_Source_FS_zoe<int>",23,79)
            (\ zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_79_3 ->
            Func ("D__Dropbox_Zoe_Source_FS_zoe<int>",23,79)
              (\ zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_79_4 ->
              Apply("D__Dropbox_Zoe_Source_FS_zoe<int>",23,79)
              (
              zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_79_2
              )
              (Tcheck ("D__Dropbox_Zoe_Source_FS_zoe<int>",23,79)zoearg_D__Dropbox_Zoe_Source_FS_zoe_1_79_1 (Nums ("D__Dropbox_Zoe_Source_FS_zoe",29,79))
              )
            )
          )
        )
      )
  )

bool = 
  Func ("D__Dropbox_Zoe_Source_FS_zoe<bool>",33,79)
    (\ zoearg_D__Dropbox_Zoe_Source_FS_zoe_1_79_1 ->
      Func ("D__Dropbox_Zoe_Source_FS_zoe<bool>",33,79)
        (\ zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_79_1 ->
        Func ("D__Dropbox_Zoe_Source_FS_zoe<bool>",33,79)
          (\ zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_79_2 ->
          Func ("D__Dropbox_Zoe_Source_FS_zoe<bool>",33,79)
            (\ zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_79_3 ->
            Func ("D__Dropbox_Zoe_Source_FS_zoe<bool>",33,79)
              (\ zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_79_4 ->
              Apply("D__Dropbox_Zoe_Source_FS_zoe<bool>",33,79)
              (
              zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_79_3
              )
              (Tcheck ("D__Dropbox_Zoe_Source_FS_zoe<bool>",33,79)zoearg_D__Dropbox_Zoe_Source_FS_zoe_1_79_1 (Bools ("D__Dropbox_Zoe_Source_FS_zoe",40,79))
              )
            )
          )
        )
      )
  )

zlist = 
  Func ("D__Dropbox_Zoe_Source_FS_zoe<zlist>",45,79)
    (\ zoearg_D__Dropbox_Zoe_Source_FS_zoe_1_79_1 ->
      Func ("D__Dropbox_Zoe_Source_FS_zoe<zlist>",45,79)
        (\ zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_79_1 ->
        Func ("D__Dropbox_Zoe_Source_FS_zoe<zlist>",45,79)
          (\ zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_79_2 ->
          Func ("D__Dropbox_Zoe_Source_FS_zoe<zlist>",45,79)
            (\ zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_79_3 ->
            Func ("D__Dropbox_Zoe_Source_FS_zoe<zlist>",45,79)
              (\ zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_79_4 ->
              Apply("D__Dropbox_Zoe_Source_FS_zoe<zlist>",45,79)
              (
              zoeop_D__Dropbox_Zoe_Source_FS_zoe_1_79_4
              )
              (Tcheck ("D__Dropbox_Zoe_Source_FS_zoe<zlist>",45,79)zoearg_D__Dropbox_Zoe_Source_FS_zoe_1_79_1 (Listprim ("D__Dropbox_Zoe_Source_FS_zoe",53,79) [Nums ("D__Dropbox_Zoe_Source_FS_zoe",54,79)])
              )
            )
          )
        )
      )
  )


zprint = eval
  (
    Func ("D__Dropbox_Zoe_Source_FS_zoe",8,81)
    (\ dt -> 
      
      (FoldPacket("D__Dropbox_Zoe_Source_FS_zoe",13,81) dt zdata (
        Namstup ("D__Dropbox_Zoe_Source_FS_zoe",28,81) [
          Tuple ("D__Dropbox_Zoe_Source_FS_zoe",4,82) [Nam ("D__Dropbox_Zoe_Source_FS_zoe",4,82)"pair" ,
            Func ("D__Dropbox_Zoe_Source_FS_zoe",13,82)
            (\ x -> 
              Func ("D__Dropbox_Zoe_Source_FS_zoe",15,82)
              (\ y -> 
                
                Concat ("D__Dropbox_Zoe_Source_FS_zoe",20,82)
                [
                  x
                  ,
                  Strprim ("D__Dropbox_Zoe_Source_FS_zoe",25,82)" "
                  ,y
                ]
              )
            )],
          Tuple ("D__Dropbox_Zoe_Source_FS_zoe",34,82) [Nam ("D__Dropbox_Zoe_Source_FS_zoe",4,83)"zdefault" ,
            Func ("D__Dropbox_Zoe_Source_FS_zoe",17,83)
            (\ x -> 
              (Showop ("D__Dropbox_Zoe_Source_FS_zoe",22,83)(x))
            )]
        ])
      )
    )
  )
