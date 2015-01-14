module PM where
import ZRTE
import ZPOST

tup@
  (
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
      ])
  ) = eval
  (
    
    Tuple ("D__Dropbox_Zoe_Source_PM_zoe",28,8)
    [
    Tuple ("D__Dropbox_Zoe_Source_PM_zoe",29,8)
    [Numprim ("D__Dropbox_Zoe_Source_PM_zoe",30,8)1
    ,Numprim ("D__Dropbox_Zoe_Source_PM_zoe",32,8)2]
    ,
    Tuple ("D__Dropbox_Zoe_Source_PM_zoe",35,8)
    [Numprim ("D__Dropbox_Zoe_Source_PM_zoe",36,8)3
    ,Numprim ("D__Dropbox_Zoe_Source_PM_zoe",38,8)4]]
  )

getsecondfirst = eval
  (
    Func ("D__Dropbox_Zoe_Source_PM_zoe",16,11)
    (\ zoefpD__Dropbox_Zoe_Source_PM_zoe_16_11@
        (Tuple _
          [_
            ,
            (Tuple _
              [t1
                ,_
              ])
          ])->
        t1
    )
  )

func = eval
  (
    Func ("D__Dropbox_Zoe_Source_PM_zoe",6,12)
    (\ x -> 
      x
    )
  )

gete1 = eval
  (
    Func ("D__Dropbox_Zoe_Source_PM_zoe",7,17)
    (\ 
    (Listprim _
      [
        e1
        ,
        e2
        ,
        e3
      ]) -> 
      e1
    )
  )


(Listprim _
  [
    e1
    ,
    e2
    ,
    e3
  ]) = eval
  (
    
    Listprim ("D__Dropbox_Zoe_Source_PM_zoe",14,18)
    [
    Numprim ("D__Dropbox_Zoe_Source_PM_zoe",15,18)1
    ,
    Numprim ("D__Dropbox_Zoe_Source_PM_zoe",17,18)2
    ,
    Numprim ("D__Dropbox_Zoe_Source_PM_zoe",19,18)3
    ]
  )

gethead = eval
  (
    Func ("D__Dropbox_Zoe_Source_PM_zoe",9,23)
    (\ l@
    (Listprim _
      (h
        :tail_D__Dropbox_Zoe_Source_PM_zoe_11_23
      )) -> 
      let
        t = (Listprim ("D__Dropbox_Zoe_Source_PM_zoe",14,23) tail_D__Dropbox_Zoe_Source_PM_zoe_11_23)
      in
        h
    )
  )

gettail = eval
  (
    Func ("D__Dropbox_Zoe_Source_PM_zoe",9,24)
    (\ l@
    (Listprim _
      (h
        :tail_D__Dropbox_Zoe_Source_PM_zoe_11_24
      )) -> 
      let
        t = (Listprim ("D__Dropbox_Zoe_Source_PM_zoe",14,24) tail_D__Dropbox_Zoe_Source_PM_zoe_11_24)
      in
        t
    )
  )

getlist = eval
  (
    Func ("D__Dropbox_Zoe_Source_PM_zoe",9,25)
    (\ l@
    (Listprim _
      (h
        :tail_D__Dropbox_Zoe_Source_PM_zoe_11_25
      )) -> 
      let
        t = (Listprim ("D__Dropbox_Zoe_Source_PM_zoe",14,25) tail_D__Dropbox_Zoe_Source_PM_zoe_11_25)
      in
        l
    )
  )

bintree = Ftype("D__Dropbox_Zoe_Source_PM_zoe<branch><nil>",1,27)

branch = 
  Func ("D__Dropbox_Zoe_Source_PM_zoe<branch>",16,27)
    (\ zoearg_D__Dropbox_Zoe_Source_PM_zoe_1_27_1 ->
    Func ("D__Dropbox_Zoe_Source_PM_zoe<branch>",16,27)
      (\ zoearg_D__Dropbox_Zoe_Source_PM_zoe_1_27_2 ->
      Func ("D__Dropbox_Zoe_Source_PM_zoe<branch>",16,27)
        (\ zoearg_D__Dropbox_Zoe_Source_PM_zoe_1_27_3 ->
          Func ("D__Dropbox_Zoe_Source_PM_zoe<branch>",16,27)
            (\ zoeop_D__Dropbox_Zoe_Source_PM_zoe_1_27_1 ->
            Func ("D__Dropbox_Zoe_Source_PM_zoe<branch>",16,27)
              (\ zoeop_D__Dropbox_Zoe_Source_PM_zoe_1_27_2 ->
              Apply("D__Dropbox_Zoe_Source_PM_zoe<branch>",16,27)
              (
                Apply("D__Dropbox_Zoe_Source_PM_zoe<branch>",16,27)
                (
                  Apply("D__Dropbox_Zoe_Source_PM_zoe<branch>",16,27)
                  (zoeop_D__Dropbox_Zoe_Source_PM_zoe_1_27_1
                  )
                  (
                    Apply("D__Dropbox_Zoe_Source_PM_zoe<branch>",16,27)
                    (
                      Apply("D__Dropbox_Zoe_Source_PM_zoe<branch>",16,27)
                      (Tcheck ("D__Dropbox_Zoe_Source_PM_zoe<branch>",16,27) zoearg_D__Dropbox_Zoe_Source_PM_zoe_1_27_1 bintree
                      )
                      (zoeop_D__Dropbox_Zoe_Source_PM_zoe_1_27_1
                      )
                    )
                    (zoeop_D__Dropbox_Zoe_Source_PM_zoe_1_27_2
                    )
                  )
                )
                (
                Tcheck ("D__Dropbox_Zoe_Source_PM_zoe<branch>",16,27)zoearg_D__Dropbox_Zoe_Source_PM_zoe_1_27_2 (Nums ("D__Dropbox_Zoe_Source_PM_zoe",25,27))
                )
              )
              (
                Apply("D__Dropbox_Zoe_Source_PM_zoe<branch>",16,27)
                (
                  Apply("D__Dropbox_Zoe_Source_PM_zoe<branch>",16,27)
                  (Tcheck ("D__Dropbox_Zoe_Source_PM_zoe<branch>",16,27) zoearg_D__Dropbox_Zoe_Source_PM_zoe_1_27_3 bintree
                  )
                  (zoeop_D__Dropbox_Zoe_Source_PM_zoe_1_27_1
                  )
                )
                (zoeop_D__Dropbox_Zoe_Source_PM_zoe_1_27_2
                )
                )
            )
          )
      )
    )
  )

nil = 
  Func ("D__Dropbox_Zoe_Source_PM_zoe<nil>",31,27)
    (\ zoeop_D__Dropbox_Zoe_Source_PM_zoe_1_27_1 ->
    Func ("D__Dropbox_Zoe_Source_PM_zoe<nil>",31,27)
      (\ zoeop_D__Dropbox_Zoe_Source_PM_zoe_1_27_2 ->
      zoeop_D__Dropbox_Zoe_Source_PM_zoe_1_27_2
      )
    )


mytree = eval
  (
    Apply("D__Dropbox_Zoe_Source_PM_zoe",10,29)
    (
      Apply("D__Dropbox_Zoe_Source_PM_zoe",10,29)
      (
        Apply("D__Dropbox_Zoe_Source_PM_zoe",10,29)
        (branch
        )
        (
          Apply("D__Dropbox_Zoe_Source_PM_zoe",18,29)
          (
            Apply("D__Dropbox_Zoe_Source_PM_zoe",18,29)
            (
              Apply("D__Dropbox_Zoe_Source_PM_zoe",18,29)
              (branch
              )
              (nil
              )
            )
            (Numprim ("D__Dropbox_Zoe_Source_PM_zoe",29,29)1
            )
          )
          (nil
          )
        )
      )
      (Numprim ("D__Dropbox_Zoe_Source_PM_zoe",36,29)2
      )
    )
    (
      Apply("D__Dropbox_Zoe_Source_PM_zoe",39,29)
      (
        Apply("D__Dropbox_Zoe_Source_PM_zoe",39,29)
        (
          Apply("D__Dropbox_Zoe_Source_PM_zoe",39,29)
          (branch
          )
          (nil
          )
        )
        (Numprim ("D__Dropbox_Zoe_Source_PM_zoe",50,29)3
        )
      )
      (nil
      )
    )
  )

printt = eval
  (
    Func ("D__Dropbox_Zoe_Source_PM_zoe",8,31)
    (\ t -> 
      
      (FoldPacket("D__Dropbox_Zoe_Source_PM_zoe",12,31) t bintree (
        Namstup ("D__Dropbox_Zoe_Source_PM_zoe",30,31) [
          Tuple ("D__Dropbox_Zoe_Source_PM_zoe",5,32) [Nam ("D__Dropbox_Zoe_Source_PM_zoe",5,32)"branch" ,
            Func ("D__Dropbox_Zoe_Source_PM_zoe",16,32)
            (\ s1 -> 
              Func ("D__Dropbox_Zoe_Source_PM_zoe",19,32)
              (\ n -> 
                Func ("D__Dropbox_Zoe_Source_PM_zoe",21,32)
                (\ s2 -> 
                  
                  Concat ("D__Dropbox_Zoe_Source_PM_zoe",27,32)
                  [
                    s1
                    ,
                    (Showop ("D__Dropbox_Zoe_Source_PM_zoe",33,32)(n))
                    ,s2
                  ]
                )
              )
            )],
          Tuple ("D__Dropbox_Zoe_Source_PM_zoe",48,32) [Nam ("D__Dropbox_Zoe_Source_PM_zoe",5,33)"nil" ,Strprim ("D__Dropbox_Zoe_Source_PM_zoe",11,33)""]
        ])
      )
    )
  )

zhead = eval
  (
    Func ("D__Dropbox_Zoe_Source_PM_zoe",7,36)
    (\ ls -> 
      
      Apply("D__Dropbox_Zoe_Source_PM_zoe",12,36)
      (
        Apply("D__Dropbox_Zoe_Source_PM_zoe",12,36)
        (ls
        )
        (
          Func ("D__Dropbox_Zoe_Source_PM_zoe",17,36)
          (\ x -> 
            Func ("D__Dropbox_Zoe_Source_PM_zoe",19,36)
            (\ xs -> 
              x
            )
          )
        )
      )
      (
        Listprim ("D__Dropbox_Zoe_Source_PM_zoe",28,36)
        [
        ]
      )
    )
  )

h = eval
  (
    Apply("D__Dropbox_Zoe_Source_PM_zoe",5,37)
    (zhead
    )
    (
      Listprim ("D__Dropbox_Zoe_Source_PM_zoe",11,37)
      [
      Numprim ("D__Dropbox_Zoe_Source_PM_zoe",12,37)1
      ,
      Numprim ("D__Dropbox_Zoe_Source_PM_zoe",14,37)2
      ,
      Numprim ("D__Dropbox_Zoe_Source_PM_zoe",16,37)3
      ]
    )
  )
