module TC where
import ZRTE
import ZPOST

i1 = eval
  (
    Tcheck("D__Dropbox_Zoe_Source_TC_zoe",6,9)
    (
    Add ("D__Dropbox_Zoe_Source_TC_zoe",7,9)
    [
      Numprim ("D__Dropbox_Zoe_Source_TC_zoe",7,9)1
      ,Numprim ("D__Dropbox_Zoe_Source_TC_zoe",11,9)2
    ]) ((Nums ("D__Dropbox_Zoe_Source_TC_zoe",17,9)))
  )

i2 = eval
  (
    Tcheck("D__Dropbox_Zoe_Source_TC_zoe",6,10)
    ((CompareOp ("D__Dropbox_Zoe_Source_TC_zoe",7,10)[Numprim ("D__Dropbox_Zoe_Source_TC_zoe",7,10)1,Lt ("D__Dropbox_Zoe_Source_TC_zoe",9,10) (Numprim ("D__Dropbox_Zoe_Source_TC_zoe",11,10)2)])) ((Nums ("D__Dropbox_Zoe_Source_TC_zoe",17,10)))
  )

b1 = eval
  (
    Tcheck("D__Dropbox_Zoe_Source_TC_zoe",6,14)
    ((CompareOp ("D__Dropbox_Zoe_Source_TC_zoe",7,14)[Numprim ("D__Dropbox_Zoe_Source_TC_zoe",7,14)1,Lt ("D__Dropbox_Zoe_Source_TC_zoe",9,14) (Numprim ("D__Dropbox_Zoe_Source_TC_zoe",11,14)2)])) ((Bools ("D__Dropbox_Zoe_Source_TC_zoe",17,14)))
  )

b2 = eval
  (
    Tcheck("D__Dropbox_Zoe_Source_TC_zoe",6,15)
    (
    Concat ("D__Dropbox_Zoe_Source_TC_zoe",7,15)
    [
      Strprim ("D__Dropbox_Zoe_Source_TC_zoe",7,15)"a"
      ,Strprim ("D__Dropbox_Zoe_Source_TC_zoe",14,15)"b"
    ]) ((Bools ("D__Dropbox_Zoe_Source_TC_zoe",22,15)))
  )

l1 = eval
  (
    Tcheck("D__Dropbox_Zoe_Source_TC_zoe",6,19)
    (
    Listprim ("D__Dropbox_Zoe_Source_TC_zoe",6,19)
    [
    Numprim ("D__Dropbox_Zoe_Source_TC_zoe",7,19)1
    ,
    Numprim ("D__Dropbox_Zoe_Source_TC_zoe",9,19)2
    ,
    Numprim ("D__Dropbox_Zoe_Source_TC_zoe",11,19)3
    ]) ((Lists ("D__Dropbox_Zoe_Source_TC_zoe",17,19)))
  )

l2 = eval
  (
    Tcheck("D__Dropbox_Zoe_Source_TC_zoe",6,20)
    (
    Listprim ("D__Dropbox_Zoe_Source_TC_zoe",6,20)
    [
    Numprim ("D__Dropbox_Zoe_Source_TC_zoe",7,20)1
    ,
    Numprim ("D__Dropbox_Zoe_Source_TC_zoe",9,20)2
    ,
    Boolprim ("D__Dropbox_Zoe_Source_TC_zoe",11,20)True
    ]) ((Lists ("D__Dropbox_Zoe_Source_TC_zoe",20,20)))
  )

l3 = eval
  (
    Tcheck("D__Dropbox_Zoe_Source_TC_zoe",6,24)
    (
    Listprim ("D__Dropbox_Zoe_Source_TC_zoe",6,24)
    [
    Numprim ("D__Dropbox_Zoe_Source_TC_zoe",7,24)1
    ,
    Numprim ("D__Dropbox_Zoe_Source_TC_zoe",9,24)2
    ,
    Numprim ("D__Dropbox_Zoe_Source_TC_zoe",11,24)3
    ]) (
    Listprim ("D__Dropbox_Zoe_Source_TC_zoe",17,24)
    [
    (Nums ("D__Dropbox_Zoe_Source_TC_zoe",18,24))
    ])
  )

l4 = eval
  (
    Tcheck("D__Dropbox_Zoe_Source_TC_zoe",6,25)
    (
    Listprim ("D__Dropbox_Zoe_Source_TC_zoe",6,25)
    [
    Numprim ("D__Dropbox_Zoe_Source_TC_zoe",7,25)1
    ,
    Numprim ("D__Dropbox_Zoe_Source_TC_zoe",9,25)2
    ,
    Boolprim ("D__Dropbox_Zoe_Source_TC_zoe",11,25)True
    ]) (
    Listprim ("D__Dropbox_Zoe_Source_TC_zoe",20,25)
    [
    (Nums ("D__Dropbox_Zoe_Source_TC_zoe",21,25))
    ])
  )

zadd = eval
  (
    Func ("D__Dropbox_Zoe_Source_TC_zoe",6,29)
    (\ x -> 
      Func ("D__Dropbox_Zoe_Source_TC_zoe",8,29)
      (\ y -> 
        
        Add ("D__Dropbox_Zoe_Source_TC_zoe",12,29)
        [
          x
          ,y
        ]
      )
    )
  )

zor = eval
  (
    Func ("D__Dropbox_Zoe_Source_TC_zoe",5,30)
    (\ x -> 
      Func ("D__Dropbox_Zoe_Source_TC_zoe",7,30)
      (\ y -> 
        
        Or ("D__Dropbox_Zoe_Source_TC_zoe",11,30)
        [
          x
          ,y
        ]
      )
    )
  )

apply = eval
  (
    Func ("D__Dropbox_Zoe_Source_TC_zoe",7,31)
    (\ op -> 
      Func ("D__Dropbox_Zoe_Source_TC_zoe",10,31)
      (\ x -> 
        Func ("D__Dropbox_Zoe_Source_TC_zoe",12,31)
        (\ y -> 
          
          Apply("D__Dropbox_Zoe_Source_TC_zoe",16,31)
          (
            Apply("D__Dropbox_Zoe_Source_TC_zoe",16,31)
            (op
            )
            (x
            )
          )
          (y
          )
        )
      )
    )
  )

f1 = eval
  (
    Func ("D__Dropbox_Zoe_Source_TC_zoe",4,32)
    (\ x -> 
      Func ("D__Dropbox_Zoe_Source_TC_zoe",6,32)
      (\ y -> 
        
        Apply("D__Dropbox_Zoe_Source_TC_zoe",10,32)
        (
          Apply("D__Dropbox_Zoe_Source_TC_zoe",10,32)
          (
            Apply("D__Dropbox_Zoe_Source_TC_zoe",10,32)
            (apply
            )
            (zadd
            )
          )
          (x
          )
        )
        (y
        )
      )
    )
  )

f2 = eval
  (
    Func ("D__Dropbox_Zoe_Source_TC_zoe",4,33)
    (\ x -> 
      Func ("D__Dropbox_Zoe_Source_TC_zoe",6,33)
      (\ y -> 
        
        Apply("D__Dropbox_Zoe_Source_TC_zoe",10,33)
        (
          Apply("D__Dropbox_Zoe_Source_TC_zoe",10,33)
          (
            Apply("D__Dropbox_Zoe_Source_TC_zoe",10,33)
            (apply
            )
            (zor
            )
          )
          (x
          )
        )
        (y
        )
      )
    )
  )

t1 = eval
  (
    Tcheck("D__Dropbox_Zoe_Source_TC_zoe",6,37)
    (
    Tuple ("D__Dropbox_Zoe_Source_TC_zoe",6,37)
    [Numprim ("D__Dropbox_Zoe_Source_TC_zoe",7,37)1
    ,Numprim ("D__Dropbox_Zoe_Source_TC_zoe",9,37)2
    ,Boolprim ("D__Dropbox_Zoe_Source_TC_zoe",11,37)True]) ((Tuples ("D__Dropbox_Zoe_Source_TC_zoe",20,37)))
  )

t2 = eval
  (
    Tcheck("D__Dropbox_Zoe_Source_TC_zoe",6,41)
    (
    Tuple ("D__Dropbox_Zoe_Source_TC_zoe",6,41)
    [Numprim ("D__Dropbox_Zoe_Source_TC_zoe",7,41)1
    ,Numprim ("D__Dropbox_Zoe_Source_TC_zoe",9,41)2
    ,Boolprim ("D__Dropbox_Zoe_Source_TC_zoe",11,41)True]) (
    Tuple ("D__Dropbox_Zoe_Source_TC_zoe",20,41)
    [(Nums ("D__Dropbox_Zoe_Source_TC_zoe",21,41))
    ,(Nums ("D__Dropbox_Zoe_Source_TC_zoe",25,41))
    ,(Bools ("D__Dropbox_Zoe_Source_TC_zoe",29,41))])
  )

t3 = eval
  (
    Tcheck("D__Dropbox_Zoe_Source_TC_zoe",6,42)
    (
    Tuple ("D__Dropbox_Zoe_Source_TC_zoe",6,42)
    [Numprim ("D__Dropbox_Zoe_Source_TC_zoe",7,42)1
    ,Boolprim ("D__Dropbox_Zoe_Source_TC_zoe",9,42)True]) (
    Tuple ("D__Dropbox_Zoe_Source_TC_zoe",18,42)
    [(Nums ("D__Dropbox_Zoe_Source_TC_zoe",19,42))
    ,(Nums ("D__Dropbox_Zoe_Source_TC_zoe",23,42))])
  )

t4 = eval
  (
    Tcheck("D__Dropbox_Zoe_Source_TC_zoe",6,43)
    (
    Tuple ("D__Dropbox_Zoe_Source_TC_zoe",6,43)
    [Numprim ("D__Dropbox_Zoe_Source_TC_zoe",7,43)1
    ,Numprim ("D__Dropbox_Zoe_Source_TC_zoe",9,43)2
    ,Numprim ("D__Dropbox_Zoe_Source_TC_zoe",11,43)3]) (
    Tuple ("D__Dropbox_Zoe_Source_TC_zoe",17,43)
    [(Nums ("D__Dropbox_Zoe_Source_TC_zoe",18,43))
    ,(Nums ("D__Dropbox_Zoe_Source_TC_zoe",22,43))])
  )

n1 = eval
  (
    Tcheck("D__Dropbox_Zoe_Source_TC_zoe",6,47)
    (
    Namstup ("D__Dropbox_Zoe_Source_TC_zoe",6,47) [
      Tuple ("D__Dropbox_Zoe_Source_TC_zoe",7,47) [Nam ("D__Dropbox_Zoe_Source_TC_zoe",7,47)"a" ,Numprim ("D__Dropbox_Zoe_Source_TC_zoe",9,47)1],
      Tuple ("D__Dropbox_Zoe_Source_TC_zoe",10,47) [Nam ("D__Dropbox_Zoe_Source_TC_zoe",11,47)"b" ,Boolprim ("D__Dropbox_Zoe_Source_TC_zoe",13,47)True]
    ]) (
    Namstup ("D__Dropbox_Zoe_Source_TC_zoe",20,47) [
      Tuple ("D__Dropbox_Zoe_Source_TC_zoe",21,47) [Nam ("D__Dropbox_Zoe_Source_TC_zoe",21,47)"a" ,(Nums ("D__Dropbox_Zoe_Source_TC_zoe",23,47))],
      Tuple ("D__Dropbox_Zoe_Source_TC_zoe",26,47) [Nam ("D__Dropbox_Zoe_Source_TC_zoe",27,47)"b" ,(Bools ("D__Dropbox_Zoe_Source_TC_zoe",29,47))]
    ])
  )

n2 = eval
  (
    Tcheck("D__Dropbox_Zoe_Source_TC_zoe",6,48)
    (
    Namstup ("D__Dropbox_Zoe_Source_TC_zoe",6,48) [
      Tuple ("D__Dropbox_Zoe_Source_TC_zoe",7,48) [Nam ("D__Dropbox_Zoe_Source_TC_zoe",7,48)"a" ,Numprim ("D__Dropbox_Zoe_Source_TC_zoe",9,48)1],
      Tuple ("D__Dropbox_Zoe_Source_TC_zoe",10,48) [Nam ("D__Dropbox_Zoe_Source_TC_zoe",11,48)"b" ,Boolprim ("D__Dropbox_Zoe_Source_TC_zoe",13,48)False]
    ]) (
    Namstup ("D__Dropbox_Zoe_Source_TC_zoe",21,48) [
      Tuple ("D__Dropbox_Zoe_Source_TC_zoe",22,48) [Nam ("D__Dropbox_Zoe_Source_TC_zoe",22,48)"a" ,(Nums ("D__Dropbox_Zoe_Source_TC_zoe",24,48))],
      Tuple ("D__Dropbox_Zoe_Source_TC_zoe",27,48) [Nam ("D__Dropbox_Zoe_Source_TC_zoe",28,48)"b" ,(Nums ("D__Dropbox_Zoe_Source_TC_zoe",30,48))]
    ])
  )

n3 = eval
  (
    Tcheck("D__Dropbox_Zoe_Source_TC_zoe",6,49)
    (
    Namstup ("D__Dropbox_Zoe_Source_TC_zoe",6,49) [
      Tuple ("D__Dropbox_Zoe_Source_TC_zoe",7,49) [Nam ("D__Dropbox_Zoe_Source_TC_zoe",7,49)"a" ,Numprim ("D__Dropbox_Zoe_Source_TC_zoe",9,49)1],
      Tuple ("D__Dropbox_Zoe_Source_TC_zoe",10,49) [Nam ("D__Dropbox_Zoe_Source_TC_zoe",11,49)"b" ,Boolprim ("D__Dropbox_Zoe_Source_TC_zoe",13,49)True]
    ]) (
    Namstup ("D__Dropbox_Zoe_Source_TC_zoe",20,49) [
      Tuple ("D__Dropbox_Zoe_Source_TC_zoe",21,49) [Nam ("D__Dropbox_Zoe_Source_TC_zoe",21,49)"a" ,(Nums ("D__Dropbox_Zoe_Source_TC_zoe",23,49))]
    ])
  )
