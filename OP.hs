module OP where
import ZRTE
import ZPOST

u1 = eval
  (
    Neg ("D__Dropbox_Zoe_Source_OP_zoe",6,10)
    (
      Numprim ("D__Dropbox_Zoe_Source_OP_zoe",8,10)7
    )
  )

u2 = eval
  (
    Not ("D__Dropbox_Zoe_Source_OP_zoe",6,11)
    (
      Boolprim ("D__Dropbox_Zoe_Source_OP_zoe",8,11)False
    )
  )

u3 = eval
  ((Showop ("D__Dropbox_Zoe_Source_OP_zoe",6,12)(
    Listprim ("D__Dropbox_Zoe_Source_OP_zoe",12,12)
    [
    Numprim ("D__Dropbox_Zoe_Source_OP_zoe",13,12)1
    ,
    Numprim ("D__Dropbox_Zoe_Source_OP_zoe",15,12)2
    ,
    Numprim ("D__Dropbox_Zoe_Source_OP_zoe",17,12)3
    ,
    Numprim ("D__Dropbox_Zoe_Source_OP_zoe",19,12)4
    ]))
  )

l1 = eval
  (
    Func ("D__Dropbox_Zoe_Source_OP_zoe",4,16)
    (\ x -> 
      
      Or ("D__Dropbox_Zoe_Source_OP_zoe",8,16)
      [
        (CompareOp ("D__Dropbox_Zoe_Source_OP_zoe",8,16)[x,LtEqual ("D__Dropbox_Zoe_Source_OP_zoe",10,16) (Numprim ("D__Dropbox_Zoe_Source_OP_zoe",13,16)5)])
        ,(CompareOp ("D__Dropbox_Zoe_Source_OP_zoe",18,16)[x,GtEqual ("D__Dropbox_Zoe_Source_OP_zoe",20,16) (Numprim ("D__Dropbox_Zoe_Source_OP_zoe",23,16)7)])
      ]
    )
  )

l2 = eval
  (
    Or ("D__Dropbox_Zoe_Source_OP_zoe",6,17)
    [
      Boolprim ("D__Dropbox_Zoe_Source_OP_zoe",6,17)True
      ,
      And ("D__Dropbox_Zoe_Source_OP_zoe",14,17)
      [
        Boolprim ("D__Dropbox_Zoe_Source_OP_zoe",14,17)True
        ,Boolprim ("D__Dropbox_Zoe_Source_OP_zoe",22,17)False
      ]
    ]
  )

i1 = eval
  (
    IndexOp ("D__Dropbox_Zoe_Source_OP_zoe",6,21)
    [
      
      Listprim ("D__Dropbox_Zoe_Source_OP_zoe",6,21)
      [
      Numprim ("D__Dropbox_Zoe_Source_OP_zoe",7,21)1
      ,
      Numprim ("D__Dropbox_Zoe_Source_OP_zoe",9,21)2
      ,
      Numprim ("D__Dropbox_Zoe_Source_OP_zoe",11,21)3
      ,
      Numprim ("D__Dropbox_Zoe_Source_OP_zoe",13,21)4
      ,
      Numprim ("D__Dropbox_Zoe_Source_OP_zoe",15,21)5
      ,
      Numprim ("D__Dropbox_Zoe_Source_OP_zoe",17,21)6
      ]
      ,Numprim ("D__Dropbox_Zoe_Source_OP_zoe",23,21)3
    ]
  )

namedTuple = eval
  (
    Namstup ("D__Dropbox_Zoe_Source_OP_zoe",14,25) [
      Tuple ("D__Dropbox_Zoe_Source_OP_zoe",15,25) [Nam ("D__Dropbox_Zoe_Source_OP_zoe",15,25)"a" ,Numprim ("D__Dropbox_Zoe_Source_OP_zoe",19,25)1],
      Tuple ("D__Dropbox_Zoe_Source_OP_zoe",20,25) [Nam ("D__Dropbox_Zoe_Source_OP_zoe",22,25)"b" ,Numprim ("D__Dropbox_Zoe_Source_OP_zoe",26,25)2],
      Tuple ("D__Dropbox_Zoe_Source_OP_zoe",27,25) [Nam ("D__Dropbox_Zoe_Source_OP_zoe",29,25)"c" ,
        Func ("D__Dropbox_Zoe_Source_OP_zoe",36,25)
        (\ x -> 
          Func ("D__Dropbox_Zoe_Source_OP_zoe",38,25)
          (\ y -> 
            
            Add ("D__Dropbox_Zoe_Source_OP_zoe",43,25)
            [
              x
              ,y
            ]
          )
        )]
    ]
  )

selector1 = eval
  ((NamstupSelector ("D__Dropbox_Zoe_Source_OP_zoe",13,27)(namedTuple) (Nam("D__Dropbox_Zoe_Source_OP_zoe",27,27) "a"))
  )

selector2 = eval
  ((NamstupSelector ("D__Dropbox_Zoe_Source_OP_zoe",13,28)(namedTuple) (Nam("D__Dropbox_Zoe_Source_OP_zoe",27,28) "b"))
  )

newTup = eval
  ((NamstupUpdate ("D__Dropbox_Zoe_Source_OP_zoe",10,30)(namedTuple)(
    Namstup ("D__Dropbox_Zoe_Source_OP_zoe",24,30) [
      Tuple ("D__Dropbox_Zoe_Source_OP_zoe",25,30) [Nam ("D__Dropbox_Zoe_Source_OP_zoe",25,30)"a" ,Numprim ("D__Dropbox_Zoe_Source_OP_zoe",29,30)4]
    ]))
  )
