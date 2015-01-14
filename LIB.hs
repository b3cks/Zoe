module LIB where
import ZRTE
import ZPOST

add = eval
  (
    Func ("D__Dropbox_Zoe_Source_LIB_zoe",5,3)
    (\ x -> 
      Func ("D__Dropbox_Zoe_Source_LIB_zoe",7,3)
      (\ y -> 
        
        Add ("D__Dropbox_Zoe_Source_LIB_zoe",11,3)
        [
          x
          ,y
        ]
      )
    )
  )

list = Ftype("D__Dropbox_Zoe_Source_LIB_zoe<consl><nil>",1,6)

consl = 
  Func ("D__Dropbox_Zoe_Source_LIB_zoe<consl>",13,6)
    (\ zoearg_D__Dropbox_Zoe_Source_LIB_zoe_1_6_1 ->
    Func ("D__Dropbox_Zoe_Source_LIB_zoe<consl>",13,6)
      (\ zoearg_D__Dropbox_Zoe_Source_LIB_zoe_1_6_2 ->
        Func ("D__Dropbox_Zoe_Source_LIB_zoe<consl>",13,6)
          (\ zoeop_D__Dropbox_Zoe_Source_LIB_zoe_1_6_1 ->
          Func ("D__Dropbox_Zoe_Source_LIB_zoe<consl>",13,6)
            (\ zoeop_D__Dropbox_Zoe_Source_LIB_zoe_1_6_2 ->
            Apply("D__Dropbox_Zoe_Source_LIB_zoe<consl>",13,6)
            (
              Apply("D__Dropbox_Zoe_Source_LIB_zoe<consl>",13,6)
              (zoeop_D__Dropbox_Zoe_Source_LIB_zoe_1_6_1
              )
              (
              Tcheck ("D__Dropbox_Zoe_Source_LIB_zoe<consl>",13,6)zoearg_D__Dropbox_Zoe_Source_LIB_zoe_1_6_1 (Anys ("D__Dropbox_Zoe_Source_LIB_zoe",19,6))
              )
            )
            (
              Apply("D__Dropbox_Zoe_Source_LIB_zoe<consl>",13,6)
              (
                Apply("D__Dropbox_Zoe_Source_LIB_zoe<consl>",13,6)
                (Tcheck ("D__Dropbox_Zoe_Source_LIB_zoe<consl>",13,6) zoearg_D__Dropbox_Zoe_Source_LIB_zoe_1_6_2 list
                )
                (zoeop_D__Dropbox_Zoe_Source_LIB_zoe_1_6_1
                )
              )
              (zoeop_D__Dropbox_Zoe_Source_LIB_zoe_1_6_2
              )
            )
          )
        )
    )
  )

nil = 
  Func ("D__Dropbox_Zoe_Source_LIB_zoe<nil>",25,6)
    (\ zoeop_D__Dropbox_Zoe_Source_LIB_zoe_1_6_1 ->
    Func ("D__Dropbox_Zoe_Source_LIB_zoe<nil>",25,6)
      (\ zoeop_D__Dropbox_Zoe_Source_LIB_zoe_1_6_2 ->
      zoeop_D__Dropbox_Zoe_Source_LIB_zoe_1_6_2
      )
    )


bintree = Ftype("D__Dropbox_Zoe_Source_LIB_zoe<node><leaf>",1,8)

node = 
  Func ("D__Dropbox_Zoe_Source_LIB_zoe<node>",16,8)
    (\ zoearg_D__Dropbox_Zoe_Source_LIB_zoe_1_8_1 ->
    Func ("D__Dropbox_Zoe_Source_LIB_zoe<node>",16,8)
      (\ zoearg_D__Dropbox_Zoe_Source_LIB_zoe_1_8_2 ->
        Func ("D__Dropbox_Zoe_Source_LIB_zoe<node>",16,8)
          (\ zoeop_D__Dropbox_Zoe_Source_LIB_zoe_1_8_1 ->
          Func ("D__Dropbox_Zoe_Source_LIB_zoe<node>",16,8)
            (\ zoeop_D__Dropbox_Zoe_Source_LIB_zoe_1_8_2 ->
            Apply("D__Dropbox_Zoe_Source_LIB_zoe<node>",16,8)
            (
              Apply("D__Dropbox_Zoe_Source_LIB_zoe<node>",16,8)
              (zoeop_D__Dropbox_Zoe_Source_LIB_zoe_1_8_1
              )
              (
                Apply("D__Dropbox_Zoe_Source_LIB_zoe<node>",16,8)
                (
                  Apply("D__Dropbox_Zoe_Source_LIB_zoe<node>",16,8)
                  (Tcheck ("D__Dropbox_Zoe_Source_LIB_zoe<node>",16,8) zoearg_D__Dropbox_Zoe_Source_LIB_zoe_1_8_1 bintree
                  )
                  (zoeop_D__Dropbox_Zoe_Source_LIB_zoe_1_8_1
                  )
                )
                (zoeop_D__Dropbox_Zoe_Source_LIB_zoe_1_8_2
                )
              )
            )
            (
              Apply("D__Dropbox_Zoe_Source_LIB_zoe<node>",16,8)
              (
                Apply("D__Dropbox_Zoe_Source_LIB_zoe<node>",16,8)
                (Tcheck ("D__Dropbox_Zoe_Source_LIB_zoe<node>",16,8) zoearg_D__Dropbox_Zoe_Source_LIB_zoe_1_8_2 bintree
                )
                (zoeop_D__Dropbox_Zoe_Source_LIB_zoe_1_8_1
                )
              )
              (zoeop_D__Dropbox_Zoe_Source_LIB_zoe_1_8_2
              )
            )
          )
        )
    )
  )

leaf = 
  Func ("D__Dropbox_Zoe_Source_LIB_zoe<leaf>",25,8)
    (\ zoearg_D__Dropbox_Zoe_Source_LIB_zoe_1_8_1 ->
      Func ("D__Dropbox_Zoe_Source_LIB_zoe<leaf>",25,8)
        (\ zoeop_D__Dropbox_Zoe_Source_LIB_zoe_1_8_1 ->
        Func ("D__Dropbox_Zoe_Source_LIB_zoe<leaf>",25,8)
          (\ zoeop_D__Dropbox_Zoe_Source_LIB_zoe_1_8_2 ->
          Apply("D__Dropbox_Zoe_Source_LIB_zoe<leaf>",25,8)
          (
          zoeop_D__Dropbox_Zoe_Source_LIB_zoe_1_8_2
          )
          (Tcheck ("D__Dropbox_Zoe_Source_LIB_zoe<leaf>",25,8)zoearg_D__Dropbox_Zoe_Source_LIB_zoe_1_8_1 (Anys ("D__Dropbox_Zoe_Source_LIB_zoe",32,8))
          )
        )
      )
  )


unary = eval
  (
    Neg ("D__Dropbox_Zoe_Source_LIB_zoe",9,9)
    (
      Numprim ("D__Dropbox_Zoe_Source_LIB_zoe",11,9)2
    )
  )

bool = eval
  (
    Or ("D__Dropbox_Zoe_Source_LIB_zoe",8,10)
    [
      Boolprim ("D__Dropbox_Zoe_Source_LIB_zoe",8,10)True
      ,Boolprim ("D__Dropbox_Zoe_Source_LIB_zoe",16,10)False
    ]
  )

cal = eval
  (
    Add ("D__Dropbox_Zoe_Source_LIB_zoe",7,11)
    [
      Numprim ("D__Dropbox_Zoe_Source_LIB_zoe",7,11)1
      ,
      Numprim ("D__Dropbox_Zoe_Source_LIB_zoe",11,11)2
      ,
      
      Neg ("D__Dropbox_Zoe_Source_LIB_zoe",13,11)
      (
        Numprim ("D__Dropbox_Zoe_Source_LIB_zoe",15,11)3
      )
      ,
      Numprim ("D__Dropbox_Zoe_Source_LIB_zoe",19,11)1
      ,
      Neg ("D__Dropbox_Zoe_Source_LIB_zoe",21,11)
      (
        Numprim ("D__Dropbox_Zoe_Source_LIB_zoe",23,11)6
      )
    ]
  )

cal1 = eval
  (
    Add ("D__Dropbox_Zoe_Source_LIB_zoe",8,12)
    [
      
      Neg ("D__Dropbox_Zoe_Source_LIB_zoe",8,12)
      (
        Numprim ("D__Dropbox_Zoe_Source_LIB_zoe",10,12)1
      )
      ,Numprim ("D__Dropbox_Zoe_Source_LIB_zoe",15,12)2
    ]
  )

plus2 = eval
  (
    Func ("D__Dropbox_Zoe_Source_LIB_zoe",7,13)
    (\ x -> 
      
      Add ("D__Dropbox_Zoe_Source_LIB_zoe",11,13)
      [
        x
        ,Numprim ("D__Dropbox_Zoe_Source_LIB_zoe",15,13)2
      ]
    )
  )

a = eval
  (Boolprim ("D__Dropbox_Zoe_Source_LIB_zoe",5,14)True
  )
