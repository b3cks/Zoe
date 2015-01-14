module BINTREE where
import ZRTE
import ZPOST

node = 
  Func ("D:\\Dropbox\\Zoe\\Source_BINTREE_zoe",1,10)
    (\ zoearg_D_Dropbox_Zoe_Source_BINTREE_zoe_1_10_1 ->
    Func ("D:\\Dropbox\\Zoe\\Source_BINTREE_zoe",1,10)
      (\ zoearg_D_Dropbox_Zoe_Source_BINTREE_zoe_1_10_2 ->
        Func ("D:\\Dropbox\\Zoe\\Source_BINTREE_zoe",1,10)
          (\ zoeop_D_Dropbox_Zoe_Source_BINTREE_zoe_1_10_1 ->
          Func ("D:\\Dropbox\\Zoe\\Source_BINTREE_zoe",1,10)
            (\ zoeop_D_Dropbox_Zoe_Source_BINTREE_zoe_1_10_2 ->
            Apply("D:\\Dropbox\\Zoe\\Source_BINTREE_zoe",1,10)
            (
              Apply("D:\\Dropbox\\Zoe\\Source_BINTREE_zoe",1,10)
              (zoeop_D_Dropbox_Zoe_Source_BINTREE_zoe_1_10_1
              )
              (
                Apply("D:\\Dropbox\\Zoe\\Source_BINTREE_zoe",1,10)
                (
                  Apply("D:\\Dropbox\\Zoe\\Source_BINTREE_zoe",1,10)
                  (zoearg_D_Dropbox_Zoe_Source_BINTREE_zoe_1_10_1
                  )
                  (zoeop_D_Dropbox_Zoe_Source_BINTREE_zoe_1_10_1
                  )
                )
                (zoeop_D_Dropbox_Zoe_Source_BINTREE_zoe_1_10_2
                )
              )
            )
            (
              Apply("D:\\Dropbox\\Zoe\\Source_BINTREE_zoe",1,10)
              (
                Apply("D:\\Dropbox\\Zoe\\Source_BINTREE_zoe",1,10)
                (zoearg_D_Dropbox_Zoe_Source_BINTREE_zoe_1_10_2
                )
                (zoeop_D_Dropbox_Zoe_Source_BINTREE_zoe_1_10_1
                )
              )
              (zoeop_D_Dropbox_Zoe_Source_BINTREE_zoe_1_10_2
              )
            )
          )
        )
    )
  )

leaf = 
  Func ("D:\\Dropbox\\Zoe\\Source_BINTREE_zoe",25,10)
    (\ zoearg_D_Dropbox_Zoe_Source_BINTREE_zoe_1_10_1 ->
      Func ("D:\\Dropbox\\Zoe\\Source_BINTREE_zoe",25,10)
        (\ zoeop_D_Dropbox_Zoe_Source_BINTREE_zoe_1_10_1 ->
        Func ("D:\\Dropbox\\Zoe\\Source_BINTREE_zoe",25,10)
          (\ zoeop_D_Dropbox_Zoe_Source_BINTREE_zoe_1_10_2 ->
          Apply("D:\\Dropbox\\Zoe\\Source_BINTREE_zoe",25,10)
          (
          zoeop_D_Dropbox_Zoe_Source_BINTREE_zoe_1_10_2
          )
          (zoearg_D_Dropbox_Zoe_Source_BINTREE_zoe_1_10_1
          )
        )
      )
  )


printtree =
  Func ("D:\\Dropbox\\Zoe\\Source_BINTREE_zoe",11,18)
  (\ bintree -> 
    
    Apply("D:\\Dropbox\\Zoe\\Source_BINTREE_zoe",21,18)
    (
      Apply("D:\\Dropbox\\Zoe\\Source_BINTREE_zoe",21,18)
      (bintree
      )
      (
        Func ("D:\\Dropbox\\Zoe\\Source_BINTREE_zoe",32,18)
        (\ x -> 
          Func ("D:\\Dropbox\\Zoe\\Source_BINTREE_zoe",34,18)
          (\ y -> 
            
            Concat ("D:\\Dropbox\\Zoe\\Source_BINTREE_zoe",39,18)
            [
              Strprim ("D:\\Dropbox\\Zoe\\Source_BINTREE_zoe",39,18)"("
              ,
              x
              ,
              y
              ,Strprim ("D:\\Dropbox\\Zoe\\Source_BINTREE_zoe",56,18)")"
            ]
          )
        )
      )
    )
    (
      Func ("D:\\Dropbox\\Zoe\\Source_BINTREE_zoe",63,18)
      (\ x -> 
        
        Concat ("D:\\Dropbox\\Zoe\\Source_BINTREE_zoe",68,18)
        [
          Strprim ("D:\\Dropbox\\Zoe\\Source_BINTREE_zoe",68,18)"("
          ,
          x
          ,Strprim ("D:\\Dropbox\\Zoe\\Source_BINTREE_zoe",80,18)")"
        ]
      )
    )
  )

 
  