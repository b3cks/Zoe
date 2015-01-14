module ZPOST where
import ZRTE


universe = eval
  (
    Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",3,13)
    (\ x -> 
      Boolprim ("D__Dropbox_Zoe_Source_ZPOST_zoe",8,13)True
    )
  )

compose = eval
  (
    Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",4,17)
    (\ f -> 
      Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",6,17)
      (\ g -> 
        Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",8,17)
        (\ x -> 
          
          Apply("D__Dropbox_Zoe_Source_ZPOST_zoe",13,17)
          (f
          )
          (
            Apply("D__Dropbox_Zoe_Source_ZPOST_zoe",16,17)
            (g
            )
            (x
            )
          )
        )
      )
    )
  )

zid = eval
  (
    Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",5,19)
    (\ x -> 
      x
    )
  )

znot = eval
  (
    Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",6,21)
    (\ b -> 
      
      Apply("D__Dropbox_Zoe_Source_ZPOST_zoe",10,21)
      (
        Apply("D__Dropbox_Zoe_Source_ZPOST_zoe",10,21)
        (b
        )
        (Boolprim ("D__Dropbox_Zoe_Source_ZPOST_zoe",12,21)False
        )
      )
      (Boolprim ("D__Dropbox_Zoe_Source_ZPOST_zoe",15,21)True
      )
    )
  )

t2s1 = eval
  (
    Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",6,23)
    (\ zoefpD__Dropbox_Zoe_Source_ZPOST_zoe_6_23@
        (Tuple _
          [a
            ,b
          ])->
        a
    )
  )

t2s2 = eval
  (
    Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",6,24)
    (\ zoefpD__Dropbox_Zoe_Source_ZPOST_zoe_6_24@
        (Tuple _
          [a
            ,b
          ])->
        b
    )
  )

eq = eval
  (
    Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",4,29)
    (\ s1 -> 
      Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",7,29)
      (\ s2 -> 
        
        Apply("D__Dropbox_Zoe_Source_ZPOST_zoe",12,29)
        (
          Apply("D__Dropbox_Zoe_Source_ZPOST_zoe",12,29)
          (
            Apply("D__Dropbox_Zoe_Source_ZPOST_zoe",12,29)
            (s1
            )
            (s2
            )
          )
          (
            Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",21,29)
            (\ residue -> 
              Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",29,29)
              (\ comp -> 
                
                Apply("D__Dropbox_Zoe_Source_ZPOST_zoe",37,29)
                (znot
                )
                (comp
                )
              )
            )
          )
        )
        (
          Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",50,29)
          (\ residue -> 
            Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",58,29)
            (\ comp -> 
              Boolprim ("D__Dropbox_Zoe_Source_ZPOST_zoe",66,29)False
            )
          )
        )
      )
    )
  )

ne = eval
  (
    Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",4,30)
    (\ s1 -> 
      Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",7,30)
      (\ s2 -> 
        
        Apply("D__Dropbox_Zoe_Source_ZPOST_zoe",12,30)
        (
          Apply("D__Dropbox_Zoe_Source_ZPOST_zoe",12,30)
          (
            Apply("D__Dropbox_Zoe_Source_ZPOST_zoe",12,30)
            (s1
            )
            (s2
            )
          )
          (
            Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",21,30)
            (\ residue -> 
              Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",29,30)
              (\ comp -> 
                comp
              )
            )
          )
        )
        (
          Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",45,30)
          (\ residue -> 
            Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",53,30)
            (\ comp -> 
              Boolprim ("D__Dropbox_Zoe_Source_ZPOST_zoe",61,30)True
            )
          )
        )
      )
    )
  )

ge = eval
  (
    Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",4,31)
    (\ s1 -> 
      Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",7,31)
      (\ s2 -> 
        
        Apply("D__Dropbox_Zoe_Source_ZPOST_zoe",12,31)
        (
          Apply("D__Dropbox_Zoe_Source_ZPOST_zoe",12,31)
          (
            Apply("D__Dropbox_Zoe_Source_ZPOST_zoe",12,31)
            (s1
            )
            (s2
            )
          )
          (
            Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",21,31)
            (\ residue -> 
              Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",29,31)
              (\ comp -> 
                Boolprim ("D__Dropbox_Zoe_Source_ZPOST_zoe",37,31)False
              )
            )
          )
        )
        (
          Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",43,31)
          (\ residue -> 
            Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",51,31)
            (\ comp -> 
              
              Apply("D__Dropbox_Zoe_Source_ZPOST_zoe",59,31)
              (znot
              )
              (comp
              )
            )
          )
        )
      )
    )
  )

le = eval
  (
    Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",4,32)
    (\ s1 -> 
      Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",7,32)
      (\ s2 -> 
        
        Apply("D__Dropbox_Zoe_Source_ZPOST_zoe",12,32)
        (
          Apply("D__Dropbox_Zoe_Source_ZPOST_zoe",12,32)
          (
            Apply("D__Dropbox_Zoe_Source_ZPOST_zoe",12,32)
            (s1
            )
            (s2
            )
          )
          (
            Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",21,32)
            (\ residue -> 
              Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",29,32)
              (\ comp -> 
                Boolprim ("D__Dropbox_Zoe_Source_ZPOST_zoe",37,32)True
              )
            )
          )
        )
        (
          Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",43,32)
          (\ residue -> 
            Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",51,32)
            (\ comp -> 
              Boolprim ("D__Dropbox_Zoe_Source_ZPOST_zoe",59,32)True
            )
          )
        )
      )
    )
  )

gt = eval
  (
    Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",4,33)
    (\ s1 -> 
      Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",7,33)
      (\ s2 -> 
        
        Apply("D__Dropbox_Zoe_Source_ZPOST_zoe",12,33)
        (
          Apply("D__Dropbox_Zoe_Source_ZPOST_zoe",12,33)
          (
            Apply("D__Dropbox_Zoe_Source_ZPOST_zoe",12,33)
            (s1
            )
            (s2
            )
          )
          (
            Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",21,33)
            (\ residue -> 
              Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",29,33)
              (\ comp -> 
                Boolprim ("D__Dropbox_Zoe_Source_ZPOST_zoe",37,33)False
              )
            )
          )
        )
        (
          Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",43,33)
          (\ residue -> 
            Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",51,33)
            (\ comp -> 
              
              Apply("D__Dropbox_Zoe_Source_ZPOST_zoe",59,33)
              (znot
              )
              (comp
              )
            )
          )
        )
      )
    )
  )

lt = eval
  (
    Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",4,34)
    (\ s1 -> 
      Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",7,34)
      (\ s2 -> 
        
        Apply("D__Dropbox_Zoe_Source_ZPOST_zoe",12,34)
        (
          Apply("D__Dropbox_Zoe_Source_ZPOST_zoe",12,34)
          (
            Apply("D__Dropbox_Zoe_Source_ZPOST_zoe",12,34)
            (s1
            )
            (s2
            )
          )
          (
            Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",21,34)
            (\ residue -> 
              Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",29,34)
              (\ comp -> 
                comp
              )
            )
          )
        )
        (
          Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",45,34)
          (\ residue -> 
            Func ("D__Dropbox_Zoe_Source_ZPOST_zoe",53,34)
            (\ comp -> 
              comp
            )
          )
        )
      )
    )
  )
