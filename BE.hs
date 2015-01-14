module BE where
import ZRTE
import ZPOST

zand = eval
  (
    Func ("D__Dropbox_Zoe_Source_BE_zoe",6,9)
    (\ x -> 
      Func ("D__Dropbox_Zoe_Source_BE_zoe",8,9)
      (\ y -> 
        
        Apply("D__Dropbox_Zoe_Source_BE_zoe",12,9)
        (
          Apply("D__Dropbox_Zoe_Source_BE_zoe",12,9)
          (x
          )
          (y
          )
        )
        (x
        )
      )
    )
  )

zor = eval
  (
    Func ("D__Dropbox_Zoe_Source_BE_zoe",5,10)
    (\ x -> 
      Func ("D__Dropbox_Zoe_Source_BE_zoe",7,10)
      (\ y -> 
        
        Apply("D__Dropbox_Zoe_Source_BE_zoe",11,10)
        (
          Apply("D__Dropbox_Zoe_Source_BE_zoe",11,10)
          (x
          )
          (x
          )
        )
        (y
        )
      )
    )
  )

n1 = eval
  (
    Apply("D__Dropbox_Zoe_Source_BE_zoe",6,14)
    (
      Apply("D__Dropbox_Zoe_Source_BE_zoe",6,14)
      (Numprim ("D__Dropbox_Zoe_Source_BE_zoe",6,14)7
      )
      (
        Func ("D__Dropbox_Zoe_Source_BE_zoe",10,14)
        (\ x -> 
          
          Add ("D__Dropbox_Zoe_Source_BE_zoe",15,14)
          [
            x
            ,Numprim ("D__Dropbox_Zoe_Source_BE_zoe",19,14)1
          ]
        )
      )
    )
    (Numprim ("D__Dropbox_Zoe_Source_BE_zoe",22,14)0
    )
  )

isZero = eval
  (
    Func ("D__Dropbox_Zoe_Source_BE_zoe",12,18)
    (\ n -> 
      
      Apply("D__Dropbox_Zoe_Source_BE_zoe",17,18)
      (
        Apply("D__Dropbox_Zoe_Source_BE_zoe",17,18)
        (n
        )
        (
          Func ("D__Dropbox_Zoe_Source_BE_zoe",21,18)
          (\ x -> 
            Boolprim ("D__Dropbox_Zoe_Source_BE_zoe",25,18)False
          )
        )
      )
      (Boolprim ("D__Dropbox_Zoe_Source_BE_zoe",32,18)True
      )
    )
  )

leq = eval
  (
    Func ("D__Dropbox_Zoe_Source_BE_zoe",5,19)
    (\ m -> 
      Func ("D__Dropbox_Zoe_Source_BE_zoe",7,19)
      (\ n -> 
        
        Apply("D__Dropbox_Zoe_Source_BE_zoe",11,19)
        (isZero
        )
        (
          Add ("D__Dropbox_Zoe_Source_BE_zoe",19,19)
          [
            m
            ,
            Neg ("D__Dropbox_Zoe_Source_BE_zoe",21,19)
            (
              n
            )
          ]
        )
      )
    )
  )

s1 = eval
  (
    Apply("D__Dropbox_Zoe_Source_BE_zoe",6,23)
    (
      Apply("D__Dropbox_Zoe_Source_BE_zoe",6,23)
      (
        Apply("D__Dropbox_Zoe_Source_BE_zoe",6,23)
        (Strprim ("D__Dropbox_Zoe_Source_BE_zoe",6,23)"hello"
        )
        (Strprim ("D__Dropbox_Zoe_Source_BE_zoe",14,23)"hello world"
        )
      )
      (Boolprim ("D__Dropbox_Zoe_Source_BE_zoe",28,23)True
      )
    )
    (Boolprim ("D__Dropbox_Zoe_Source_BE_zoe",33,23)False
    )
  )

s2 = eval
  (
    Apply("D__Dropbox_Zoe_Source_BE_zoe",6,24)
    (
      Apply("D__Dropbox_Zoe_Source_BE_zoe",6,24)
      (
        Apply("D__Dropbox_Zoe_Source_BE_zoe",6,24)
        (Strprim ("D__Dropbox_Zoe_Source_BE_zoe",6,24)"hello"
        )
        (Strprim ("D__Dropbox_Zoe_Source_BE_zoe",14,24)"world"
        )
      )
      (Boolprim ("D__Dropbox_Zoe_Source_BE_zoe",22,24)True
      )
    )
    (Boolprim ("D__Dropbox_Zoe_Source_BE_zoe",27,24)False
    )
  )

s3 = eval
  (
    Apply("D__Dropbox_Zoe_Source_BE_zoe",6,25)
    (
      Apply("D__Dropbox_Zoe_Source_BE_zoe",6,25)
      (
        Apply("D__Dropbox_Zoe_Source_BE_zoe",6,25)
        (Strprim ("D__Dropbox_Zoe_Source_BE_zoe",6,25)"world"
        )
        (Strprim ("D__Dropbox_Zoe_Source_BE_zoe",14,25)"hello"
        )
      )
      (Boolprim ("D__Dropbox_Zoe_Source_BE_zoe",22,25)True
      )
    )
    (Boolprim ("D__Dropbox_Zoe_Source_BE_zoe",27,25)False
    )
  )

l1 = eval
  (
    Apply("D__Dropbox_Zoe_Source_BE_zoe",6,29)
    (
      Apply("D__Dropbox_Zoe_Source_BE_zoe",6,29)
      (
        Listprim ("D__Dropbox_Zoe_Source_BE_zoe",6,29)
        [
        Numprim ("D__Dropbox_Zoe_Source_BE_zoe",7,29)1
        ,
        Numprim ("D__Dropbox_Zoe_Source_BE_zoe",9,29)2
        ,
        Numprim ("D__Dropbox_Zoe_Source_BE_zoe",11,29)3
        ,
        Numprim ("D__Dropbox_Zoe_Source_BE_zoe",13,29)4
        ]
      )
      (
        Func ("D__Dropbox_Zoe_Source_BE_zoe",18,29)
        (\ x -> 
          Func ("D__Dropbox_Zoe_Source_BE_zoe",20,29)
          (\ xs -> 
            
            Add ("D__Dropbox_Zoe_Source_BE_zoe",26,29)
            [
              x
              ,xs
            ]
          )
        )
      )
    )
    (Numprim ("D__Dropbox_Zoe_Source_BE_zoe",34,29)0
    )
  )

zfirst = eval
  (
    Func ("D__Dropbox_Zoe_Source_BE_zoe",8,33)
    (\ tup -> 
      
      Apply("D__Dropbox_Zoe_Source_BE_zoe",14,33)
      (tup
      )
      (
        Func ("D__Dropbox_Zoe_Source_BE_zoe",20,33)
        (\ x -> 
          Func ("D__Dropbox_Zoe_Source_BE_zoe",22,33)
          (\ y -> 
            x
          )
        )
      )
    )
  )

zsecond = eval
  (
    Func ("D__Dropbox_Zoe_Source_BE_zoe",9,34)
    (\ tup -> 
      
      Apply("D__Dropbox_Zoe_Source_BE_zoe",15,34)
      (tup
      )
      (
        Func ("D__Dropbox_Zoe_Source_BE_zoe",21,34)
        (\ x -> 
          Func ("D__Dropbox_Zoe_Source_BE_zoe",23,34)
          (\ y -> 
            y
          )
        )
      )
    )
  )

nt1 = eval
  (
    Apply("D__Dropbox_Zoe_Source_BE_zoe",7,38)
    (
      Apply("D__Dropbox_Zoe_Source_BE_zoe",7,38)
      (
        Namstup ("D__Dropbox_Zoe_Source_BE_zoe",7,38) [
          Tuple ("D__Dropbox_Zoe_Source_BE_zoe",8,38) [Nam ("D__Dropbox_Zoe_Source_BE_zoe",8,38)"a" ,Numprim ("D__Dropbox_Zoe_Source_BE_zoe",12,38)1],
          Tuple ("D__Dropbox_Zoe_Source_BE_zoe",13,38) [Nam ("D__Dropbox_Zoe_Source_BE_zoe",15,38)"b" ,Numprim ("D__Dropbox_Zoe_Source_BE_zoe",19,38)2]
        ]
      )
      (Strprim ("D__Dropbox_Zoe_Source_BE_zoe",22,38)"a b"
      )
    )
    (
      Func ("D__Dropbox_Zoe_Source_BE_zoe",30,38)
      (\ x -> 
        Func ("D__Dropbox_Zoe_Source_BE_zoe",32,38)
        (\ y -> 
          x
        )
      )
    )
  )
