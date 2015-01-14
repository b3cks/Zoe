module ZPOST where
import ZRTE


y=
  Func ("_Users_WTW_thesis_project_ZPOST_zoe",4,12)
  (\ a -> 
    
    Apply("_Users_WTW_thesis_project_ZPOST_zoe",9,12)
    (
      Func ("_Users_WTW_thesis_project_ZPOST_zoe",12,12)
      (\ b -> 
        
        Apply("_Users_WTW_thesis_project_ZPOST_zoe",17,12)
        (a
        )
        (
          Apply("_Users_WTW_thesis_project_ZPOST_zoe",20,12)
          (b
          )
          (b
          )
        )
      )
    )
    (
      Func ("_Users_WTW_thesis_project_ZPOST_zoe",28,12)
      (\ b -> 
        
        Apply("_Users_WTW_thesis_project_ZPOST_zoe",33,12)
        (a
        )
        (
          Apply("_Users_WTW_thesis_project_ZPOST_zoe",36,12)
          (b
          )
          (b
          )
        )
      )
    )
  )

universe=
  Func ("_Users_WTW_thesis_project_ZPOST_zoe",3,16)
  (\ x -> 
    Boolprim ("_Users_WTW_thesis_project_ZPOST_zoe",8,16)True
  )

compose=
  Func ("_Users_WTW_thesis_project_ZPOST_zoe",4,20)
  (\ f -> 
    Func ("_Users_WTW_thesis_project_ZPOST_zoe",6,20)
    (\ g -> 
      Func ("_Users_WTW_thesis_project_ZPOST_zoe",8,20)
      (\ x -> 
        
        Apply("_Users_WTW_thesis_project_ZPOST_zoe",13,20)
        (f
        )
        (
          Apply("_Users_WTW_thesis_project_ZPOST_zoe",16,20)
          (g
          )
          (x
          )
        )
      )
    )
  )

zid =
  Func ("_Users_WTW_thesis_project_ZPOST_zoe",5,22)
  (\ x -> 
    x
  )

znot =
  Func ("_Users_WTW_thesis_project_ZPOST_zoe",6,24)
  (\ b -> 
    
    Apply("_Users_WTW_thesis_project_ZPOST_zoe",10,24)
    (
      Apply("_Users_WTW_thesis_project_ZPOST_zoe",10,24)
      (b
      )
      (Boolprim ("_Users_WTW_thesis_project_ZPOST_zoe",12,24)False
      )
    )
    (Boolprim ("_Users_WTW_thesis_project_ZPOST_zoe",14,24)True
    )
  )

t2s1 =
  Func ("_Users_WTW_thesis_project_ZPOST_zoe",6,26)
  (\ zoefp6_26 -> 
    let
      Tuple _ [a,b] = eval zoefp6_26
    in
      a
  )

t2s2 =
  Func ("_Users_WTW_thesis_project_ZPOST_zoe",6,27)
  (\ zoefp6_27 -> 
    let
      Tuple _ [a,b] = eval zoefp6_27
    in
      b
  )

eq =
  Func ("_Users_WTW_thesis_project_ZPOST_zoe",4,32)
  (\ s1 -> 
    Func ("_Users_WTW_thesis_project_ZPOST_zoe",7,32)
    (\ s2 -> 
      
      Apply("_Users_WTW_thesis_project_ZPOST_zoe",12,32)
      (
        Apply("_Users_WTW_thesis_project_ZPOST_zoe",12,32)
        (
          Apply("_Users_WTW_thesis_project_ZPOST_zoe",12,32)
          (s1
          )
          (s2
          )
        )
        (
          Func ("_Users_WTW_thesis_project_ZPOST_zoe",21,32)
          (\ residue -> 
            Func ("_Users_WTW_thesis_project_ZPOST_zoe",29,32)
            (\ comp -> 
              
              Apply("_Users_WTW_thesis_project_ZPOST_zoe",37,32)
              (znot
              )
              (comp
              )
            )
          )
        )
      )
      (
        Func ("_Users_WTW_thesis_project_ZPOST_zoe",50,32)
        (\ residue -> 
          Func ("_Users_WTW_thesis_project_ZPOST_zoe",58,32)
          (\ comp -> 
            Boolprim ("_Users_WTW_thesis_project_ZPOST_zoe",66,32)False
          )
        )
      )
    )
  )

ne =
  Func ("_Users_WTW_thesis_project_ZPOST_zoe",4,33)
  (\ s1 -> 
    Func ("_Users_WTW_thesis_project_ZPOST_zoe",7,33)
    (\ s2 -> 
      
      Apply("_Users_WTW_thesis_project_ZPOST_zoe",12,33)
      (
        Apply("_Users_WTW_thesis_project_ZPOST_zoe",12,33)
        (
          Apply("_Users_WTW_thesis_project_ZPOST_zoe",12,33)
          (s1
          )
          (s2
          )
        )
        (
          Func ("_Users_WTW_thesis_project_ZPOST_zoe",21,33)
          (\ residue -> 
            Func ("_Users_WTW_thesis_project_ZPOST_zoe",29,33)
            (\ comp -> 
              comp
            )
          )
        )
      )
      (
        Func ("_Users_WTW_thesis_project_ZPOST_zoe",45,33)
        (\ residue -> 
          Func ("_Users_WTW_thesis_project_ZPOST_zoe",53,33)
          (\ comp -> 
            Boolprim ("_Users_WTW_thesis_project_ZPOST_zoe",61,33)True
          )
        )
      )
    )
  )

ge =
  Func ("_Users_WTW_thesis_project_ZPOST_zoe",4,34)
  (\ s1 -> 
    Func ("_Users_WTW_thesis_project_ZPOST_zoe",7,34)
    (\ s2 -> 
      
      Apply("_Users_WTW_thesis_project_ZPOST_zoe",12,34)
      (
        Apply("_Users_WTW_thesis_project_ZPOST_zoe",12,34)
        (
          Apply("_Users_WTW_thesis_project_ZPOST_zoe",12,34)
          (s1
          )
          (s2
          )
        )
        (
          Func ("_Users_WTW_thesis_project_ZPOST_zoe",21,34)
          (\ residue -> 
            Func ("_Users_WTW_thesis_project_ZPOST_zoe",29,34)
            (\ comp -> 
              Boolprim ("_Users_WTW_thesis_project_ZPOST_zoe",37,34)False
            )
          )
        )
      )
      (
        Func ("_Users_WTW_thesis_project_ZPOST_zoe",42,34)
        (\ residue -> 
          Func ("_Users_WTW_thesis_project_ZPOST_zoe",50,34)
          (\ comp -> 
            
            Apply("_Users_WTW_thesis_project_ZPOST_zoe",58,34)
            (znot
            )
            (comp
            )
          )
        )
      )
    )
  )

le =
  Func ("_Users_WTW_thesis_project_ZPOST_zoe",4,35)
  (\ s1 -> 
    Func ("_Users_WTW_thesis_project_ZPOST_zoe",7,35)
    (\ s2 -> 
      
      Apply("_Users_WTW_thesis_project_ZPOST_zoe",12,35)
      (
        Apply("_Users_WTW_thesis_project_ZPOST_zoe",12,35)
        (
          Apply("_Users_WTW_thesis_project_ZPOST_zoe",12,35)
          (s1
          )
          (s2
          )
        )
        (
          Func ("_Users_WTW_thesis_project_ZPOST_zoe",21,35)
          (\ residue -> 
            Func ("_Users_WTW_thesis_project_ZPOST_zoe",29,35)
            (\ comp -> 
              Boolprim ("_Users_WTW_thesis_project_ZPOST_zoe",37,35)True
            )
          )
        )
      )
      (
        Func ("_Users_WTW_thesis_project_ZPOST_zoe",42,35)
        (\ residue -> 
          Func ("_Users_WTW_thesis_project_ZPOST_zoe",50,35)
          (\ comp -> 
            Boolprim ("_Users_WTW_thesis_project_ZPOST_zoe",58,35)True
          )
        )
      )
    )
  )

gt =
  Func ("_Users_WTW_thesis_project_ZPOST_zoe",4,36)
  (\ s1 -> 
    Func ("_Users_WTW_thesis_project_ZPOST_zoe",7,36)
    (\ s2 -> 
      
      Apply("_Users_WTW_thesis_project_ZPOST_zoe",12,36)
      (
        Apply("_Users_WTW_thesis_project_ZPOST_zoe",12,36)
        (
          Apply("_Users_WTW_thesis_project_ZPOST_zoe",12,36)
          (s1
          )
          (s2
          )
        )
        (
          Func ("_Users_WTW_thesis_project_ZPOST_zoe",21,36)
          (\ residue -> 
            Func ("_Users_WTW_thesis_project_ZPOST_zoe",29,36)
            (\ comp -> 
              Boolprim ("_Users_WTW_thesis_project_ZPOST_zoe",37,36)False
            )
          )
        )
      )
      (
        Func ("_Users_WTW_thesis_project_ZPOST_zoe",42,36)
        (\ residue -> 
          Func ("_Users_WTW_thesis_project_ZPOST_zoe",50,36)
          (\ comp -> 
            
            Apply("_Users_WTW_thesis_project_ZPOST_zoe",58,36)
            (znot
            )
            (comp
            )
          )
        )
      )
    )
  )

lt =
  Func ("_Users_WTW_thesis_project_ZPOST_zoe",4,37)
  (\ s1 -> 
    Func ("_Users_WTW_thesis_project_ZPOST_zoe",7,37)
    (\ s2 -> 
      
      Apply("_Users_WTW_thesis_project_ZPOST_zoe",12,37)
      (
        Apply("_Users_WTW_thesis_project_ZPOST_zoe",12,37)
        (
          Apply("_Users_WTW_thesis_project_ZPOST_zoe",12,37)
          (s1
          )
          (s2
          )
        )
        (
          Func ("_Users_WTW_thesis_project_ZPOST_zoe",21,37)
          (\ residue -> 
            Func ("_Users_WTW_thesis_project_ZPOST_zoe",29,37)
            (\ comp -> 
              comp
            )
          )
        )
      )
      (
        Func ("_Users_WTW_thesis_project_ZPOST_zoe",45,37)
        (\ residue -> 
          Func ("_Users_WTW_thesis_project_ZPOST_zoe",53,37)
          (\ comp -> 
            comp
          )
        )
      )
    )
  )
