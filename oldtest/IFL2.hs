module IFL2 where
import ZRTE
import ZPOST

zstst1=
  Apply("_Users_WTW_thesis_project_IFL2_zoe",1,11)
  (
    Apply("_Users_WTW_thesis_project_IFL2_zoe",1,11)
    (
      Apply("_Users_WTW_thesis_project_IFL2_zoe",1,11)
      (Strprim ("_Users_WTW_thesis_project_IFL2_zoe",1,11)"qaz"
      )
      (Strprim ("_Users_WTW_thesis_project_IFL2_zoe",7,11)"qazws"
      )
    )
    (
      Func ("_Users_WTW_thesis_project_IFL2_zoe",18,11)
      (\ a -> 
        Func ("_Users_WTW_thesis_project_IFL2_zoe",20,11)
        (\ b -> 
          
          Listprim ("_Users_WTW_thesis_project_IFL2_zoe",25,11)
          [
          a
          ,
          b
          ]
        )
      )
    )
  )
  (
    Func ("_Users_WTW_thesis_project_IFL2_zoe",35,11)
    (\ c -> 
      Func ("_Users_WTW_thesis_project_IFL2_zoe",37,11)
      (\ d -> 
        
        Listprim ("_Users_WTW_thesis_project_IFL2_zoe",42,11)
        [
        c
        ,
        d
        ]
      )
    )
  )

zstst2=
  Apply("_Users_WTW_thesis_project_IFL2_zoe",1,15)
  (
    Apply("_Users_WTW_thesis_project_IFL2_zoe",1,15)
    (
      Apply("_Users_WTW_thesis_project_IFL2_zoe",1,15)
      (Strprim ("_Users_WTW_thesis_project_IFL2_zoe",1,15)"qaz"
      )
      (Strprim ("_Users_WTW_thesis_project_IFL2_zoe",7,15)"qwerty"
      )
    )
    (
      Func ("_Users_WTW_thesis_project_IFL2_zoe",19,15)
      (\ a -> 
        Func ("_Users_WTW_thesis_project_IFL2_zoe",21,15)
        (\ b -> 
          
          Listprim ("_Users_WTW_thesis_project_IFL2_zoe",26,15)
          [
          a
          ,
          b
          ]
        )
      )
    )
  )
  (
    Func ("_Users_WTW_thesis_project_IFL2_zoe",36,15)
    (\ c -> 
      Func ("_Users_WTW_thesis_project_IFL2_zoe",38,15)
      (\ d -> 
        
        Listprim ("_Users_WTW_thesis_project_IFL2_zoe",43,15)
        [
        c
        ,
        d
        ]
      )
    )
  )

tok =
  Func ("_Users_WTW_thesis_project_IFL2_zoe",5,19)
  (\ s -> 
    
    Func ("_Users_WTW_thesis_project_IFL2_zoe",12,19)
    (\ ss -> 
      
      Apply("_Users_WTW_thesis_project_IFL2_zoe",18,19)
      (
        Apply("_Users_WTW_thesis_project_IFL2_zoe",18,19)
        (
          Apply("_Users_WTW_thesis_project_IFL2_zoe",18,19)
          (s
          )
          (ss
          )
        )
        (
          Func ("_Users_WTW_thesis_project_IFL2_zoe",26,19)
          (\ residue -> 
            Func ("_Users_WTW_thesis_project_IFL2_zoe",34,19)
            (\ comp -> 
              
              Listprim ("_Users_WTW_thesis_project_IFL2_zoe",42,19)
              [
              residue
              ]
            )
          )
        )
      )
      (
        Func ("_Users_WTW_thesis_project_IFL2_zoe",55,19)
        (\ residue -> 
          Func ("_Users_WTW_thesis_project_IFL2_zoe",63,19)
          (\ comp -> 
            
            Listprim ("_Users_WTW_thesis_project_IFL2_zoe",71,19)
            [
            ]
          )
        )
      )
    )
  )

ytst =
  Func ("_Users_WTW_thesis_project_IFL2_zoe",6,31)
  (\ s -> 
    
    Apply("_Users_WTW_thesis_project_IFL2_zoe",1,33)
    (y
    )
    (
      Func ("_Users_WTW_thesis_project_IFL2_zoe",6,33)
      (\ ss -> 
        
        Concat ("_Users_WTW_thesis_project_IFL2_zoe",12,33)
        [
          
          Listprim ("_Users_WTW_thesis_project_IFL2_zoe",12,33)
          [
          s
          ]
          ,ss
        ]
      )
    )
  )

ytst2 =
  Func ("_Users_WTW_thesis_project_IFL2_zoe",7,35)
  (\ s -> 
    
    Apply("_Users_WTW_thesis_project_IFL2_zoe",1,37)
    (y
    )
    (
      Func ("_Users_WTW_thesis_project_IFL2_zoe",6,37)
      (\ ss -> 
        
        Concat ("_Users_WTW_thesis_project_IFL2_zoe",12,37)
        [
          
          Listprim ("_Users_WTW_thesis_project_IFL2_zoe",12,37)
          [
          s
          ]
          ,
          Listprim ("_Users_WTW_thesis_project_IFL2_zoe",19,37)
          [
          ss
          ]
        ]
      )
    )
  )
