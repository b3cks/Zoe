module Main (main) where
import ZRTE
import ZPOST
import LIB

main :: IO()
main =
  (print . eval) (
    Add ("D:\\Dropbox\\Zoe\\Source_zoeinteraction_zoe",1,1)
    [
      
      Neg ("D:\\Dropbox\\Zoe\\Source_zoeinteraction_zoe",1,1)
      (
      Numprim ("D:\\Dropbox\\Zoe\\Source_zoeinteraction_zoe",3,1)2
      )
      ,Numprim ("D:\\Dropbox\\Zoe\\Source_zoeinteraction_zoe",6,1)3
    ]
  )
