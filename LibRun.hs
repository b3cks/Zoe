module Main (main) where
import ZRTE
import ZPOST
import LIB


main :: IO()
main =
  (print . eval) (
    Apply("D:\\Dropbox\\Zoe\\Source_LibRun_zoe",1,1)
    (
      Apply("D:\\Dropbox\\Zoe\\Source_LibRun_zoe",1,1)
      (a
      )
      (Numprim ("D:\\Dropbox\\Zoe\\Source_LibRun_zoe",3,1)1
      )
    )
    (Numprim ("D:\\Dropbox\\Zoe\\Source_LibRun_zoe",5,1)2
    )
  )
