(require 'cont)

(ert-deftest reset-simple-test ()
  (should
   (=
    (reset (+ 3 (shift k (k 2)) 1)) 6)))
