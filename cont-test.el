(require 'cont)

(ert-deftest reset-simple-test ()
  (should
   (=
    (reset (+ 3 (shift k (k 2)) 1)) 6)))

(ert-deftest yield-simple-test ()
  (should
   (equal
    (reset
     (yield 1)
     (yield 2))
    '(1 2))))
