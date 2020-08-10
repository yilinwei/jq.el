(require 'jq)
(require 'ert)

(defmacro jq/deftest (name jq json result)
  `(ert-deftest ,name ()
     (should
      (equal
       (jq-run-string ,jq (json-encode ,json))
       ,result))))

(jq/deftest
 field-depth1-test
 (jq (.foo))
 '((bar . 1) (foo . 42)) 42)

(jq/deftest
 field-nested-test
 (jq (.foo .bar))
 '((foo . ((bar . "moo")))) "moo")
