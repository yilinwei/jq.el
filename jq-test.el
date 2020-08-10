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
 (jq .foo)
 '((bar . 1) (foo . 42)) 42)

(jq/deftest
 field-nested-test
 (jq .foo .bar)
 '((foo . ((bar . "moo")))) "moo")

(ert-deftest for-each-test ()
  (should
   (equal
    (with-temp-buffer
      (insert "[1, 2, 3]")
      (goto-char (point-min))
      (let ((res nil))
	(jq/for-each
	 (lambda (next)
	   (add-to-list 'res (json-read))
	   (funcall next))
	 (lambda () nil))
	(reverse res)))
    '(1 2 3))))
