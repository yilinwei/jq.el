;;; -*- lexical-binding: t; -*-

(require 'json)

(setq lexical-binding t)

(defsubst jq/curry (f &rest args)
  (lambda (&rest rest) (apply f (append args rest))))

(cl-defun jq/skip-obj (succ err)
  "Skip a JSON object where SUCC and ERR are continuations."
  (if (search-forward-regexp (rx (or ?{  ?})))
      (pcase (match-string-no-properties 0)
	((rx ?{) (jq/skip-obj succ err))
	((rx ?}) (funcall succ)))
    (funcall err)))

(cl-defun jq/skip-field (succ err)
  "Skip a attribute-value where SUCC and ERR are continuations."
  (if (search-forward-regexp (rx (or ?{ ?, ?})))
      (pcase (match-string-no-properties 0)
	;;skipped a field
	((rx ?,) (funcall succ))
	((rx ?}) (funcall err))
	((rx ?{) (jq/skip-obj succ err)))
    (funcall err)))

(cl-defun jq/match-field (field succ err)
  "Match FIELD where SUCC and ERR are continuations."
  (cl-letf
      ((attrib (rx ?\" (group (1+ word)) ?\" ?:)))
    (cl-flet
	;; define matchers
	((field-p (jq/curry #'string-equal field)))
	(letrec
	    ((seek
	      (lambda ()
		(if (search-forward-regexp attrib)
		    (pcase (match-string-no-properties 1)
		      ;; success
		      ((pred field-p) (funcall succ))
		      ;; other field
		      (_ (jq/skip-field seek err)))
		  (funcall err)))))
	  (funcall seek)))))

(cl-defun jq-run-string (expr str)
  "Run a `jq' expression on STR, returning the json result."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (funcall expr #'json-read (lambda () nil))))


(cl-defun jq/field-syntax (sym)
  (let
      ((name (symbol-name sym)))
    (string-match (rx ?. (group (1+ word))) name)
    (match-string-no-properties 1 name)))

(defmacro jq (exprs)
  "jq macro."
  (pcase exprs
    (`(,(and expr (pred symbolp)) . ,exprs)
     (let
	 ;; field accessor
	 ((field (jq/field-syntax expr)))
       `(lambda (succ err)
	  ,(cond
	    (field
	     `(jq/match-field
	       ,field
	       ;;on success
	       (lambda () (funcall  (jq ,exprs) succ err))
	       err))))))
    (_
     `(lambda (succ err)
	(funcall succ)))))

(provide 'jq)
