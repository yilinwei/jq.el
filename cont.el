;;; -*- lexical-binding: t; -*-

(setq lexical-binding t)

(cl-defmacro reset (expr &rest exprs)
  (letrec
      ((rewrite
	;; `ret' is an early return
	(lambda (ret expr)
	  (pcase expr
	    (`(shift ,(and id (pred symbolp sym)) ,expr)
	     (let*
		 ((sym (gensym))
		  (subst (funcall ret sym)))
	       `(cl-flet
		    ((,id (lambda (,sym) ,subst)))
		  ,expr)))
	    (`(,expr . ,exprs)
	     ;; redex: [E/v] -> 
	     (funcall rewrite
		      (lambda (t)
			(funcall rewrite
				 (lambda (h)
				   (funcall ret (cons h t)))
				 expr))
		      exprs))
	    ;; redex : VAL -> VAL
	    (_ (funcall ret expr)))
	  )
	))
    (funcall rewrite #'identity expr)))

(provide 'cont)
