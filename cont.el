;;; -*- lexical-binding: t; -*-

(setq lexical-binding t)

(cl-defmacro yield (x)
  `(shift k (cons ,x (k nil))))

(cl-defmacro shift (id expr)
  "The `shift' control flow operator for delimited continuations. See `reset'."
  `(user-error "`shift' operator should be rewritten out, call in `reset' block."))

(cl-defmacro reset (expr &rest exprs)
  "Create a delimited continuation, rewriting control flow operators inside (EXPR . EXPRS)."
  (letrec
      ((rewrite
	;; `ret' is an early return
	(lambda (ret expr)
	  (pcase expr
	    ;;TODO: generalize control flow operators?
	    (`(yield ,expr)
	     (funcall rewrite ret (macroexpand-1 `(yield ,expr))))
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
		      (lambda (h)
			(funcall rewrite
				 (lambda (t)
				   (funcall ret (cons h t)))
				 exprs))
		      expr))
	    ;; redex : VAL -> VAL
	    (_ (funcall ret expr)))
	  )
	))
    (if exprs
	`(reset (progn ,expr ,@exprs))
      (funcall rewrite #'identity expr))))

(provide 'cont)
