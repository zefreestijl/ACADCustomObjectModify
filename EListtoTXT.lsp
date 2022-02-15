(defun LOG2lst (e / file fopen line logf rest str itm)
  (setvar "logfilemode" 1)
  (setq	file (getvar "logfilename")
	dump nil
	logf nil
  )
  ;; setq
  (vlax-dump-object e T)
  (princ)
  (setvar "logfilemode" 0)
  (if (setq fopen (open file "r"))
    (progn
      (while (setq line (read-line fopen))
	(if (> (strlen line) 0)
	  (setq logf (cons line logf))
	)
	;; if
      )
      ;; while
      (close fopen)
    )
    ;; progn
  )
  ;; if
  (if (setq rest (member "; Property values:" logf))
    (setq num (+ (- (length logf) (length rest)) 2))
  )
  ;; if
  (setq itm 0)
  (while (< itm num)
    (setq str  (nth itm logf)
	  dump (cons str dump)
	  itm  (1+ itm)
    )
    ;; setq
  )
  ;; while
  (princ)
  DUMP
)
;; LOG2lst


(defun elist1 (e / d d1 i)
  (cond
    ((or (and (eq 'ENAME (type e)) (setq d (entget e)))
	 (and (listp e) (setq d e) (setq e (cdr (assoc -1 d))))
     )
     (princ "(\n" des)
     (foreach pair d
       (princ " " des)
       (prin1 pair des)
       (princ "\n" des)
     )
     (princ ")\n\n\n" des)


     (princ
       "\n\n------------------------ Nested Object: ----------------------------\n\n"
       des
     )
     (if (= 1 (cdr (assoc 66 d)))
       (while
	 (progn
	   (elist (setq e (entnext e)))
	   (not (eq "SEQEND" (cdr (assoc 0 (entget e)))))
	 )
       )
     )
    )
  )
  (princ)
)

(defun c:EE-EE (/ des)
  (setq des (open "elist1.txt" "w"))
  (repeat 10 (princ "\n"))
  (elist1 (car (entsel)))
  (close des)
  (textpage)
  (startapp "Notepad" "elist1.txt")
)
