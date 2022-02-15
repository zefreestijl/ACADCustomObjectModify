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


(defun elist (e / d d1 i)
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

     (setq d1 (LOG2lst (vlax-ename->vla-object e)))
     (setq i 0)
     (repeat (length d1)
       (write-line (vl-string-translate ";" " " (nth i d1)) des)
       (setq i (1+ i))
     )

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

(defun c:DD-EE (/ des)
  (setq des (open "elist.txt" "w"))
  (repeat 10 (princ "\n"))
  (elist (car (entsel)))
  (close des)
  (textpage)
  (startapp "Notepad" "elist.txt")
)

 ;|
usage:
load this file
type AA-EE in command window
|;