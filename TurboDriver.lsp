(defun c:TD
  ( / 
    *error*
    oldmutt
    n td
    sset ename
    attname attlist tagname 
    lead leads llist runs
    watts tlen
    rmname
    htmlfile
    dlabel
  )

; error function
(defun *error* ( msg )
  (if oldmutt (setvar "NOMUTT" oldmutt))

  (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
  )
  (princ)
); end defun *error*

; save and update system variables
(setq oldmutt (getvar "NOMUTT"))

(if (not pwr)
    (setq pwr 5.5))
;(print (strcat "Current Watts/Foot : " (rtos pwr 2 1)))
(initget "Watts Select")
(setq td (getkword (strcat "\nSet Watts/Foot " "<" (rtos pwr 2 1) "> or Select Turbo Tape(s) [Watts/Select] <Select> : ")))

(if (= td "Watts")
    (progn
      (setq pwr (getreal (strcat "\nCurrent Watts/Foot: <" (rtos pwr 2 1) "> | " "Enter new Watts/Foot: ")))
      (if (not pwr)
          (setq pwr 5.5)
      ); end if
    ); end progn
); end if

;;;;;;;;;; SELECT TURBO TAPE ;;;;;;;;;;

(prompt "\nSelect Turbo Tape(s): ")
(setvar "nomutt" 1)
(setq sset (ssget))
(setvar "nomutt" 0)

(setq n 0)
(repeat (sslength sset)
        (if
          (not (>= (if (= (cdr (assoc 0 (entget (ssname sset n)))) "INSERT") (vl-string-search "in" (vla-get-effectivename (vlax-ename->vla-object (ssname sset n))))) 7))
          (progn
            (alert "Select official Turbo Tape™ blocks only!")
            (exit)
          ); end progn
        )
        (setq n (1+ n))
); end repeat

(setq n 0)
(repeat (sslength sset)

  (setq ename (tblobjname "block" (vl-princ-to-string (vla-get-effectivename (vlax-ename->vla-object (ssname sset n))))))

  (while
    (setq ename (entnext ename))
      (if (= (cdr (assoc 40 (entget ename))) 4.5)
          (progn
            (setq attname (cdr (assoc 1 (entget ename))))
            (setq attname
              (*
                (/ (+ (* (atoi (substr attname (+ (vl-string-search "-" attname) 2) (- (- (vl-string-search "'" attname) 1) 2))) 12)
                      (atoi (vl-string-trim "\"" (substr attname (+ (vl-string-search "'" attname) 3))))) 12.0 )
                (atoi
                  (if (not (vl-string-search "(" (cdr (assoc 1 (entget ename)))))
                      "1"
                      (vl-string-trim ")"
                        (substr (cdr (assoc 1 (entget ename)))
                                (+ (vl-string-search "(" (cdr (assoc 1 (entget ename)))) 3))
                      )
                  )
                )
              )
            );end setq
            (setq attlist (cons attname attlist))
          );end progn
      )
      (if (= (cdr (assoc 40 (entget ename))) 3.0)
          (setq tagname (strcase (cdr (assoc 1 (entget ename))) T)))
  ); end while

  (setq n (1+ n))

); end repeat

(if (not tagname)
    (setq tagname "*tagname*"))

(setq tlen (apply '+ attlist))
(setq tlen
  (strcat
    (rtos (fix (- tlen (- tlen (fix tlen)))) 2 0)
    "'-"
    (rtos (* (- tlen (fix tlen)) 12) 2 0)
    "\""
  )
)

(setq attlist (mapcar '(lambda (tt) (* tt pwr)) attlist))

(setq watts (apply '+ attlist))

; create list 'runs' of all required driver feeds for given attlist
(while attlist

  (if (= (length attlist) 1)
    (progn ; [start attlist length = 1]
      (if (> (car attlist) 95)
          (progn
            (setq attlist (append (list (/ (car attlist) 2))(list (/ (car attlist) 2)) (cdr attlist)))
          ); end progn
          (progn
            (setq runs (append (reverse runs) attlist))
            (setq attlist nil)
          ); end progn
      ); end if
    ); [end attlist length = 1]
    (progn ; [start attlist length /= 1]
      (if (> (+ (car attlist)(cadr attlist)) 95)
          (progn
            (if (> (car attlist) 95)
              (progn
                (setq attlist (append (list (/ (car attlist) 2))(list (/ (car attlist) 2)) (cdr attlist)))
              ); end progn
              (progn
                (setq runs (cons (car attlist) runs))
                (setq attlist (cdr attlist))
                (cond
                  ((= (length attlist) 1)
                      (if (> (car attlist) 95)
                          (progn
                            (setq attlist (append (list (/ (car attlist) 2))(list (/ (car attlist) 2)) (cdr attlist)))
                          ); end progn
                          (progn
                            (setq runs (append (reverse runs) attlist))
                            (setq attlist nil)
                          ); end progn
                      ); end if
                  ); end condition
                  (t nil) 
                ); end cond
              ); end progn
            );end if
          );end progn
          (progn
            (setq attlist (cons (+ (car attlist)(cadr attlist)) (cdr (member (cadr attlist) attlist))))
          );end progn
      ); end if
    ); [end attlist length /= 1]
  ); end if

); end while

;;;;;;;;;; SELECT LEADERS ;;;;;;;;;;

;; ADD FUNCTION TO ALSO SELECT DRIVER BLOCK ;;
(prompt "\nSelect Leader(s): ")
(setvar "nomutt" 1)
(setq sset (ssget))
(setvar "nomutt" 0)

(setq n 0)
(repeat (sslength sset)
        (if 
          (not (= (if (= (cdr (assoc 0 (entget (ssname sset n)))) "INSERT") (vla-get-effectivename (vlax-ename->vla-object (ssname sset n)))) "CDL-Leader" ))
          (progn
            (alert "Select official CDL Leader™ blocks only!")
            (exit)
          ); end progn
        ); end if
        (setq n (1+ n))
); end repeat

(setq n 0)
(repeat (sslength sset)

    (setq ename (ssname sset n))
    (setq lead  (vl-string-trim "'"
                (vl-string-trim "TO " 
                  (vl-some 
                    '(lambda ( att ) 
                      (if (= (strcase "TOV#") (strcase (vla-get-tagstring att))) 
                          (vla-get-textstring att)
                      )) 
                    (vlax-invoke (vlax-ename->vla-object ename) 'getattributes))
                )))
    (if (= lead "V#")
      (progn
        (setq lead  (vl-string-trim "'"
                    (vl-string-trim "TO " 
                      (vl-some 
                        '(lambda ( att )
                          (if (= (strcase "TOV##") (strcase (vla-get-tagstring att))) 
                              (vla-get-textstring att)
                          )) 
                        (vlax-invoke (vlax-ename->vla-object ename) 'getattributes))
                    )))
      ); end progn
    ); end if

  (setq n (1+ n))

  (setq leads (cons lead leads))

); end repeat

(while leads
    (setq llist (cons (car leads) llist))
    (setq leads (vl-remove (car leads) (cdr leads)))
); end while
(setq llist (vl-sort (reverse llist) '<))

(if (> (vl-list-length llist) 1)
    (setq llist (strcat (car llist) "-" (substr (car (reverse llist)) (strlen (car (reverse llist))))))
    (setq llist (strcat (car llist)))
)

;;;;;;;;;; SELECT ROOM NAME ;;;;;;;;;;

(setvar "ERRNO" 0)
(while
  (AND (not rmname) (/= (getvar "ERRNO") 52))
    (if (AND 
          (setq rmname (car(entsel "\nSelect Room Name: ")))
          (not (= (if (= (cdr (assoc 0 (entget rmname))) "MTEXT") (cdr (assoc 40 (entget rmname)))) 4.5 ))
        ); end AND
        (setq rmname nil)
    ); end if
); end while
(setq rmname (substr (cdr (assoc 1 (entget rmname ))) 1 (vl-string-search "\\" (cdr (assoc 1 (entget rmname))))))

;;;;;;;;;; CALCS ;;;;;;;;;;

(cond
  ((<= (length runs) 3)
      (cond
        ( (< watts 29)
          (setq dlabel (strcat "[" llist "]" " 30w/24vDC - Enviro #DMU-24-30 - " rmname " " tagname " (" tlen " / " (itoa (1+ (fix watts))) "w)"))
        )
        ( (AND (>= watts 29) (< watts 59))
          (setq dlabel (strcat "[" llist "]" " 60w/24vDC - Enviro #DMU-24-60 - " rmname " " tagname " (" tlen " / " (itoa (1+ (fix watts))) "w)"))
        )
        ( (AND (>= watts 59) (< watts 95))
          (setq dlabel (strcat "[" llist "]" " 96w/24vDC - Enviro #DMU-24-96 - " rmname " " tagname " (" tlen " / " (itoa (1+ (fix watts))) "w)"))
        )
        ( (AND (>= watts 95) (< watts 191))
          (if (= (length runs) 3)
              (setq dlabel (strcat "[" llist "]" " 288w/24vDC - Enviro #DMU-24-3x96 - " rmname " " tagname " (" tlen " / " (itoa (1+ (fix watts))) "w)"))
              (setq dlabel (strcat "[" llist "]" " 192w/24vDC - Enviro #DMU-24-2x96 - " rmname " " tagname " (" tlen " / " (itoa (1+ (fix watts))) "w)"))
          )
        )
        ( (AND (>= watts 191) (< watts 287))
          (setq dlabel (strcat "[" llist "]" " 288w/24vDC - Enviro #DMU-24-3x96 - " rmname " " tagname " (" tlen " / " (itoa (1+ (fix watts))) "w)"))
        )
        (t nil)
      );end cond
  ); end condition

  ((> (length runs) 3)
    (if (zerop (rem (length runs) 2))
        (progn
          (if (zerop (rem (length runs) 3))
              (setq dlabel (strcat "[" llist "]" " (x" (rtos (/ (length runs) 3.0) 2 0) ") " "288w/24vDC - Enviro #DMU-24-3x96 - " rmname " " tagname " (" tlen " / " (itoa (1+ (fix watts))) "w)"))
              (setq dlabel (strcat "[" llist "]" " (x" (rtos (/ (length runs) 2.0) 2 0) ") " "192w/24vDC - Enviro #DMU-24-2x96 - " rmname " " tagname " (" tlen " / " (itoa (1+ (fix watts))) "w)"))
          )
        ); end progn
        (progn
          (setq dlabel (strcat "[" llist "]" " (x" (rtos (1+ (fix (/ (length runs) 3.0))) 2 0) ") " "288w/24vDC - Enviro #DMU-24-3x96 - " rmname " " tagname " (" tlen " / " (itoa (1+ (fix watts))) "w)"))
        ); end progn
    ); end if
  ); end condition

); end cond

(vlax-invoke
  (vlax-get (vlax-get (setq htmlfile (vlax-create-object "htmlfile")) 'ParentWindow) 'ClipBoardData)
  'SetData "Text" dlabel)
(vlax-release-object htmlfile)

(terpri)

(if oldmutt (setvar "NOMUTT" oldmutt))
  
(print dlabel)
(princ)

); end defun