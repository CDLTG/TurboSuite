(defun c:TN
  ( /
    *error*
    xname rmname
    xnum rmnum
    xheight rmheight
    rmlabel
  )

; error function
(defun *error* ( msg )

  (if xname (vla-put-color (vlax-ename->vla-object xname) AcByLayer))
  (if xnum (vla-put-color (vlax-ename->vla-object xnum) AcByLayer))
  (if xheight (vla-put-color (vlax-ename->vla-object xheight) AcByLayer))
  (vla-regen (vla-get-activedocument (vlax-get-acad-object)) 0)

  (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
  )
  (princ)
); end defun *error*

; load visual lisp extensions
(vl-load-com) 

; load text layer if not found
(if (not (tblsearch "layer" "CDL-A-LTG-TEXT" ))
    (entmake '( (0 . "LAYER")
                (100 . "AcDbSymbolTableRecord")
                (100 . "AcDbLayerTableRecord")
                (2 . "CDL-A-LTG-TEXT")
                (70 . 0) )
    ); end entmake
); end if

; load text style if not found
(if (not (tblsearch "style" "CDL4.5-BOLD"))
    (entmake '((0 . "STYLE")
                (100 . "AcDbSymbolTableRecord")
                (100 . "AcDbTextStyleTableRecord")
                (2 . "CDL4.5-BOLD")
                (3 . "FUTURAM.TTF")
                (70 . 0)
                (40 . 4.5)
                (41 . 1.0)
                (50 . 0.0)
                (71 . 0))
    )); end entmake / if

; select room name and filter carriage returns and text formatting
(setvar "ERRNO" 0)
(while (AND (not xname) (/= (getvar "ERRNO") 52))
          (if (AND 
                (setq xname (car(nentselp "\nSelect Room Name: ")))
                (not (vl-string-search "Text" (apply 'strcat (mapcar 'cdr (vl-remove-if '(lambda (x) (/= 100 (car x))) (entget xname))))))
              )
              (setq xname nil)
          ); end if
); end while

(if (not xname)
    (setq xname nil)
    (progn
      (setq rmname (strcase (cdr (assoc 1 (entget xname)))))
        (while  (vl-string-search "\\P" rmname)
                (setq rmname (vl-string-subst " " "\\P" rmname)) )
        (while  (vl-string-search "%%U" rmname)
                (setq rmname (vl-string-subst "" "%%U" rmname)) )
        (while  (vl-string-search "\\L" rmname)
                (setq rmname (vl-string-subst "" "\\L" rmname)) )
        (while  (vl-string-search ";" rmname)
                (setq rmname (substr rmname (+ (vl-string-search ";" rmname) 2))) )
        (setq rmname  (vl-string-subst "CLOSET" "CLO." rmname)
              rmname  (vl-string-subst "STORAGE" "STOR." rmname)
              rmname  (vl-string-subst "POWDER" "PWDR" rmname)
              rmname  (vl-string-subst "MECH" "MECH." rmname)
              rmname  (vl-string-subst "W.C." "WC" rmname)
              rmname  (vl-string-subst "W.C." "W/C" rmname)
              rmname  (vl-string-subst "RESTROOM" "RR" rmname)
              rmname  (vl-string-subst "A.V." "A/V" rmname)
              rmname  (vl-string-subst "VESTIBULE" "VEST." rmname)
              rmname  (vl-string-subst "COVERED" "COV." rmname)
              rmname  (vl-string-subst "BED" "BEDROOM" rmname)
              rmname  (vl-string-subst "" "#" rmname)
        ); end setq
      (vla-put-color (vlax-ename->vla-object xname) 70)
      (vla-regen (vla-get-activedocument (vlax-get-acad-object)) 0)
      (print (strcat "Room Name : " rmname ))(princ)
    ); end progn
); end if

; select room ceiling height and remove spaces
(setvar "ERRNO" 0)
(while (AND (not xheight) (/= (getvar "ERRNO") 52))
          (if (AND
                (setq xheight (car(nentselp "\nSelect Ceiling Height: ")))
                (not (vl-string-search "Text" (apply 'strcat (mapcar 'cdr (vl-remove-if '(lambda (x) (/= 100 (car x))) (entget xheight))))))
              )
              (setq xheight nil)
          ); end if
); end while

(if (not xheight)
    (setq xheight nil)
    (progn
      (setq rmheight (strcase (cdr (assoc 1 (entget xheight)))))  
        (setq rmheight (vl-string-subst "" "CLG" rmheight))
        (while  (vl-string-search " " rmheight)
                (setq rmheight (vl-string-subst "" " " rmheight)) )
        (while  (vl-string-search "\\L" rmheight)
                (setq rmheight (vl-string-subst "" "\\L" rmheight)) )
        (while  (vl-string-search "." rmheight)
                (setq rmheight (vl-string-subst "" "." rmheight)) )
        (setq rmheight  (vl-string-subst "" "-0\"" rmheight)
        ); end setq
      (vla-put-color (vlax-ename->vla-object xheight) 70)
      (vla-regen (vla-get-activedocument (vlax-get-acad-object)) 0)
      (print (strcat "Ceiling Height : " rmheight ))(princ)
    ); end progn
); end if

; select room number
(setvar "ERRNO" 0)
(while (AND (not xnum) (/= (getvar "ERRNO") 52))
          (if (AND
                (setq xnum (car(nentselp "\nSelect Room Number: ")))
                (not (vl-string-search "Text" (apply 'strcat (mapcar 'cdr (vl-remove-if '(lambda (x) (/= 100 (car x))) (entget xnum))))))
              )
              (setq xnum nil)
          ); end if
); end while

(if (not xnum)
    (setq rmnum nil)
    (progn
      (setq rmnum (strcase (cdr (assoc 1 (entget xnum)))))
        (while  (vl-string-search "\\P" rmnum)
                (setq rmnum (vl-string-subst " " "\\P" rmnum)) )
        (while  (vl-string-search "%%U" rmnum)
                (setq rmnum (vl-string-subst "" "%%U" rmnum)) )
        (while  (vl-string-search "\\L" rmnum)
                (setq rmnum (vl-string-subst "" "\\L" rmnum)) )
        (while  (vl-string-search ";" rmnum)
                (setq rmnum (substr rmnum (+ (vl-string-search ";" rmnum) 2))) )
      (vla-put-color (vlax-ename->vla-object xnum) 40)
      (print (strcat "Room Number : " rmnum ))(princ)
    ); end progn
); end if

(if xname (vla-put-color (vlax-ename->vla-object xname) 40))
(if xheight (vla-put-color (vlax-ename->vla-object xheight) 40))
(vla-regen (vla-get-activedocument (vlax-get-acad-object)) 0)

; concatenate room label with carriage returns
(setq rmlabel (strcat 
                (if rmname (if (AND (not rmnum)(not rmheight)) rmname (strcat rmname "\\P")) "")
                (if rmnum rmnum "")
                (if rmheight (if rmnum (strcat "\\P" rmheight) rmheight) "")
              ); end strcat
); end setq

; make mtext
(entmake  (list (cons 0 "MTEXT")
                (cons 100 "AcDbEntity")
                (cons 67 0)
                (cons 410 "Model")
                (cons 8 "CDL-A-LTG-TEXT")
                (cons 100 "AcDbMText")
                (cons 10 (getpoint "\nSelect Insertion Point: "))
                (cons 40 4.5)
                (cons 46 0.0)
                (cons 71 5)
                (cons 72 5)
                (cons 1 rmlabel)
                (cons 7 "CDL4.5")
          ); end list
); end entmake

(if xname (vla-put-color (vlax-ename->vla-object xname) AcByLayer))
(if xnum (vla-put-color (vlax-ename->vla-object xnum) AcByLayer))
(if xheight (vla-put-color (vlax-ename->vla-object xheight) AcByLayer))
(vla-regen (vla-get-activedocument (vlax-get-acad-object)) 0)

;set mtext width to non-0
(setpropertyvalue (entlast) "Width" 35)
;set mtext columns to dynamic
(setpropertyvalue (entlast) "ColumnType" 2)
;set mtext column width to 35
(setpropertyvalue (entlast) "ColumnWidth" 35)

; enter text edit
(command "_MTEDIT" "L")

(princ)

); end defun