(sb-int:set-floating-point-modes  :traps '(:overflow  :invalid :divide-by-zero))
(defvar *save* nil)
;;; ================================================================
;;; 1. CONFIGURATION & GRAMMAR
;;; ================================================================
(defvar *integer-types* '(integer fixnum
                          (unsigned-byte #.sb-vm:n-word-bits)
                          (signed-byte #.sb-vm:n-word-bits)
                          (and fixnum unsigned-byte)
                          (integer * 0)
                          (integer * -1)
                          (integer 1)
                          unsigned-byte
                          (and fixnum (integer * 0))
                          (and fixnum (integer * -1))
                          (and fixnum (integer * -1))
                          (and fixnum (integer 1))))

(defparameter *types* (list* 'boolean *integer-types*))
(defvar *number-types*)
(defparameter *max-depth* 6)

(defvar *number-ops*
  '((+   (number number) number)
    (-   (number number) number)
    (-   (number) number)
    (*   (number number) number)
    (/   (number number) number)

    (truncate (number) integer)
    (ceiling (number) integer)
    (round (number) integer)

    (ffloor (number) number)
    (ftruncate (number) number)
    (fceiling (number) number)
    (fround (number) number)

    (max (number number) number)
    (min (number number) number)
    (abs (number) number)

    (sin (number) number)
    (sin (integer) number)
    (cos (number) number)
    (cos (integer) number)

    ;; (expt (number number) number)
    (exp (number) number)
    (log (number) number)
    (log (number number) number)

    ;; (expt (integer number) number)
    (exp (integer) number)
    (log (integer) number)
    (log (integer number) number)

    (+   (number integer) number)
    (-   (number integer) number)
    (*   (number integer) number)
    (/   (number integer) number)

    (>   (number number) boolean)
    (<   (number number) boolean)
    (=   (number number) boolean)
    (eql   (number number) boolean)
    (equal   (number number) boolean)
    (equalp   (number number) boolean)))

(defvar *float-ops*
  '((+   (single-float single-float) single-float)
    (-   (single-float single-float) single-float)
    (-   (single-float) single-float)
    (*   (single-float single-float) single-float)
    (/   (single-float single-float) single-float)
    (truncate (single-float single-float) integer)
    (ceiling (single-float single-float) integer)
    (round (single-float single-float) integer)

    (ffloor (single-float single-float) single-float)
    (ftruncate (single-float single-float) single-float)
    (fceiling (single-float single-float) single-float)
    (fround (single-float single-float) single-float)

    (truncate (single-float) integer)
    (ceiling (single-float) integer)
    (round (single-float) integer)

    (ffloor (single-float) single-float)
    (ftruncate (single-float) single-float)
    (fceiling (single-float) single-float)
    (fround (single-float) single-float)

    (max (single-float single-float) single-float)
    (min (single-float single-float) single-float)
    (abs (single-float) single-float)

    (sin (single-float) single-float)
    (sin (integer) single-float)
    (cos (single-float) single-float)
    (cos (integer) single-float)

    (expt (single-float single-float) number)
    (exp (single-float) number)
    (log (single-float) number)
    (log (single-float single-float) number)

    (expt (integer single-float) number)
    (exp (integer) number)
    (log (integer) number)
    (log (integer single-float) number)

    (+   (single-float integer) single-float)
    (-   (single-float integer) single-float)
    (*   (single-float integer) single-float)
    (/   (single-float integer) single-float)

    (>   (single-float single-float) boolean)
    (<   (single-float single-float) boolean)))

(defparameter *noise-ops*
  '((unwind-protect (t t) t)
    (unwind-protect (t) t)
    (catch (t t) t)
    (values (t) t)
    (values (t t) t)
    (values (t t t) t)
    (values (t t t t) t)
    (values (t t t t t) t)))

(defvar *ratio-ops*
  '((+   (rational rational) rational)
    (-   (rational rational) rational)
    (-   (rational) rational)
    (*   (rational rational) rational)
    (/   (rational rational) rational)

    (truncate (rational rational) integer)
    (ceiling (rational rational) integer)
    (round (rational rational) integer)

    (ffloor (rational rational) rational)
    (ftruncate (rational rational) rational)
    (fceiling (rational rational) rational)
    (fround (rational rational) rational)
    (ffloor (rational rational) rational)

    (truncate (rational) integer)
    (ceiling (rational) integer)
    (round (rational) integer)

    (ffloor (rational) rational)
    (ftruncate (rational) rational)
    (fceiling (rational) rational)
    (fround (rational) rational)
    (ffloor (rational) rational)

    (max (rational rational) rational)
    (min (rational rational) rational)
    (abs (rational) rational)

    (sin (rational) single-float)
    (sin (integer) single-float)
    (cos (rational) single-float)
    (cos (integer) single-float)

    ;; (expt (rational rational) number)
    (exp (rational) number)
    (log (rational) number)
    (log (rational rational) number)

    (exp (integer) number)
    (log (integer) number)
    (log (integer rational) number)

    (+   (rational integer) rational)
    (-   (rational integer) rational)
    (*   (rational integer) rational)
    (/   (rational integer) rational)

    (>   (rational rational) boolean)
    (<   (rational rational) boolean)))

(defparameter *operators*
  `(;; Integer
    (+   (integer integer)     integer)
    (-   (integer integer)     integer)
    (-   (integer)     integer)
    (*   (integer integer)     integer)
    (truncate (integer integer) integer)
    (floor (integer integer) integer)
    (ceiling (integer integer) integer)
    (round (integer integer) integer)

    (logand (integer integer) integer)
    (logxor (integer integer) integer)
    (logior (integer integer) integer)
    (lognot (integer) integer)
    (integer-length (integer) integer)
    (logcount (integer) integer)
    (max (integer integer) integer)
    (min (integer integer) integer)
    (abs (integer) integer)
    (evenp (integer) boolean)
    (oddp (integer) boolean)
    (ash (integer (integer -256 256)) integer)


    ;; Comparison
    (>   (integer integer)           boolean)
    (<   (integer integer)           boolean)
    (=   (integer integer)           boolean)


    ;; Logic
    (and (boolean boolean) boolean)
    (or  (boolean boolean) boolean)
    (not (boolean)         boolean)))

;;; ================================================================
;;; 2. GENERATOR
;;; ================================================================

(defun random-elt (seq)
  (if (null seq) nil (elt seq (random (length seq)))))


(defun get-ops (ret-type)
  (when (member ret-type *integer-types* :test #'eq)
    (setf ret-type 'integer))
  (remove-if-not (lambda (x) (eq (third x) ret-type)) *operators*))

(defun get-vars (ret-type schema)
  (mapcar #'car (remove-if-not (lambda (x) (eq (cdr x) ret-type)) schema)))

(defconstant max-integer (1- sb-vm:n-fixnum-bits))

(defun random-const (type)
  (case type
    (number
     (random-const (random-elt *number-types*)))
    (integer (* (if (< (random 100) 30)
                    (random 100)
                    (random (expt 2 max-integer)))
                (if (zerop (random 2)) 1 -1)))
    (unsigned-byte (if (< (random 100) 30)
                       (random 100)
                       (random (expt 2 max-integer))))
    (single-float (+ (random 50.0) 0.5))
    (boolean      (if (zerop (random 2)) nil t))
    (fixnum (* (random (if (< (random 100) 50)
                           (1+ (random 100))
                           (expt 2 #.(1- sb-vm:n-fixnum-bits))))
               (if (zerop (random 2)) 1 -1)))
    (rational
     (if (< (random 100) 50)
         (random-const 'integer)
         (* (/ (1+ (random (expt 2 max-integer)))
               (1+ (random (expt 2 max-integer))))
            (if (zerop (random 2)) 1 -1))))
    (t
     (cond ((equal type '(signed-byte #.sb-vm:n-word-bits))
            (* (if (< (random 100) 50)
                   (1+ (random 100))
                   (random (expt 2 #.(1- sb-vm:n-word-bits))))
               (if (zerop (random 2)) 1 -1)))
           ((equal type '(unsigned-byte #.sb-vm:n-word-bits))
            (random (if (< (random 100) 50)
                        (1+ (random 100))
                        (expt 2 #.sb-vm:n-word-bits))))
           ((equal type '(and fixnum unsigned-byte))
            (random (if (< (random 100) 50)
                        (1+ (random 100))
                        (expt 2 #.(1- sb-vm:n-fixnum-bits)))))
           ((equal type '(integer * 0))
            (- (if (< (random 100) 30)
                   (random 100)
                   (random (expt 2 max-integer)))))
           ((equal type '(integer * -1))
            (- -1 (if (< (random 100) 30)
                      (random 100)
                      (random (expt 2 max-integer)))))
           ((equal type '(and fixnum (integer * -1)))
            (- -1 (if (< (random 100) 30)
                      (random 100)
                      (random (expt 2 #.(1- sb-vm:n-fixnum-bits))))))
           ((equal type '(and fixnum (integer * 0)))
            (- (if (< (random 100) 30)
                   (random 100)
                   (random (expt 2 #.(1- sb-vm:n-fixnum-bits))))))
           ((equal type '(and fixnum (integer 1)))
            (1+ (if (< (random 100) 30)
                    (random 100)
                    (random (1- (expt 2 #.(1- sb-vm:n-fixnum-bits)))))))
           ((equal type '(integer 1))
            (1+ (if (< (random 100) 30)
                    (random 100)
                    (random (expt 2 max-integer)))))
           ((equal type '(integer -256 256))
            (* (random 257)
               (if (zerop (random 2)) 1 -1)))))))

(defvar *noise* nil)

(defun generate-ast (type depth schema)
  (let ((terminals (unless (or (eq type 'boolean)
                               (= depth 0))
                     '(const)))
        (vars (unless (= depth 0)
                (get-vars type schema)))
        (funcs (get-ops type)))
    (when vars (push 'var terminals))
    (let* ((stop (>= depth *max-depth*))
           (options (or (if stop terminals (append terminals '(func if)))
                        '(func))))
      (when (and *noise*
                 (not stop))
        (push 'noise options))
      (case (random-elt options)
        (const (random-const type))
        (var   (random-elt vars))
        (if    (let ((test (generate-ast 'boolean (1+ depth) schema))
                     (c (generate-ast type (1+ depth) schema))
                     (a (generate-ast type (1+ depth) schema)))
                 (cond ;; ((eq test nil)
                       ;;  a)
                       ;; ((eq test t)
                       ;;  c)
                       ;; ((eql c a) c)
                       (t
                        (list 'if test c a)))))
        (noise
         (let ((op (random-elt *noise-ops*)))
           (cons (first op)
                 (loop for arg-t in (second op)
                       collect (generate-ast (if (eq arg-t t)
                                                 type
                                                 arg-t) (1+ depth) schema)))))
        (func  (if (null funcs)
                   (random-const type)
                   (let ((op (random-elt funcs)))
                     (cons (first op)
                           (loop for arg-t in (second op)
                                 collect (generate-ast arg-t (1+ depth) schema))))))))))

(defun build-random-function (target-type)
  (let ((schema (loop for i from 1 to 3
                      collect (cons (intern (format nil "V~d" i))
                                    (random-elt *types*)))))
    (let ((body (generate-ast target-type 0 schema))
          (vars (mapcar #'car schema)))
      (values
       `(lambda ,vars
          (declare (ignorable ,@vars))
          ,@(progn;if (> (random 100) 90)
                (loop for (v . t-name) in schema
                      collect `(declare (type ,(if nil;(> (random 100) 60)
                                                   t
                                                   t-name) ,v))))
          (declare (optimize (safety 1) (speed 1)))
          ,body)
       schema))))

;;; ================================================================
;;; 3. EXECUTION & COMPARISON
;;; ================================================================

(defun values-match-p (val1 val2)
  (cond
    ;; ((and (typep val1 'single-float) (typep val2 'single-float))
    ;;  (< (abs (- val1 val2)) 0.001))
    (t (and (= (length val1) (length val2))
            (loop for v1 in val1
                  for v2 in val2
                  always (or (eql v1 v2)
                             #+(and arm64 (not darwin))
                             (or (and (floatp v1) (sb-ext:float-nan-p v1))
                                 (and (floatp v2)
                                      (sb-ext:float-nan-p v2)))
                             ;; MINUS-ZERO
                             ;; (and (or (eql v1 0.0)
                             ;;          (eql v2 0.0))
                             ;;      (= v1 v2))
                             #-(and arm64 (not darwin))
                             (and (and (floatp v1)
                                       (sb-ext:float-nan-p v1))
                                  (and (floatp v2)
                                       (sb-ext:float-nan-p v2)))))))))

(defun safe-execute (code func)
  "Returns (values result condition)"
  (handler-case
      (handler-bind ((sb-sys:memory-fault-error (lambda (c)
                                                  (with-standard-io-syntax
                                                    (princ code))
                                                  (break "~a" c))))
        (values (multiple-value-list (funcall func)) nil))
    (error (c) (values nil c))))

(defun save-test (code thread)
  (with-open-file (st (format nil "/tmp/test~a" thread)
                      :if-does-not-exist :create :if-exists :overwrite
                      :direction :output)
    (write code :stream st)))

(defun report-error (thread reason code inputs c-res i-res)
  (format t "~%!!! DETECTED DISCREPANCY !!!")
  (format t "~%Reason: ~A" reason)
  (format t "~%Code: ~S" code)
  (format t "~%Inputs: ~A" inputs)
  (format t "~%Compiled Result: ~S" c-res)
  (format t "~%Interpret Result: ~S" i-res)
  (format t "~%--------------------------------------------------")
  (save-test code thread)
  (error "~a" (format nil "/tmp/test~a" thread)))

(defun is-div-zero (err)
  (typep err '(or floating-point-invalid-operation
               floating-point-overflow
               division-by-zero)))

(defun run-test (thread)
  (let ((target (random-elt *types*)))
    (multiple-value-bind (code schema) (build-random-function target)
      (when *save*
        (save-test code thread))
      (let* ((fn (handler-bind (((or sb-ext:code-deletion-note sb-ext:compiler-note style-warning warning) #'muffle-warning))
                   (multiple-value-bind (fun warn fail) (compile nil code)
                     (declare (ignore warn fail))
                     ;; (when fail
                     ;;   (error "~a" code))
                     fun)))
             (type (caddr (sb-kernel:%simple-fun-type fn)))
             (types (when (typep type '(cons (eql values)))
                      (let ((ctype (sb-kernel:values-specifier-type type)))
                        (sb-kernel:values-type-required ctype)))))
        
        (loop repeat 2000
              do

              (let ((inputs (loop for (_ . t-name) in schema
                                  collect (random-const t-name))))
                (multiple-value-bind (c-val c-err) (safe-execute code
                                                                 (lambda ()
                                                                   (apply fn inputs)))
                  (unless (or c-err
                              (loop for type in types
                                    for value in c-val
                                    always (sb-kernel:%%typep value type)))
                    (report-error thread "TYPE MISMATCH" code inputs c-val type))
                  ;; 2. Run Interpreted
                  (multiple-value-bind (i-val i-err)
                      (safe-execute (cons 'i code)
                                    (lambda ()
                                      #+sbcl
                                      (let ((sb-ext:*evaluator-mode* :interpret))
                                        (apply (eval code) inputs))
                                      #-sbcl
                                      (apply (eval code) inputs)))

                    ;; 3. Compare (Filtering Logic)
                    (cond
                      ;; A. If either failed due to Div-By-Zero, Ignore completely.
                      ((or (is-div-zero c-err) (is-div-zero i-err))
                       nil)

                      ;; B. Both Succeeded: Check Values
                      ((and (not c-err) (not i-err))
                       (unless (values-match-p c-val i-val)
                         (report-error thread "VALUE MISMATCH" code inputs c-val i-val)))

                      ;; C. Both Errored (Non-DivZero): Check Error Types match
                      ((and c-err i-err)
                       (unless (or (eq (type-of c-err) (type-of i-err))
                                   (subtypep (type-of c-err) (type-of i-err))
                                   (subtypep (type-of i-err) (type-of c-err)))
                         (report-error thread "ERROR TYPE MISMATCH" code inputs c-err i-err)))

                      ;; D. One Error, One Success (Non-DivZero)
                      (t
                       (report-error thread "STATUS MISMATCH (One Error/One Value)"
                                     code inputs (or c-err c-val) (or i-err i-val))))))))))))

;;; ================================================================
;;; 4. MAIN LOOP
;;; ================================================================

(defun main (&key (threads 12) float depth rational number
                  noise
                  save)
  (setf *random-state* (make-random-state t))
  (when save
    (setf *save* t))
  (when noise
    (setf *noise* t))
  (when depth
    (setf *max-depth* depth))
  (when float
    (setf *operators* (append *operators* *float-ops*))
    (push 'single-float *types*))
  (when rational
    (setf *operators* (append *operators* *ratio-ops*))
    (push 'rational *types*))
  (when number
    (setf *operators* (append *operators* *number-ops*))
    (setf *number-types* (remove 'boolean *types*))
    (push 'single-float *types*)
    (push 'rational *types*)
    (push 'number *types*))
  (if (= threads 1)
      (loop
       (run-test 0))
      (let ((threads
              (loop for i below threads
                    collect
                    (sb-thread:make-thread
                     (let ((i i))
                       (lambda ()
                         (loop
                          (run-test i))))
                     :name "random"))))
        (unwind-protect (mapcar (lambda (th)
                                  (sb-thread:join-thread th :default nil)) threads)
          (mapcar (lambda (th)
                    (ignore-errors (sb-thread:terminate-thread th)))
                  threads)))))
