;;;; 830730 Rendina Davide


;;;; -*- Mode: Lisp -*-


;;;; lmc.lisp --


;;;; Program that simulates a Little Man Computer.




;;; assembly instructions parameter

;;; Standard assembly instruction set for LMC.

(defparameter +assembly-instructions+
  '((add 100 xx)
    (sub 200 xx)
    (sta 300 xx)
    (lda 500 xx)
    (bra 600 xx)
    (brz 700 xx)
    (brp 800 xx)
    (inp 901 void) 
    (out 902 void)
    (hlt 0 void)
    (dat 0 void)
    (dat 0 xx)))


;;; one instruction 

;;; Carry out a single instruction on a state.%%%

(defun one-instruction (state)
  (let* ((state-parameters (rest state))
         (pc (getf state-parameters :pc))
         (mem (getf state-parameters :mem))
         (instruction (nth pc mem))
         (instruction-type (quotient instruction 100)))
    (cond
     ((not (eql (first state) 'state)) NIL)
     
     ((eql instruction-type 1) (add-instruction state-parameters))

     ((eql instruction-type 2) (sub-instruction state-parameters))

     ((eql instruction-type 3) (store-instruction state-parameters))

     ((eql instruction-type 5) (load-instruction state-parameters))

     ((eql instruction-type 6) (branch-instruction state-parameters
                                                   :branch-type 'unconditional))

     ((eql instruction-type 7) (branch-instruction state-parameters
                                                   :branch-type 'zero))

     ((eql instruction-type 8) (branch-instruction state-parameters
                                                   :branch-type 'positive))    

     ((eql instruction 901) (input-instruction state-parameters))              

     ((eql instruction 902) (output-instruction state-parameters))

     ((eql instruction-type 0) (halt-instruction state-parameters))        

     (T NIL))))



;;; x-instructions

;;; These instructions perform the various instruction of the LMC.
;;; They get as input a state, passed without the first element.
;;; It returns the result state with "state" or "halted-state" as first element. 

(defun add-instruction (state-parameters)
  (let* ((acc (getf state-parameters :acc))
         (pc (getf state-parameters :pc))
         (mem (getf state-parameters :mem))
         (memory-value (nth (mod (nth pc mem) 100)  mem)))
    (cons 'state
          (progn 
            (set-parameter state-parameters 
                           :pc (increment-pc (getf state-parameters :pc)))
            (set-parameter state-parameters 
                           :acc (mod (+ acc memory-value) 
                                     1000))
            (set-parameter state-parameters 
                           :flag (set-flag (+ acc memory-value)))))))

(defun sub-instruction (state-parameters)
  (let* ((acc (getf state-parameters :acc))
         (pc (getf state-parameters :pc))
         (mem (getf state-parameters :mem))
         (memory-value (nth (mod (nth pc mem) 100)  mem)))
    (cons 'state
          (progn 
            (set-parameter state-parameters 
                           :pc (increment-pc pc))
            (set-parameter state-parameters 
                           :acc (mod (- acc memory-value) 
                                     1000))
            (set-parameter state-parameters 
                           :flag (set-flag (- acc memory-value)))))))

(defun store-instruction (state-parameters)
  (let* ((acc (getf state-parameters :acc))
         (pc (getf state-parameters :pc))
         (mem (getf state-parameters :mem))
         (memory-position (mod (nth pc mem) 100)))
    (cons 'state
          (progn
            (set-parameter state-parameters 
                           :pc (increment-pc pc))
            (set-parameter state-parameters 
                           :acc (setf (nth memory-position mem) acc))))))

(defun load-instruction (state-parameters)
  (let* ((pc (getf state-parameters :pc))
         (mem (getf state-parameters :mem))
         (memory-value (nth (mod (nth pc mem) 100) mem)))
    (cons 'state
          (progn 
            (set-parameter state-parameters 
                           :pc (increment-pc pc))
            (set-parameter state-parameters 
                           :acc memory-value)))))

(defun branch-instruction (state-parameters &key branch-type)
  (let* ((instruction (nth (getf state-parameters :pc)
                           (getf state-parameters :mem))))
    (when (or (eql branch-type 'unconditional)
              (eql branch-type 'positive)
              (eql branch-type 'zero))
      (cons 'state
            (set-parameter state-parameters 
                           :pc (get-pc-branch state-parameters
                                              (mod instruction 100)
                                              :branch-type branch-type))))))

(defun input-instruction (state-parameters)
  (if (null (getf state-parameters :in)) 
      NIL
      (cons 'state
            (progn 
              (set-parameter state-parameters 
                             :acc (first (getf state-parameters :in)))
              (set-parameter state-parameters 
                             :pc (increment-pc (getf state-parameters :pc)))
              (set-parameter state-parameters 
                             :in (rest (getf state-parameters :in)))))))

(defun output-instruction (state-parameters)
    (cons 'state
          (progn
            (set-parameter state-parameters 
                           :pc (increment-pc (getf state-parameters :pc)))
            (set-parameter state-parameters 
                           :out (append (getf state-parameters :out)
                                        (list (getf state-parameters :acc)))))))

(defun halt-instruction (state-parameters)
  (cons 'halted-state
        state-parameters))



;;; execution loop

;;; Simulate the execution of the LMC.

(defun execution-loop (state)
  (cond 
   ((eql state NIL) NIL)
   ((eql (first state) 'halted-state) (getf (rest state) :out))
   (T (execution-loop (one-instruction state)))))



;;; set parameter

;;; Change the value of a state parameter.

(defun set-parameter (state parameter-name new-value)
  (setf (getf state parameter-name) new-value)
  state)



;;; get-pc-branch

;;; Calculate the value of pc for branch instructions.

(defun get-pc-branch (state new-value &key branch-type)
  (let ((pc (getf state :pc)))
    (cond 
     ((eql branch-type 'unconditional) new-value)
     ((eql branch-type 'zero) (if (and (eql (getf state :acc) 0)
                                       (eql (getf state :flag) 'noflag)) 
                                  new-value
                                  (increment-pc pc)))
     ((eql branch-type 'positive) (if (eql (getf state :flag) 'noflag)
                                      new-value
                                      (increment-pc pc))))))



;;; quotient

;;; Get the quotient of the division.

(defun quotient (dividend divisor) 
  (/ (- dividend 
        (mod dividend divisor)) 
     divisor))



;;; set-flag 

;;; Set the flag value, according to the result of the instruction (add or sub).

(defun set-flag (instruction-result) 
  (cond 
   ((betweenp instruction-result 0 999) 'noflag)
   (T 'flag)))



;;; increment-pc

;;; Increment the value of program counter.

(defun increment-pc (pc)
  (mod (+ pc 1)
       100))



;;; read from file

;;; Open the stream and start reading process.

(defun read-from-file (filename)
  (let ((input-stream (open filename
                            :direction :input
                            :if-does-not-exist NIL)))
    (if (eql input-stream NIL)
        'fail
        (let ((input-from-file (read-from-stream input-stream)))
          (if (member 'fail input-from-file)
              'fail
               input-from-file)))))



;;; read from stream

;;; Read from the stream and create a list of instruction.
;;; Remove the comments from the string readed.

(defun read-from-stream (stream)
  (let ((input-string (read-line stream NIL 'eof T)))
    (unless (eq input-string 'eof)
      (let ((string-no-comment (delete-comment input-string)))
        (if (eql string-no-comment 'fail)
            '(fail)
            (let ((final-string (string-to-list string-no-comment)))
              (cond
               ((null final-string)(read-from-stream stream))
               (T (cons final-string
                        (read-from-stream stream))))))))))



;;; string to list

;;; Convert a string to a list, removing spaces 

(defun string-to-list (str)
  (if (not (stringp str))
      NIL 
      (when (string/=  str "")
	(multiple-value-bind (value num-chars) (read-from-string str NIL)
          (when value
            (cons value
                  (string-to-list (subseq str num-chars))))))))



;;; delete comment

;;; Delete comment from a string.
;;; Return symbol fail if the comment is not well-formed.

(defun delete-comment (string)
  (let ((comment-position (search "//" string))
        (wrong-comment-position (search "/" string))) 
    (cond 
     ((and (null comment-position)
           (null wrong-comment-position)) string)
     ((null comment-position) 'fail)
     ((< wrong-comment-position 
         comment-position) 'fail)
     (T (subseq string 0 comment-position)))))



;;; format multiple instruction

;;; Get a list of instruction and format it.

(defun format-multiple-instruction (instruction-list)
  (let ((instruction (format-single-instruction (first instruction-list))))
    (if (null instruction-list) 
        instruction-list
        (if (null instruction) 
            '(fail)
            (cons instruction
                  (format-multiple-instruction (rest instruction-list)))))))



;;; format single instruction

;;; Format a single instruction and return the instruction formatted.
;;; Instruction structure: (label-name instruction-name value)

(defun format-single-instruction (instruction)
  (let ((instruction-length (length instruction)))
    (cond 
     ((eql instruction-length 1) (when (instruction-p (first instruction)) 
                                   (list NIL
                                         (first instruction)
                                         0)))
     ((eql instruction-length 2) (if (instruction-xx-p (first instruction))
                                     (list NIL 
                                           (first instruction) 
                                           (second instruction))
                                     (list (first instruction) 
                                           (second instruction) 
                                           0)))
     ((eql instruction-length 3) instruction)
     (T NIL))))



;;; get instructions by type

;;; Get a list of all assembly instructions that matches instruction-type.

(defun get-instructions-by-type (instruction-type)
  (mapcar (lambda (instruction) (when (eql (third instruction)
                                           instruction-type) 
                                  (first instruction))) 
          +assembly-instructions+))



;;; is instruction xx p

;;; Return T if instruction-name is an instruction with type xx.

(defun instruction-xx-p (instruction-name)
  (when
      (member instruction-name
              (get-instructions-by-type 'xx))
    T))



;;; is instruction p

;;; Return T if instruction-name is an instruction with type void.

(defun instruction-p (instruction-name)
  (when
      (member instruction-name
              (get-instructions-by-type 'void))
    T))



;;; load-multiple-labels

;;; Create a list of labels retrieved from instructions-formatted list.
;;; Return fail symbols if labels are not well-formed.

(defun load-multiple-labels (instructions-formatted)
  (let ((label-list (get-labels instructions-formatted)))
    (if (valid-label-list-p (mapcar 'car label-list))
        label-list
        'fail)))



;;; get labels from instructions

;;; Create the list of labels present in instructions-formatted list.

(defun get-labels (instructions-formatted &optional (label-value 0))
  (let ((label-name (first (first instructions-formatted))))
    (unless (null instructions-formatted)
      (if (null label-name)
          (get-labels (rest instructions-formatted)
                      (+ label-value 1))
          (cons (list label-name 
                      label-value)
                (get-labels (rest instructions-formatted)
                            (+ label-value 1)))))))

 
 
;;; valid label list p

;;; Return T if the label-list passed id valid. Otherwise, returns NIL.

(defun valid-label-list-p (label-list)
  (let ((label (first label-list)))
    (if (null label) 
        T
        (when (valid-label-p label 
                             (rest label-list))
          (valid-label-list-p (rest label-list))))))



;;; valid label p

;;; True if the label name passed is valid.

(defun valid-label-p (label-name label-list)
  (if (null label-name) 
      T
      (when (and (legal-label-p label-name)
                 (null (member label-name 
                               label-list)))
        T)))



;;; legal-label-p

;;; True if label name start with an alpha character

(defun legal-label-p (label-name)
  (unless (numberp label-name)
    (alpha-char-p (char (string label-name) 0))))



;;; load multiple instruction

;;; Generate the memory according to the parameter passed.
;;; If the function fails, the list returned contains the symbol fail.

(defun load-multiple-instruction (instructions-formatted label-list)
  (let ((instruction (load-single-instruction (first instructions-formatted ) 
                                              label-list)))
    (unless (null instructions-formatted)
      (cons instruction
            (load-multiple-instruction (rest instructions-formatted)
                                       label-list)))))



;;; load-single-instruction

;;; Generate the machine instruction of instruction and return it.

(defun load-single-instruction (instruction label-list)
  (let 
      ((instruction-value (get-instruction-value (second instruction)))
       (mem-position (get-memory-position instruction label-list)))
    (cond 
     ((null instruction-value) 'fail)
     ((null mem-position) 'fail)
     (T (+ instruction-value 
           mem-position)))))



;;; get-instruction-value

;;; Retrieve value of the assembly instruction.

(defun get-instruction-value (instruction-name)
  (second (search-deep instruction-name +assembly-instructions+)))



;;; get-memory-position

;;; Get the memory position (from number or from label) of an instruction.

(defun get-memory-position (instruction label-list)
  (let ((position (third instruction)))
    (cond 
     ((eql (second instruction) 'dat) (when (and (numberp position)
                                                 (betweenp position 0 999)) 
                                        position))
     ((numberp position) (when (betweenp position 0 99) 
                           position))
     (T (retrieve-label-value position
                              label-list)))))



;;; retrieve label value

;;; Search the corrispond value of a label-name in a list of labels.

(defun retrieve-label-value (label-name label-list)
  (second (search-deep label-name
                       label-list)))



;;; search deep

;;; Search in a list of sublist.
;;; Returns the sub-list if value is equal to its first element. 

(defun search-deep (value list)
  (let ((element (first list)))
    (cond 
     ((null element) NIL)
     ((eql value 
           (first element)) element)
     (T (search-deep value 
                     (rest list))))))



;;; between p

;;; True if value is in the specified range.

(defun betweenp (value start-range-value  end-range-value)
  (and (<= value end-range-value)
       (>= value start-range-value)))



;;; lmc load

;;; Load the lmc memory according to an assembly file (fname).

(defun lmc-load (filename)
  (let* ((readed-stream (read-from-file filename))
        (instr-formatted (unless (eql readed-stream 'fail)
                           (format-multiple-instruction readed-stream))))
    (cond 
     ((eql readed-stream 'fail) NIL)
     ((null instr-formatted) (make-list 100 :initial-element 0))
     ((member 'fail instr-formatted) NIL) 
     (T (load-memory instr-formatted))))) 



;;; load memory 

;;; Calculate the memory starting from instruction list.

(defun load-memory (instructions-formatted)
  (let ((labels (load-multiple-labels instructions-formatted)))
    (unless (eql labels 'fail)
      (validate-memory (load-multiple-instruction instructions-formatted
                                                  labels)))))



;;; validate memory

;;; Return the final memory if there are no errors.

(defun validate-memory (memory)
  (cond
   ((null memory) NIL)
   ((member 'fail memory) NIL)
   (T (fill-memory memory))))



;;; fill memory

;;; Fill the memory adding some 0s in tail of the memory.

(defun fill-memory (memory)
  (if (> (length memory) 100) 
      NIL
      (append memory
              (make-list (- 100 (length memory))
                         :initial-element 0))))



;;; not valid input list p

;;; Test if the input list is valid.

(defun not-valid-input-list-p (input-list)
  (when (member NIL
                (mapcar (lambda (value) (betweenp value 0 999))
                        input-list))
    T))



;;; lmc-run

;;; Run the LMC.

(defun lmc-run (filename input-list)
  (let ((MEMORY (lmc-load filename)))
    (cond
     ((not-valid-input-list-p input-list) NIL)
     ((equal MEMORY NIL) NIL)
     (T (execution-loop (list 'state
                              :acc 0
                              :pc 0
                              :mem MEMORY
                              :in input-list
                              :out ()
                              :flag 'noflag))))))






;;;; end of file -- lisp.lmc --
