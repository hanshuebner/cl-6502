(in-package :6502)

(defun decode-status (status)
  (coerce (loop with bit-names = "CZIDB-VN"
                for i below 8
                if (zerop (logand status (ash 1 i)))
                  collect #\.
                else
                  collect (aref bit-names i))
          'string))

(defun disassemble-instruction-at (address)
  (second (disasm-ins address
                      (lambda (bytes index name docs mode)
                        (declare (ignore index docs))
                        (let ((args (arg-formatter (rest bytes) mode)))
                          (if (emptyp args)
                              name
                              (format nil "~A ~A" name args)))))))

(defclass symbolic-cpu (6502:cpu)
  ((env :accessor cpu-env :initarg :env)
   (symbolic-addresses :reader cpu-symbolic-addresses :initform (make-array 65536))
   (symbolic-address-max-length :reader cpu-symbolic-address-max-length)))

(defmethod (setf cpu-env) :after (env (cpu symbolic-cpu))
  (update-symbolic-addresses cpu env))

(defmethod update-symbolic-addresses ((cpu symbolic-cpu) env)
  (with-slots (symbolic-addresses symbolic-address-max-length) cpu
    (fill symbolic-addresses nil)
    (maphash (lambda (label address)
               (setf (aref symbolic-addresses address) label))
             env)
    (loop with label-address = 0
          for i below (length symbolic-addresses)
          if (aref symbolic-addresses i)
             do (setf label-address i)
          else
            do (setf (aref symbolic-addresses i)
                     (format nil "~@[~A~]+~D"
                             (aref symbolic-addresses label-address)
                             (- i label-address))))
    (setf symbolic-address-max-length (reduce #'max (map 'list #'length symbolic-addresses)))))

(defmethod initialize-instance :after ((cpu symbolic-cpu) &key env)
  (update-symbolic-addresses cpu env))

(defgeneric format-address (cpu address)
  (:method (cpu address)
    (format nil "$~4,'0X" address)))

(defmethod format-address ((cpu symbolic-cpu) address)
  (format nil "$~4,'0X ~VA " address (cpu-symbolic-address-max-length cpu) (aref (cpu-symbolic-addresses cpu) address)))

(defmethod print-object ((cpu cpu) out)
  (format out "#<CPU PC:~A A:$~2,'0X X:$~2,'0X Y:$~2,'0X SR:$~2,'0X (~A) SP:$~2,'0X (~:D) - ~A>"
          (format-address cpu (cpu-pc cpu))
          (cpu-ar cpu)
          (cpu-xr cpu)
          (cpu-yr cpu)
          (cpu-sr cpu)
          (decode-status (cpu-sr cpu))
          (cpu-sp cpu)
          (cpu-cc cpu)
          (disassemble-instruction-at (cpu-pc cpu))))

(defun execute (cpu)
  "Step the CPU until a BRK instruction."
  (loop for opcode of-type u8 = (get-byte (cpu-pc cpu))
     do (handler-case (step-cpu cpu opcode)
          (undefined-function ()
            (error 'illegal-opcode :opcode opcode)))
     until (zerop opcode)))

(defun step-cpu (cpu opcode)
  "Step the CPU through the next OPCODE."
  (funcall (aref *opcode-funs* opcode) cpu))

(defun trace-cpu ()
  (trace step-cpu :report nil :print *cpu*))

(defmacro trace-cpu-when (condition)
  `(trace step-cpu
          :report nil
          :print *cpu*
          :condition ,condition))

(defmacro trace-cpu-between (from to)
  `(trace step-cpu
          :report nil
          :print *cpu*
          :condition (<= ,from (6502:cpu-pc cl-6502:*cpu*) ,to)))
