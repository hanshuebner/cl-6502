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

(defmethod print-object ((cpu cpu) out)
  (format out "#<CPU PC:$~4,'0X A:$~2,'0X X:$~2,'0X Y:$~2,'0X SR:$~2,'0X (~A) SP:$~2,'0X (~:D) - ~A>"
          (cpu-pc cpu)
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
