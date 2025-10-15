(ql:quickload '(:iolib :iolib/os))
(defpackage :exec (:use :cl :iolib :iolib/os))
(in-package :exec)



(iolib/streams:fd-of stream) => fd
(iolib/streams::read-fn-of stream) => read-fn
(iolib/streams::input-buffer-of stream) => input buffer
(iolib/streams::%fill-ibuf/no-hang buffer fd read-fn)
(iolib/streams::%flush-obuf-if-needed stream)



;;
;;
;; <##< read section (reading advances the left boundary)
;; [--[ free section (writing advances the left boundary)
;; | is when < and [ are the same
;;
;; Typically a circular buffer looks like this
;;
;;  --|########## DATA ###############|-------- free ------------
;;
;; At the circular buffer boundaries, there might need two system calls to
;; fill the buffer, one to fill up to the end of the array, and one from index
;; 0 up to the beginning of DATA section.
;;
;; But sometimes the FD will give an amount of new read data that makes the
;; next read index very close to the end of the array.
;;
;;  --|########## DATA ###############****** newly read *****|---
;;
;; The final segment can be sacrificed temporarily, the new free position can
;; directly be position 0. That's why at this point we need a position for
;; DATA end:
;;
;;  [-|########## DATA ######################################<===
;;
;; If the free space is too small, the queue is assumed to be full even if its
;; not 100% full, so that we don't send small packets of data per syscall.
;;
;; The section <===[ is ignored until we emit the data up to its end (<).
;; Then, the start of the new data is known to be 0.

;; Suppose a different case:
;;
;;  ################|-- free ------|######### DATA ##############
;;
;; We write as much data as we can.
;;
;;  ################|-- free --------------------------------|###
;;                                                           ^
;;
;; Unlike free space, we cannot ignore small packets of data near the end.
;; Also, if data part on the left was small enough, we could defragment the
;; memory and move data:
;;
;;  AAAA|-------------- free --------------------------------|BBB
;;  BBBAAAA|----------- free ------------------------------------
;;
;; This ensure we have one less system call to write the next time we write
;; data. But if the "A" section on the left of the array is big enough, this
;; might be preferable two system calls.
;;
;; But, assuming the small size is defined by S, we could also have a reserved
;; section of size S on the left of the buffer. Let's say S is 10 (written ~,
;; delimited by /)
;;
;; ~~~~~~~~~~/AAAAAAAAAAAAAAA|--------- free ----------------|BBB
;;
;; When we see that section B fits in the reserved space, we right-align the
;; data in that space:
;;
;; ~~~~~~~<BB/AAAAAAAAAAAAAAA|--------- free -------------------[
;;
;; The starting position for writes is now -3 (in a frame where reserved space
;; is negative). This makes the new DATA section contiguous.
;;
;; Should free space use the reserved space? No, this defeats the purpose of
;; that memory section.



