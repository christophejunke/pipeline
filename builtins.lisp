(in-package :pipeline.builtins)

(defun tee (&rest streams)
  (lambda ()
    (uiop:copy-stream-to-stream *standard-input*
                                (apply #'make-broadcast-stream
                                       *standard-output*
                                       streams))))

(defun tee/error ()
  (tee *error-output*))
