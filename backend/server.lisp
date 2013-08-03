(in-package :asterblaster)

(setf clws:*debug-on-server-errors* t)
(setf clws:*debug-on-resource-errors* t)



(defclass api-resource (ws-resource)
  ())

(defmethod resource-received-text ((res api-resource) client message)
  (write-to-client-text client message))

(defmethod resource-client-connected ((res api-resource) client)
  (format t "[connection on api server from ~s : ~s]~%"
          (client-host client) (client-port client))
  t)

(defmethod resource-client-disconnected ((resource api-resource) client)
  (format t "[disconnected from resource ~A: ~A]~%" resource client))


(register-global-resource "/api"
                          (make-instance 'api-resource)
                          (origin-prefix nil))

(bordeaux-threads:make-thread (lambda ()
                                (run-resource-listener
                                 (find-global-resource "/api")))
                              :name "resource listener for /api")

(defun start-asterblaster-server ()
  (bordeaux-threads:make-thread (lambda ()
                                (run-server 13373))
                                :name "Asterblaster"))
