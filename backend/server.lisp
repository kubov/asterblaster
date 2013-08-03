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

(defun start-asterblaster-server ()
  (bordeaux-threads:make-thread (lambda ()
                                (run-server 13373))
                                :name "Asterblaster")
  (bordeaux-threads:make-thread (lambda ()
                                (run-resource-listener
                                 (find-global-resource "/api")))
                              :name "resource listener for /api"))

(def class* server-message ()
  ((msg-type "unknown" :type string)
   (data nil)))

(def class* client-message ()
  ((msg-type "unknown" :type string)
   (data nil)))

(def class* hello-client-message ()
  ((name :type string)))


(defparameter *client-message-classes-assoc*
  '(("hello" . hello-client-message)))

(defun json-to-client-message (json)
  (let* ((alist (decode-json-from-string json))
         (msg-type-string (assoc-cdr :msg-type alist))
         (msg-class (assoc-cdr msg-type-string *client-message-classes-assoc*
                               :test #'equal)))
    (when (or (not msg-type-string) (not msg-class))
      (error "wrong message type"))
    (make-instance 'client-message :msg-type msg-type-string
                   :data (apply #'make-instance msg-class 
                                (alist-plist (assoc-cdr :data alist))))))

