(in-package :asterblaster)

(defvar *server-thread*)
(defvar *resource-listener-thread*)
(defvar *update-game-state-thread*)
(defvar *send-state-update-thread*)

(defun start-asterblaster-server ()
  (setf *server-thread* (bordeaux-threads:make-thread 
                         (lambda ()
                           (run-server 13373))
                         :name "Asterblaster"))
  (setf *resource-listener-thread* (bordeaux-threads:make-thread
                                    (lambda ()
                                      (run-resource-listener
                                       (find-global-resource "/api")))
                                    :name "resource listener for /api"))
  (setf *update-game-state-thread* (bordeaux-threads:make-thread 
                                    #'update-game-state
                                    :name "game state updater"))  
  (setf *send-state-update-thread* (bordeaux-threads:make-thread 
                                    (forever #'send-state-to-clients)
                                    :name "client state notifier")))

(defun stop-asterblaster-server ()
  (sb-thread:destroy-thread *server-thread*)
  (sb-thread:destroy-thread *resource-listener-thread*)
  (send *update-state-channel* (make-instance 'stop-message))
  (sb-thread:destroy-thread *send-state-update-thread*))

(defun restart-asterlaster-server ()
  (stop-asterblaster-server)
  (start-asterblaster-server))
