(in-package :asterblaster)

(defun start-asterblaster-server ()
  (bordeaux-threads:make-thread (lambda ()
                                (run-server 13373))
                                :name "Asterblaster")
  (bordeaux-threads:make-thread (lambda ()
                                (run-resource-listener
                                 (find-global-resource "/api")))
                              :name "resource listener for /api")
  (bordeaux-threads:make-thread #'update-game-state
                                :name "game state updater"))
