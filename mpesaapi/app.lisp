(ql:quickload :mpesaapi)

(defpackage mpesaapi.app
  (:use :cl)
  (:import-from :lack.builder
                :builder)
  (:import-from :ppcre
                :scan
                :regex-replace)
  (:import-from :mpesaapi.web
                :*web*)
  (:import-from :mpesaapi.config
                :config
                :productionp
                :*static-directory*))
(in-package :mpesaapi.app)

(funcall
 clack-errors:*clack-error-middleware* 
 (builder
  (:static
   :path (lambda (path)
           (if (ppcre:scan "^(?:/images/|/css/|/js/|/robot\\.txt$|/favicon\\.ico$)" path) path nil))
   :root *static-directory*)
  
  
  (if (productionp) nil :accesslog)
  (if (getf (config) :error-log)
      `(:backtrace :output ,(getf (config) :error-log))
      nil)
  
  (if (productionp) nil
      (lambda (app) (lambda (env)
                      (let ((datafly:*trace-sql* t))
                        (funcall app env)))))

  :session
  
  *web*)
 :debug t)
