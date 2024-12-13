(in-package :cl-user)

(uiop:define-package mpesaapi.mpesa
    (:use :cl
     :mpesaapi.db
          :mpesaapi.config
     :mito
          :dexador
     :shasht)
  (:shadowing-import-from :dexador :get :delete))


(in-package :mpesaapi.mpesa)

(defclass mpesa-transaction ()
  ((transaction-id :col-type (:varchar 30)
                   :initarg :transaction-id
                   :accessor transaction-id)
   (phone-no :col-type (:varchar 20)
             :initarg :phone-no
             :accessor phone-no)
   (amount :col-type :integer
           :initarg :amount
           :accessor amount)
   (status :col-type (:varchar 20)
           :initarg :status
           :accessor status)
   (merchant-req-id :col-type (or :null (:varchar 40))
                    :initarg :merchant-req-id
                    :accessor merchant-req-id)
   (timestamp :col-type (or :null :text)
              :accessor timestamp
              :initarg :timestamp)
   (mpesa-ref :col-type (or :null (:varchar 20))
              :initarg :mpesa-ref
              :accessor mpesa-ref))
  (:documentation "An mpesa transaction.")
  (:metaclass mito:dao-table-class))

(defclass payment ()
  ((result-code :col-type :integer
                :initarg :result-code
                :accessor result-code)
   (phone-no :col-type (:varchar 20)
             :initarg :phone-no
             :accessor phone-no)
   (amount :col-type :integer
           :initarg :amount
           :accessor amount)
   (mpesa-ref :col-type (:varchar 20)
              :initarg :mpesa-ref
              :accessor mpesa-ref)
   (merchant-req-id :col-type (or :null (:varchar 40))
                    :initarg :merchant-req-id
                    :accessor merchant-req-id))
  (:documentation "An mpesa payment.")
  (:metaclass mito:dao-table-class))
