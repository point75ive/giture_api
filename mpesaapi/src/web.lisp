(in-package :cl-user)
(defpackage mpesaapi.web
  (:use :cl
        :caveman2
        :mpesaapi.config
        :mpesaapi.view
        :mpesaapi.db
        :datafly
        :sxql)
  (:export :*web*))
(in-package :mpesaapi.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules

(defroute "/" ()
  (render #P"index.html"))

;; test information from test app from "https://developer.safaricom.co.ke/"

(defparameter *shortcode* "174379")
(defparameter *consumer-key* "XfrYhGXoEcaLpT8qGVe8ozYGR5YGKl7ziGRwDhA9qPJ8D4AR")
(defparameter *consumer-secret* "zkF0EWZFIWsH8mxA9XlajDQVv1L5bKp0b2hbPfaKNWiAeCzASIK2PRiVd3ttjXo6")
(defparameter *passkey* "bfb279f9aa9bdbcf158e97dd71a467cd2e0c893059b10f78e6b72ada1ed2c919")
(defparameter *access-token-url* "https://sandbox.safaricom.co.ke/oauth/v1/generate?grant_type=client_credentials")
(defparameter *stk-push-url* "https://sandbox.safaricom.co.ke/mpesa/stkpush/v1/processrequest")
(defparameter *stk-push-query* "https://sandbox.safaricom.co.ke/mpesa/stkpushquery/v1/query")
(defparameter *callback-url* "https://rnwqs-197-232-62-191.a.free.pinggy.link/stk/response")


(defun get-mpesa-access-token (access-url consumer-key consumer-secret)
  (let* ((encoded (concatenate 'string "Basic " (base64:string-to-base64-string (format nil "~a:~a" consumer-key consumer-secret))))
         (response (multiple-value-bind (body status headers url connection)
                       (dex:get access-url :headers `(("Authorization" . ,encoded)) :verbose t)
                     (declare (ignore headers url connection))
                     (list :body body :status status))))    
    (if (eq (getf response :status) 200)
        (let* ((json-response (shasht:read-json (getf response :body)))               
               (token (gethash  "access_token" json-response ))
               (expires-in (gethash "expires_in" json-response )))
          (list :access-token token :access-token-expiration (+ (get-universal-time) (parse-integer expires-in))))
        (error "Failed to get access token"))))


(defun make-stk-push (shortcode passkey timestamp callback-url stk-push-url amount phone-no-cleaned access-token)
  (let* ((push-data `(("BusinessShortCode" . ,shortcode)
                      ("Password" . ,(base64:string-to-base64-string (concatenate 'string shortcode passkey timestamp)))
                      ("Timestamp" . ,timestamp)
                      ("TransactionType" . "CustomerPayBillOnline")
                      ("Amount" . ,(ceiling (parse-integer amount)))
                      ("PartyA" . ,phone-no-cleaned)
                      ("PartyB" . ,shortcode)
                      ("PhoneNumber" . ,phone-no-cleaned)
                      ("CallBackURL" . ,callback-url)
                      ("AccountReference" . "CompanyXLTD")
                      ("TransactionDesc" . "Payment of X")))
         (push-data-json (shasht:write-json* push-data :stream nil
                                                       :alist-as-object t ))
         (encoded (concatenate 'string "Bearer " access-token))
         (response (multiple-value-bind (body status headers url connection)
                       (dex:post stk-push-url
                                 :headers `(("Authorization" . ,encoded)
                                            ("Accept" . "application/json")
                                            (:content-type . "application/json"))
                                 :content push-data-json
                                 :verbose t)
                     (declare (ignore headers url connection))
                     (list :body body :status status))))    
    (if (eq (getf response :status) 200)
        (let* ((json-response (shasht:read-json (getf response :body)))
               (CheckoutRequestID (gethash  "CheckoutRequestID" json-response)))
          (list :CheckoutRequestID CheckoutRequestID))
        (error "Failed to push SDK"))))

(defun query-transaction-status (shortcode passkey timestamp checkout-request-id query-status-url consumer-key consumer-secret)
  (let* ((encoded (concatenate 'string "Basic "
                               (base64:string-to-base64-string
                                (format nil "~a:~a" consumer-key consumer-secret))))
         (query-data `(("BusinessShortCode" . ,shortcode)
                       ("Password" . ,(base64:string-to-base64-string (concatenate 'string shortcode passkey timestamp)))
                       ("Timestamp" . ,timestamp)
                       ("CheckoutRequestID" . ,checkout-request-id)))
         (query-data-json (shasht:write-json* query-data :stream nil
                                                         :alist-as-object t ))
         (response (multiple-value-bind (body status headers url connection)
                       (dex:post query-status-url :content query-data-json
                                                  :headers `(("Authorization" . ,encoded))
                                                  :verbose t)
                     (declare (ignore headers url connection))
                     (list :body body :status status))))
    (if (eq (getf response :status) 200)
        (let* ((json-response (shasht:read-json (getf response :body)))
               (CheckoutRequestID (gethash  "CheckoutRequestID" json-response))
               (ResultCode (gethash  "ResultCode" json-response)))
          (when (not (eq ResultCode "0"))
            (error "Request canceled by the user"))
          (list :CheckoutRequestID CheckoutRequestID :ResultCode "ResultCode"))        
        (error "Failed to push SDK"))))

(defroute ("/mpesa-checkout" :method :get) ()
  (render #P"cart/mpesa-checkout.html"))

(defroute ("/mpesa-checkout" :method :post) (&key _parsed)
  (let* (
         (amount (get-param "amount" _parsed))
         (phone-no (get-param "phone-no" _parsed))
         (phone-no-cleaned (if (and phone-no (string= (subseq phone-no 0 1) "0"))
                               (concatenate 'string "254" (subseq phone-no 1))
                               (error "Invalid phone number")))
         (status "Pending")
         (mpesa-created-at (multiple-value-bind (second minute hour date month year day daylight-p zone)
                               (get-decoded-time)
                             (declare (ignore day daylight-p zone))
                             (format nil "~4,'0D~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D"
                                     year month date hour minute second)))
         (transaction-id (concatenate 'string phone-no-cleaned "-" mpesa-created-at)))
    
    ;; Make the calls to MPESA    
    (let* ((shortcode *shortcode*)
           (access-url *access-token-url*)
           (consumer-key *consumer-key*)
           (consumer-secret *consumer-secret*)
           (stk-push-url *stk-push-url*)
           (callback-url *callback-url*)
           (passkey *passkey*)
           (timestamp mpesa-created-at)
           (access-response (get-mpesa-access-token access-url consumer-key consumer-secret))
           (access-token (getf access-response :access-token))
           (CheckoutRequestID (getf (make-stk-push shortcode passkey timestamp callback-url stk-push-url amount phone-no-cleaned access-token) :CheckoutRequestID))
           (new-transaction (make-instance 'mpesaapi.mpesa::mpesa-transaction
                                           :amount amount
                                           :phone-no phone-no-cleaned
                                           :status status
                                           :timestamp mpesa-created-at
                                           :transaction-id transaction-id
                                           :merchant-req-id CheckoutRequestID)))
      ;; Create a new transaction and save it to our database
      (with-connection (db)
        (mito:save-dao new-transaction))
      (let* ((latest-transaction (with-connection (db)
                                   (first (mito:select-dao 'mpesaapi.mpesa::mpesa-transaction
                                            (where (:= :phone-no phone-no-cleaned))
                                            (order-by (:desc :created-at))
                                            (limit 1))))))
        (setf (gethash :notice ningle:*session*) "Please enter your M-Pesa pin when prompted.")   
        (render #P"cart/invoice.html" `(:transaction ,latest-transaction
                                        :notice ,(flash-gethash :notice ningle:*session*)))))))

(defroute ("/invoice" :method :post) (&key _parsed)
  (let* ((mpesa-ref (get-param "mpesa_ref" _parsed))
         (transaction (with-connection (db)
                        (first (mito:select-dao 'mpesaapi.mpesa::mpesa-transaction
                                 (where (:= :mpesa-ref mpesa-ref)))))))
    (setf (gethash :notice ningle:*session*) "Mpesa transaction Confirmed.")
    (when transaction
      (render #P"cart/confirmation.html" `(:transaction ,transaction
                                           :notice ,(flash-gethash :notice ningle:*session*))))))



;; HELP ;;


(defun extract-mpesa-receipt-number (text)
  "Extracts the MpesaReceiptNumber value from the given text. 
   Args: text: The input text string. 
   Returns: The extracted MpesaReceiptNumber value if found, otherwise nil. "
  (let* ((second-start-index (search "((Value " text :start2 (1+ (search "((Value " text))))
         (value-start (+ second-start-index (length "((Value . ")))
         (value-end (search ")" text :start2 value-start)))
    (when (and value-start value-end) (subseq text value-start value-end))))

(defun extract-checkout-request-id (text)
  "Extracts the CheckoutRequestID value from the given text. 
   Args: text: The input text string. 
   Returns: The extracted CheckoutRequestID value if found, otherwise nil. "
  (let* ((start-index (search "(CheckoutRequestID . " text))
         (value-start (+ start-index (length "(CheckoutRequestID . ")))
         (value-end (search ")" text :start2 value-start)))
    (when (and value-start value-end) (subseq text value-start value-end))))

(defroute ("/stk/response" :method :post :accept "application/JSON") (&key _parsed)
  (let* ((response (format nil "response: ~a~%" _parsed))
         (format (format t "response: ~a~%" ningle:*request*))
         (mpesa-ref (extract-mpesa-receipt-number response))
         (merchant-req-id (extract-checkout-request-id response))
         (transaction (first (mito:select-dao 'mpesa-transaction
                               (where (:= :merchant-req-id merchant-req-id))))))
    (when transaction
      (setf (getf transaction :status) "Completed")
      (setf (getf transaction :mpesa-ref) mpesa-ref)
      (let ((payment (make-instance 'mpesaapi.mpesa::payment
                                    :result-code "0"
                                    :phone-no (getf transaction :phone-no)
                                    :amount (getf transaction :amount)
                                    :mpesa-ref mpesa-ref
                                    :merchant-req-id merchant-req-id)))
        (mito:save-dao payment)
        (mito:update-dao transaction)        
        
        ;; Alert payment processed
        (render #P"cart/confirmation.html" `(:transaction ,transaction))))))



;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
