(defpackage ningle-tutorial-project/config
  (:use :cl :envy))
(in-package ningle-tutorial-project/config)

(dotenv:load-env (asdf:system-relative-pathname :ningle-tutorial-project ".env"))
(setf (config-env-var) "APP_ENV")

(defparameter *auth-mount-path* "/auth")

(defconfig :common
  `(:application-root ,(asdf:component-pathname (asdf:find-system :ningle-tutorial-project))
    :installed-apps (:ningle-auth)
    :auth-mount-path ,*auth-mount-path*
    :login-redirect "/"
    :project-name "NTP"
    :token-expiration 3600
    :email-admins ("nmunro@duck.com")))

(defconfig |database-settings|
  `((:mito (:sqlite3 :database-name ,(uiop:getenv "SQLITE_DB_NAME")))))

(defconfig |middleware|
  `(:middleware ((:session)
                 ningle-tutorial-project/middleware:refresh-roles
                 ,@|database-settings|
                 (:mount ,*auth-mount-path* ,ningle-auth:*app*)
                 (:static :root ,(asdf:system-relative-pathname :ningle-tutorial-project "src/static/") :path "/public/"))))

(defconfig |dummy-email|
`(:debug T
    ,@|middleware|
    :email-backend :dummy
    :email-default-from ,(uiop:getenv "EMAIL_DEFAULT_FROM")))

(defconfig |gmail-smtp|
  `(:debug T
    ,@|middleware|
    :email-backend :smtp
    :email-smtp-host ,(uiop:getenv "SMTP_GMAIL_HOST")
    :email-default-from ,(uiop:getenv "SMTP_GMAIL_ACCOUNT_NAME")
    :email-reply-to ,(uiop:getenv "SMTP_GMAIL_ACCOUNT_NAME")
    :email-port 587
    :email-auth (,(uiop:getenv "SMTP_GMAIL_ACCOUNT_NAME") ,(uiop:getenv "SMTP_GMAIL_PASSWORD"))
    :email-ssl :starttls))

(defconfig |ethereal-smtp|
  `(:debug T
    ,@|middleware|
    :email-backend :smtp
    :email-smtp-host ,(uiop:getenv "SMTP_ETHEREAL_HOST")
    :email-default-from ,(uiop:getenv "SMTP_ETHEREAL_ACCOUNT_NAME")
    :email-reply-to ,(uiop:getenv "SMTP_ETHEREAL_ACCOUNT_NAME")
    :email-port 587
    :email-auth (,(uiop:getenv "SMTP_ETHEREAL_ACCOUNT_NAME") ,(uiop:getenv "SMTP_ETHEREAL_PASSWORD"))
    :email-ssl :starttls))

(defconfig |sendgrid|
  `(:debug T
    ,@|middleware|
    :email-backend :sendgrid
    :email-reply-to ,(uiop:getenv "EMAIL_DEFAULT_FROM")
    :sendgrid-api-key ,(uiop:getenv "SENDGRID_API_KEY")))

(defconfig |mysql|
  `(:middleware ((:session)
                 (:mito (:mysql
                         :database-name ,(uiop:native-namestring (uiop:parse-unix-namestring (uiop:getenv "MYSQL_DB_NAME")))
                         :username ,(uiop:getenv "MYSQL_USER")
                         :password ,(uiop:getenv "MYSQL_PASSWORD")
                         :host ,(uiop:getenv "MYSQL_ADDRESS")
                         :port ,(parse-integer (uiop:getenv "MYSQL_PORT"))))
                 (:static :root ,(asdf:system-relative-pathname :ningle-tutorial-project "src/static/") :path "/public/"))))

(defconfig |postgresql|
  `(:middleware ((:session)
                 (:mito (:postgres
                         :database-name ,(uiop:native-namestring (uiop:parse-unix-namestring (uiop:getenv "POSTGRES_DB_NAME")))
                         :username ,(uiop:getenv "POSTGRES_USER")
                         :password ,(uiop:getenv "POSTGRES_PASSWORD")
                         :host ,(uiop:getenv "POSTGRES_ADDRESS")
                         :port ,(parse-integer (uiop:getenv "POSTGRES_PORT"))))
                 (:static :root ,(asdf:system-relative-pathname :ningle-tutorial-project "src/static/") :path "/public/"))))

