(defpackage ningle-tutorial-project/config
  (:use :cl :envy))
(in-package ningle-tutorial-project/config)

(dotenv:load-env (asdf:system-relative-pathname :ningle-tutorial-project ".env"))
(setf (config-env-var) "APP_ENV")

(defconfig :common
  `(:application-root ,(asdf:component-pathname (asdf:find-system :ningle-tutorial-project))))

(defconfig |sqlite|
  `(:debug T
    :middleware ((:session)
                 (:mito (:sqlite3 :database-name ,(uiop:getenv "SQLITE_DB_NAME")))
                 (:mount "/auth" ,ningle-auth:*app*)
                 (:static :root ,(asdf:system-relative-pathname :ningle-tutorial-project "src/static/") :path "/public/"))))

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
