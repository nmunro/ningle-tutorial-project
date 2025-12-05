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
                 (:static :root ,(asdf:system-relative-pathname :ningle-tutorial-project "src/static/") :path "/public/"))))

(defconfig |mysql|
  `(:middleware ((:session)
                 (:mito (:mysql
                         :database-name ,(uiop:native-namestring (uiop:parse-unix-namestring (or (uiop:getenv "MYSQL_DB_NAME") "")))
                         :username ,(or (uiop:getenv "MYSQL_USER") "")
                         :password ,(or (uiop:getenv "MYSQL_PASSWORD") "")
                         :host ,(or (uiop:getenv "MYSQL_ADDRESS") "")
                         :port (parse-integer ,(or (uiop:getenv "MYSQL_PORT") 0))))
                 (:static :root ,(asdf:system-relative-pathname :ningle-tutorial-project "src/static/") :path "/public/"))))

(defconfig |postgresql|
  `(:middleware ((:session)
                 (:mito (:postgres
                         :database-name ,(uiop:native-namestring (uiop:parse-unix-namestring (or (uiop:getenv "POSTGRES_DB_NAME") "")))
                         :username ,(or (uiop:getenv "POSTGRES_USER") "")
                         :password ,(or (uiop:getenv "POSTGRES_PASSWORD") "")
                         :host ,(or (uiop:getenv "POSTGRES_ADDRESS") "")
                         :port (parse-integer ,(or (uiop:getenv "POSTGRES_PORT") 0))))
                 (:static :root ,(asdf:system-relative-pathname :ningle-tutorial-project "src/static/") :path "/public/"))))
