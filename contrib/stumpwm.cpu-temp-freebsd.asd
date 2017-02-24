(defsystem :stumpwm.cpu-temp-freebsd
  :name :stumpwm.cpu-temp-freebsd
  :version "1.0"
  :author "Vasily Postnicov <shamaz.mazum at gmail dot com>"
  :components ((:file "cpu-temp-freebsd"))
  :depends-on (:stumpwm :freebsd-sysctl))
