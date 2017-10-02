(defsystem :stumpwm.thermal-sensor-freebsd
  :name :stumpwm.thermal-sensor-freebsd
  :version "1.0"
  :author "Vasily Postnicov <shamaz.mazum at gmail dot com>"
  :components ((:file "thermal-sensor-freebsd"))
  :depends-on (:stumpwm :freebsd-sysctl))
