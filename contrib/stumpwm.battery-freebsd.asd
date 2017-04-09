(defsystem :stumpwm.battery-freebsd
  :name :stumpwm.battery-freebsd
  :version "1.0"
  :author "Vasily Postnicov <shamaz.mazum at gmail dot com>"
  :components ((:file "battery-freebsd"))
  :depends-on (:stumpwm :freebsd-sysctl))
