(in-package :asdf-user)

(defsystem "adventcode.2024"
    :description "advent code of 2024"
    :version (:read-file-line "VERSION")
    :author "rango.he"
    :license "MIT"
    :homepage "https://github.com/Matisudaz/adventofcode/tree/main/2024"
    :depends-on (:alexandria
                 :series)
    :components
    ((:module "src"
      :components
      ((:file "initialize")
       (:file "package" :depends-on ("initialize"))
       (:file "misc" :depends-on ("package"))
       (:module "day1"
        :depends-on ("misc")
        :components ((:file "solution")))
       (:module "day2"
        :depends-on ("misc")
        :components ((:file "solution")))))))

  
