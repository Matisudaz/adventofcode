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
      ((:module "day1"
        :components ((:file "day1")))))))

  
