{:user {:plugins [[lein-pprint "1.2.0"]
                  [lein-kibit "0.1.6"]
                  [lein-try "0.4.3"]
                  [jonase/eastwood "0.2.7"]
                  [lein-ancient "0.6.15"]
                  ]
        :dependencies [[slamhound "1.5.5"]]
        :aliases {"slamhound" ["run" "-m" "slam.hound"]}}
 :repl {:dependencies [^:displace [org.clojure/clojure "1.9.0"]]}
 }
