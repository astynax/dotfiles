{:user {:plugins [[lein-pprint "1.3.2"]
                  [lein-kibit "0.1.8"]
                  [lein-try "0.4.3"]
                  [jonase/eastwood "0.4.3"]
                  [lein-ancient "0.7.0"]
                  ]
        :dependencies [[slamhound "1.5.5"]]
        :aliases {"slamhound" ["run" "-m" "slam.hound"]}}
 :repl {:dependencies [^:displace [org.clojure/clojure "1.9.3"]]}
 }
