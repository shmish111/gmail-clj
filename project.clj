(defproject gmail-clj "0.6.2"
  :description "A simple Clojure library to interface with the GMail API (read: not imap)"
  :url "https://github.com/mikeflynn/gmail-clj"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [aleph "0.4.0"]
                 [cheshire "5.5.0"]
                 [org.clojure/data.codec "0.1.0"]
                 [javax.mail/mail "1.4.7"]]
  :pedantic? :abort
  :plugins [[lein-cljfmt "0.3.0"]])
