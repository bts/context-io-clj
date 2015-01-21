(ns context-io.request
  "Helper functions for dealing with requests."
  (:use
    [context-io.callbacks])
  (:require
    [http.async.client :as ac]
    [http.async.client.util :as requ]
    [http.async.client.request :as req]
    [clojure.string :as string])
  (:import
    (com.ning.http.client Cookie PerRequestConfig RequestBuilder)
    (java.io File InputStream)))

(defn execute-request
  "Submit the actual request, transforming the response with provided handlers.

   client   - The client to use for submitting the request.
   req      - The request to submit.
   handlers - A map of keyword event names to handler functions. Possible event
             names are :on-success, :on-failure, and :on-exception.
   async    - True if http client should be invoked asynchronously

   Returns the response as transformed by the appropriate handler, or a function
   to cancel the response if async is true."
  [client req handlers & [async]]
  (let [handle #(handle-response % handlers)]
    (if async
      (let [resp (req/execute-request client
                                      req
                                      :completed handle
                                      :error (fn [resp _throwable]
                                               (handle resp)))
            cancel (:cancel (meta resp))]
        cancel)
      (let [response (req/execute-request client req)]
        (ac/await response)
        (handle response)))))

(defn- add-to-req
  [rb kvs f]
  (doseq [[k v] kvs] (f rb
                        (if (keyword? k) (name k) k)
                        (if (coll? v)
                          (string/join "," v)
                          (str v)))))

(defn- add-headers
  "Adds headers to the request builder."
  [rb headers]
  (add-to-req rb headers #(.addHeader %1 %2 %3)))

(defn- add-cookies
  "Adds the cookies to the request builder."
  [rb cookies]
  (doseq [{:keys [domain name value path max-age secure]
           :or {path "/" max-age 30 secure false}} cookies]
    (.addCookie rb (Cookie. domain name value path max-age secure))))

(defn- add-query-parameters
  "Adds the query parameters to the request builder."
  [rb query]
  (add-to-req rb query #(.addQueryParameter %1 %2 %3)))

(defn- add-body
  "Adds the body (or sequence of bodies) onto the request builder, dealing with
   the special cases."
  [rb body content-type]
  (cond
    (= "multipart/form-data" content-type) (doseq [bp (if (coll? body)
                                                   body (list body))]
                                              (.addBodyPart rb bp))
    (map? body) (doseq [[k v] body]
                  (.addParameter rb
                                 (if (keyword? k) (name k) k)
                                 (str v)))
    (string? body) (.setBody rb (.getBytes
                                  (if (= "application/x-www-form-urlencoded"
                                         content-type)
                                    (req/url-encode body)
                                    body)
                                  "UTF-8"))
    (instance? InputStream body) (.setBody rb body)
    (instance? File body) (.setBody rb body)))

(defn- set-timeout
  "Sets the timeout for the request."
  [rb timeout]
  (let [prc (PerRequestConfig.)]
    (.setRequestTimeoutInMs prc timeout)
    (.setPerRequestConfig rb prc)))

(defn prepare-request-with-multi
  "Prepare a request, allowing multi-part form data

   method   - The HTTP method for the request (:get, :post, etc.).
   url      - The URL for the request.
   :headers - The headers for the request (optional).
   :query   - The query parameters for the request (optional).
   :body    - The body for the request (optional).
   :cookies - The cookies for the request (optional).
   :proxy   - The proxy for the request (optional).
   :auth    - The authentication details for the request (optional).
   :timeout - The timeout for the request (optional).

   Returns the request with the given information."
  [method #^String url & {:keys [headers query body cookies proxy auth timeout]}]
  (let [rb (RequestBuilder. (req/convert-method method))]
    (when headers (add-headers rb headers))
    (when query (add-query-parameters rb query))
    (when body (add-body rb body (:content-type headers)))
    (when cookies (add-cookies rb cookies))
    (when auth (requ/set-realm auth rb))
    (when proxy (requ/set-proxy proxy rb))
    (when timeout (set-timeout rb timeout))
    (.. rb (setUrl url) (build))))
