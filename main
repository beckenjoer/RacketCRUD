#lang racket

(require web-server/servlet
         web-server/servlet-env
         web-server/dispatch
         db
         racket/format
         racket/date
         json)

;; Database connection
(define pgc
  (postgresql-connect #:user "***"
                      #:database "***"
                      #:password "***"))

;; Helper function to parse JSON request body
(define (call-with-request-body-json req proc)
  (define raw (request-post-data/raw req))
  (define body-str (bytes->string/utf-8 raw))
  (define json-data (read-json (open-input-string body-str)))
  (proc json-data))

;; Helper function to produce a JSON response
(define (response/json json-data)
  (response/full
   #:code 200
   #:headers (list (cons "Content-Type" "application/json"))
   #:body (jsexpr->string json-data)))

;; Read records from the database
(define (read-records)
  (define query "SELECT id, s_code, s_name, d_create FROM table_1 ORDER BY id")
  (define records (query-rows pgc query))
  (for/list ([row records])
    (hash 'id (vector-ref row 0)
          's_code (vector-ref row 1)
          's_name (vector-ref row 2)
          'd_create (vector-ref row 3))))

;; Create a new record
(define (create-record post-data)
  (define s_code (hash-ref post-data 's_code))
  (define s_name (hash-ref post-data 's_name))
  (define query (format "INSERT INTO table_1 (s_code, s_name) VALUES ('~a', '~a')" s_code s_name))
  (query-exec pgc query)
  #t)

;; Update an existing record
(define (update-record id s_name)
  (define query (format "UPDATE table_1 SET s_name = '~a' WHERE id = ~a" s_name id))
  (query-exec pgc query)
  #t)

;; Delete a record
(define (delete-record id)
  (define query (format "DELETE FROM table_1 WHERE id = ~a" id))
  (query-exec pgc query)
  #t)

;; Format SQL timestamp
(define (format-timestamp timestamp)
  (if (sql-timestamp? timestamp)
      (format "~a-~a-~a ~a:~a:~a"
              (sql-timestamp-year timestamp)
              (~r #:min-width 2 #:pad-string "0" (sql-timestamp-month timestamp))
              (~r #:min-width 2 #:pad-string "0" (sql-timestamp-day timestamp))
              (~r #:min-width 2 #:pad-string "0" (sql-timestamp-hour timestamp))
              (~r #:min-width 2 #:pad-string "0" (sql-timestamp-minute timestamp))
              (~r #:min-width 2 #:pad-string "0" (sql-timestamp-second timestamp)))
      "N/A"))

;; Generate an HTML table from records
(define (generate-table-xexpr records)
  (define (generate-row-xexpr record)
    `(tr
       (td ,(~a (hash-ref record 'id)))
       (td ,(hash-ref record 's_code))
       (td ,(hash-ref record 's_name))
       (td ,(format-timestamp (hash-ref record 'd_create)))
       (td
         (button
           ([data-id ,(~a (hash-ref record 'id))]
            [data-name ,(hash-ref record 's_name)]
            [class "edit-btn"])
           "Редактировать")
         (button
           ([data-id ,(~a (hash-ref record 'id))]
            [class "delete-btn"])
           "Удалить"))))
  `(table
     ([id "records-table"]
      [border "1"])
     (tr
       (th "ID")
       (th "Код")
       (th "Название")
       (th "Дата создания")
       (th "Действия"))
     ,@(map generate-row-xexpr records)))

;; Generate response for main page (read)
(define (response-for-read)
  (define records (read-records))
  (define table-xexpr (generate-table-xexpr records))
  (response/xexpr
   `(html
      (head
        (meta ([charset "UTF-8"]))
        (title "CRUD Приложение")
        (style
          "body { font-family: Arial, sans-serif; max-width: 800px; margin: 0 auto; padding: 20px; }
           table { width: 100%; border-collapse: collapse; margin-top: 20px; }
           th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
           th { background-color: #f2f2f2; }
           form { margin-bottom: 20px; }
           input { margin: 10px 0; padding: 5px; width: 100%; }
           button { margin: 10px 0; padding: 5px 10px; }")
        (script
          "document.addEventListener('DOMContentLoaded', function() {
             const form = document.querySelector('form');
             const table = document.getElementById('records-table');

             form.addEventListener('submit', function(e) {
               e.preventDefault();
               const formData = new FormData(form);
               fetch('/create', {
                 method: 'POST',
                 headers: {
                   'Content-Type': 'application/json',
                 },
                 body: JSON.stringify({
                   's_code': formData.get('s_code'),
                   's_name': formData.get('s_name')
                 })
               }).then(response => {
                 if (response.ok) {
                   location.reload();
                 }
               });
             });

             table.addEventListener('click', function(e) {
               if (e.target.classList.contains('delete-btn')) {
                 const id = e.target.getAttribute('data-id');
                 fetch(`/delete/${id}`, {
                   method: 'DELETE'
                 }).then(response => {
                   if (response.ok) {
                     location.reload();
                   }
                 });
               }

               if (e.target.classList.contains('edit-btn')) {
                 const id = e.target.getAttribute('data-id');
                 const currentName = e.target.getAttribute('data-name');
                 const newName = prompt('Введите новое название:', currentName);
                 if (newName) {
                   fetch('/edit', {
                     method: 'POST',
                     headers: {
                       'Content-Type': 'application/json',
                     },
                     body: JSON.stringify({
                       'id': id,
                       's_name': newName
                     })
                   }).then(response => {
                     if (response.ok) {
                       location.reload();
                     }
                   });
                 }
               }
             });
           });")
        )
      (body
        (h1 "CRUD Приложение")
        (form
          ([method "POST"] [action "/create"])
          (label "Код: ")
          (input
            ([type "text"]
             [name "s_code"]
             [placeholder "Введите код"]
             [required "required"]))
          (label "Название: ")
          (input
            ([type "text"]
             [name "s_name"]
             [placeholder "Введите название"]))
          (button ([type "submit"]) "Добавить"))
        ,table-xexpr))))

;; Dispatcher rules
(define-values (main-dispatch main-url)
  (dispatch-rules
   [("") (λ (req) (response-for-read))]
   [("create") #:method "POST"
    (λ (request)
      (define post-data
        (call-with-request-body-json
         request
         (λ (data)
           (hash 's_code (hash-ref data 's_code)
                 's_name (hash-ref data 's_name)))))
      (create-record post-data)
      (response/json (hash 'status "success")))]
   [("edit") #:method "POST"
    (λ (request)
      (define-values (id s_name)
        (call-with-request-body-json
         request
         (λ (data)
           (values
            (string->number (hash-ref data 'id))
            (hash-ref data 's_name)))))
      (update-record id s_name)
      (response/json (hash 'status "success")))]
   [("delete" (string-arg)) #:method "DELETE"
    (λ (request id)
      (delete-record (string->number id))
      (response/json (hash 'status "success")))]))

;; Start the server
(define (start)
  (serve/servlet main-dispatch
                 #:servlet-path "/"
                 #:port 8081
                 #:launch-browser? #f))

(start)
