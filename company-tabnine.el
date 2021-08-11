;;; company-tabnine.el --- A company-mode backend for TabNine
;;
;; Copyright (c) 2018 Tommy Xiang
;;
;; Author: Tommy Xiang <tommyx058@gmail.com>
;; Keywords: convenience
;; Version: 0.0.1
;; URL: https://github.com/TommyX12/company-tabnine/
;; Package-Requires: ((emacs "25") (company "0.9.3") (cl-lib "0.5") (dash "2.16.0") (s "1.12.0") (unicode-escape "1.1"))
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Commentary:
;;
;; Description:
;;
;; TabNine(https://tabnine.com/) is the all-language autocompleter.
;; It uses machine learning to provide responsive, reliable, and relevant suggestions.
;; `company-tabnine' provides TabNine completion backend for `company-mode'(https://github.com/company-mode/company-mode).
;; It takes care of TabNine binaries, so installation is easy.
;;
;; Installation:
;;
;; 1. Make sure `company-mode' is installed and configured.
;; 2. Add `company-tabnine' to `company-backends':
;;
;;   (add-to-list 'company-backends #'company-tabnine)
;;
;; 3. Run M-x company-tabnine-install-binary to install the TabNine binary for your system.
;;
;; Usage:
;;
;; `company-tabnine' should work out of the box.
;; See M-x customize-group RET company-tabnine RET for customizations.
;;
;; Recommended Configuration:
;;
;; - Trigger completion immediately.
;;
;;   (setq company-idle-delay 0)
;;
;; - Number the candidates (use M-1, M-2 etc to select completions).
;;
;;   (setq company-show-numbers t)
;;

;;; Code:

;;
;; Dependencies
;;

(require 'cl-lib)
(require 'company)
(require 'company-template)
(require 'dash)
(require 'json)
(require 's)
(require 'unicode-escape)
(require 'url)


;;
;; Constants
;;

(defconst company-tabnine--process-name "company-tabnine--process")
(defconst company-tabnine--buffer-name "*company-tabnine-log*")
(defconst company-tabnine--hooks-alist nil)
(defconst company-tabnine--protocol-version "1.0.14")

;; tmp file put in company-tabnine-binaries-folder directory
(defconst company-tabnine--version-tempfile "version")

;; current don't know how to use Prefetch and GetIdentifierRegex
(defconst company-tabnine--method-autocomplete "Autocomplete")
(defconst company-tabnine--method-prefetch "Prefetch")
(defconst company-tabnine--method-getidentifierregex "GetIdentifierRegex")

;;
;; Macros
;;

(defmacro company-tabnine-with-disabled (&rest body)
  "Run BODY with `company-tabnine' temporarily disabled.
Useful when binding keys to temporarily query other completion backends."
  `(let ((company-tabnine--disabled t))
     ,@body))

(defun company-tabnine--filename-completer-p (extra-info)
  "Check whether candidate's EXTRA-INFO indicates a filename completion."
  (-contains? '("[File]" "[Dir]" "[File&Dir]") extra-info))

(defun company-tabnine--identifier-completer-p (extra-info)
  "Check if candidate's EXTRA-INFO indicates a identifier completion."
  (s-equals? "[ID]" extra-info))

;;
;; Customization
;;

(defgroup company-tabnine nil
  "Options for company-tabnine."
  :link '(url-link :tag "Github" "https://github.com/TommyX12/company-tabnine")
  :group 'company
  :prefix "company-tabnine-")

(defcustom company-tabnine-max-num-results 10
  "Maximum number of results to show."
  :group 'company-tabnine
  :type 'integer)

(defcustom company-tabnine-context-radius 3000
  "The number of chars before point to send for completion.

Note that setting this too small will cause TabNine to not be able to read the entire license activation key."
  :group 'company-tabnine
  :type 'integer)

(defcustom company-tabnine-context-radius-after 1000
  "The number of chars after point to send for completion."
  :group 'company-tabnine
  :type 'integer)

(defcustom company-tabnine-max-restart-count 10
  "Maximum number of times TabNine can consecutively restart.
This may be due to errors in or automatic server updates.
Any successful completion will reset the consecutive count."
  :group 'company-tabnine
  :type 'integer)

(defcustom company-tabnine-always-trigger t
  "Whether to overload company's minimum prefix length.
This allows completion to trigger on as much as possible.
Default is t (strongly recommended)."
  :group 'company-tabnine
  :type 'boolean)

(defcustom company-tabnine-no-continue nil
  "Whether to make company reset idle timer on all keystrokes.
Only useful when `company-idle-delay' is not 0.
Doing so improves performance by reducing number of calls to the completer,
at the cost of less responsive completions."
  :group 'company-tabnine
  :type 'boolean)

(defcustom company-tabnine-binaries-folder "~/.TabNine"
  "Path to TabNine binaries folder.
`company-tabnine-install-binary' will use this directory."
  :group 'company-tabnine
  :type 'string)

(defcustom company-tabnine-install-static-binary (file-exists-p "/etc/nixos/hardware-configuration.nix")
  "Whether to install the musl-linked static binary instead of
the standard glibc-linked dynamic binary.
Only useful on GNU/Linux.  Automatically set if NixOS is detected."
  :group 'company-tabnine
  :type 'boolean)

(defcustom company-tabnine-log-file-path nil
  "If non-nil, next TabNine restart will write debug log to this path."
  :group 'company-tabnine
  :type 'string)

(defcustom company-tabnine-auto-balance t
  "Whether TabNine should insert balanced parentheses upon completion."
  :group 'company-tabnine
  :type 'boolean)

(defcustom company-tabnine-async t
  "Whether or not to use async operations to fetch data."
  :group 'company-tabnine
  :type 'boolean)

(defcustom company-tabnine-wait 0.25
  "Number of seconds to wait for TabNine to respond.
Only if company-tabnine-async is nil"
  :group 'company-tabnine
  :type 'float)

(defcustom company-tabnine-max-request-buffer 10
  "Maximum number of simultaneous requests at a time in async mode"
  :group 'company-tabnine
  :type 'integer)

(defcustom company-tabnine-show-annotation t
  "Whether to show an annotation inline with the candidate."
  :group 'company-tabnine
  :type 'boolean)

(defcustom company-tabnine-auto-fallback t
  "Whether to automatically fallback to other backends when TabNine has no candidates."
  :group 'company-tabnine
  :type 'boolean)

(defcustom company-tabnine-use-native-json t
  "Whether to use native JSON when possible."
  :group 'company-tabnine
  :type 'boolean)

(defcustom company-tabnine-insert-arguments t
  "When non-nil, insert function arguments as a template after completion.
Only supported by modes in `company-tabnine--extended-features-modes'"
  :group 'company-tabnine
  :type 'boolean)

(defcustom company-tabnine-show-user-message nil
  "Show user_message field in company-meta"
  :group 'company-tabnine
  :type 'boolean)

;;
;; Faces
;;

;;
;; Variables
;;

(defvar company-tabnine-executable-args nil
  "Arguments passed to TabNine.")

(defvar company-tabnine--process nil
  "TabNine server process.")

(defvar company-tabnine--restart-count 0
  "Number of times TabNine server has restarted abnormally.
Resets every time successful completion is returned.")

(defvar company-tabnine--request-point '()
  "A list store request cursor points.")

(defvar company-tabnine--response-cache '()
  "A list which contains a plist.
contains of '(
    :point \"1441\" ;; start point of completion
    :candidate \"prefix\" ;; suggestion from tabnine
    :old_suffix \"x\"
    :new_suffix \"\"
    :detail \"13%\"
    :type \"\"
    :user_message [])
")

(defvar company-tabnine--disabled nil
  "Variable to temporarily disable company-tabnine and pass control to next backend.")

(defvar company-tabnine--calling-continue nil
  "Flag for when `company-continue' is being called.")

(defvar company-tabnine--response-chunks nil
  "The string to store response chunks from TabNine server.")

;;
;; Major mode definition
;;

;;
;; Global methods
;;

(defun company-tabnine--prefix-candidate-p (candidate prefix)
  "Return t if CANDIDATE string begins with PREFIX."
  (let ((insertion-text (cdr (assq 'insertion_text candidate))))
    (s-starts-with? prefix insertion-text t)))

(defun company-tabnine--error-no-binaries ()
  "Signal error for when TabNine binary is not found."
  (error "No TabNine binaries found.  Run M-x company-tabnine-install-binary to download binaries"))

(defun company-tabnine--get-target ()
  "Return TabNine's system configuration.  Used for finding the correct binary."
  (let* ((system-architecture (car (s-split "-" system-configuration)))
         (tabnine-architecture
          (cond
           ((or (string= system-architecture "aarch64")
                (and (eq system-type 'darwin)
                     (string= system-architecture "x86_64")
                     ;; Detect AArch64 running x86_64 Emacs
                     (string= (shell-command-to-string "arch -arm64 uname -m") "arm64\n")))
            "aarch64")
           ((or (string= system-architecture "arm")
                (and (eq system-type 'darwin)
                     (string= system-architecture "x86_64")
                     ;; Detect AArch64 running x86_64 Emacs
                     (string= (shell-command-to-string "arch -arm64 uname -m") "arm64\n")))
            "aarch64")
           ((string= system-architecture "x86_64")
            "x86_64")
           ((string-match system-architecture "i.86")
            "i686")
           (t
            (error "Unknown or unsupported architecture %s" system-architecture))))

         (os
          (cond
           ((or (eq system-type 'ms-dos)
                (eq system-type 'windows-nt)
                (eq system-type 'cygwin))
            "pc-windows-gnu")
           ((or (eq system-type 'darwin))
            "apple-darwin")
           (company-tabnine-install-static-binary
            "unknown-linux-musl")
           (t
            "unknown-linux-gnu"))))

    (concat tabnine-architecture "-" os)))

(defun company-tabnine--get-exe ()
  "Return TabNine's binary file name.  Used for finding the correct binary."
  (cond
   ((or (eq system-type 'ms-dos)
        (eq system-type 'windows-nt)
        (eq system-type 'cygwin))
    "TabNine.exe")
   (t
    "TabNine")))

(defun company-tabnine--executable-path ()
  "Find and return the path of the latest TabNine binary for the current system."
  (let ((parent company-tabnine-binaries-folder))
    (if (file-directory-p parent)
        (let* ((children (->> (directory-files parent)
                              (--remove (member it '("." "..")))
                              (--filter (file-directory-p
                                         (expand-file-name
                                          it
                                          (file-name-as-directory
                                           parent))))
                              (--filter (ignore-errors (version-to-list it)))
                              (-non-nil)))
               (sorted (nreverse (sort children #'version<)))
               (target (company-tabnine--get-target))
               (filename (company-tabnine--get-exe)))
          (cl-loop
           for ver in sorted
           for fullpath = (expand-file-name (format "%s/%s/%s"
                                                    ver target filename)
                                            parent)
           if (and (file-exists-p fullpath)
                   (file-regular-p fullpath))
           return fullpath
           finally do (company-tabnine--error-no-binaries)))
      (company-tabnine--error-no-binaries))))

(defun company-tabnine-start-process ()
  "Start TabNine process."
  (company-tabnine-kill-process)
  (let ((process-connection-type nil))
    (setq company-tabnine--process
          (make-process
           :name company-tabnine--process-name
           :command (append
                     (cons (company-tabnine--executable-path)
                           (when company-tabnine-log-file-path
                             (list
                              "--log-file-path"
                              (expand-file-name
                               company-tabnine-log-file-path))))
                     (list "--client" "emacs")
                     company-tabnine-executable-args)
           :coding 'utf-8
           :buffer "*tabnine*"
           :connection-type 'pipe
           :filter #'company-tabnine--process-filter
           :sentinel #'company-tabnine--process-sentinel
           :noquery t)))
  ;; hook setup
  (message "TabNine server started.")
  (dolist (hook company-tabnine--hooks-alist)
    (add-hook (car hook) (cdr hook))))

(defun company-tabnine-send-request (request)
  "Send REQUEST to TabNine server.  REQUEST needs to be JSON-serializable object."
  (when (< (length company-tabnine--request-point) company-tabnine-max-request-buffer)
    (push (point) company-tabnine--request-point)
    (when (null company-tabnine--process)
      (company-tabnine-start-process))
    (when company-tabnine--process
      ;; TODO make sure utf-8 encoding works
      (let ((encoded (concat
                      (if (and company-tabnine-use-native-json
                               (fboundp 'json-serialize))
                          (json-serialize request
                                          :null-object nil
                                          :false-object json-false)
                        (let ((json-null nil)
                              (json-encoding-pretty-print nil))
                          (json-encode-list request)))
                      "\n")))
        (process-send-string company-tabnine--process encoded)
        (unless company-tabnine-async
          (accept-process-output company-tabnine--process company-tabnine-wait))))))

(defun company-tabnine--make-request (method)
  "Create request body for method METHOD and parameters PARAMS."
  (cond
   ((eq method 'autocomplete)
    (let* ((buffer-min 1)
           (buffer-max (1+ (buffer-size)))
           (before-point
            (max (point-min) (- (point) company-tabnine-context-radius)))
           (after-point
            (min (point-max) (+ (point) company-tabnine-context-radius-after))))

      (list
       :version company-tabnine--protocol-version
       :request
       (list :Autocomplete
             (list
              :before (buffer-substring-no-properties before-point (point))
              :after (buffer-substring-no-properties (point) after-point)
              :filename (or (buffer-file-name) nil)
              :region_includes_beginning (if (= before-point buffer-min)
                                             t json-false)
              :region_includes_end (if (= after-point buffer-max)
                                       t json-false)
              :max_num_results company-tabnine-max-num-results)))))

   ((eq method 'prefetch)
    (list
     :version company-tabnine--protocol-version
     :request
     (list :Prefetch
           (list
            :filename (or (buffer-file-name) nil)
            ))))
   ((eq method 'getidentifierregex)
    (list
     :version company-tabnine--protocol-version
     :request
     (list :GetIdentifierRegex
           (list
            :filename (or (buffer-file-name) nil)
            ))))))

(defun company-tabnine-query ()
  "Query TabNine server for auto-complete."
  (let ((request (company-tabnine--make-request 'autocomplete)))
    (company-tabnine-send-request request)))

(defun company-tabnine--decode (msg)
  "Decode TabNine server response MSG, and return the decoded object."
  (let ((json-parser (if (fboundp 'json-parse-string)
                         (lambda (data) (json-parse-string data :object-type 'alist))
                       'json-read-from-string)))
    (mapcar 'company-tabnine--store-response-to-cache
            (mapcar json-parser (butlast (split-string msg "\n"))))))

(defun company-tabnine--store-response-to-cache (response)
  "Store the response from server and format to cache."
  (setq company-tabnine--response-cache
        (append company-tabnine--response-cache
                (mapcar (lambda (result)
                          (list
                           :point (- (car company-tabnine--request-point)
                                     (length (alist-get 'old_prefix response)))
                           :candidate (alist-get 'new_prefix result)
                           :old_suffix (alist-get 'old_suffix result)
                           :new_suffix (alist-get 'new_suffix result)
                           :detail (or (alist-get 'detail result) "")
                           :type (or (alist-get 'type result) "")
                           :user_message (alist-get 'user_message result)))
                        (alist-get 'results response))))
  (setq company-tabnine--request-point (cdr company-tabnine--request-point)))

(defun company-tabnine--process-sentinel (process event)
  "Sentinel for TabNine server process.
PROCESS is the process under watch, EVENT is the event occurred."
  (when (and company-tabnine--process
             (memq (process-status process) '(exit signal)))

    (message "TabNine process %s received event %s."
             (prin1-to-string process)
             (prin1-to-string event))

    (setq company-tabnine--request-point nil)
    (setq company-tabnine--current-request-count 0)

    (if (>= company-tabnine--restart-count
            company-tabnine-max-restart-count)
        (progn
          (message "TabNine process restart limit reached.")
          (setq company-tabnine--process nil))

      (message "Restarting TabNine process.")
      (company-tabnine-start-process)
      (setq company-tabnine--restart-count
            (1+ company-tabnine--restart-count)))))

(defun company-tabnine--process-filter (process output)
  "Filter for TabNine server process.
PROCESS is the process under watch, OUTPUT is the output received."
  (push output company-tabnine--response-chunks)
  (when (s-ends-with-p "\n" output)
    (let ((response
           (mapconcat #'identity
                      (nreverse company-tabnine--response-chunks)
                      nil)))
      (setq company-tabnine--response-chunks nil)
      (company-tabnine--decode response))))

(defun company-tabnine--read-cache-prefix ()
  "Read cached result which has the same prefix, delete outdated cache, and return prefix."
  (setq company-tabnine--response-cache
        (mapcar (lambda (cache) (when (>= (- (point) (plist-get cache :point)) 0)
                                  (let ((prefix (buffer-substring-no-properties (plist-get cache :point) (point))))
                                    (when (string-prefix-p prefix (plist-get cache :candidate))
                                      cache)))) company-tabnine--response-cache))
  (setq company-tabnine--response-cache (remove nil company-tabnine--response-cache))
  (setq company-tabnine--response-cache
        (seq-uniq company-tabnine--response-cache '(lambda (left right)
                                                     (string= (plist-get left :candidate) (plist-get right :candidate)))))
  (unless (= (length company-tabnine--response-cache) 0)
    `(,(buffer-substring-no-properties
        (plist-get (car company-tabnine--response-cache) :point)
        (point)) . t)))

(defun company-tabnine--prefix ()
  "Prefix-command handler for the company backend."
  (if (or (and company-tabnine-no-continue
               company-tabnine--calling-continue)
          company-tabnine--disabled)
      nil
    (company-tabnine-query)
    (company-tabnine--read-cache-prefix)))

(defun company-tabnine--annotation(candidate)
  "Fetch the annotation text-property from a CANDIDATE string."
  (when company-tabnine-show-annotation
    (-if-let (annotation (get-text-property 0 'annotation candidate))
        annotation
      (let ((kind (get-text-property 0 'kind candidate))
            ;; (return-type (get-text-property 0 'return_type candidate))
            (params (get-text-property 0 'params candidate)))
        (when kind
          (concat params
                  ;; (when (s-present? return-type)
                  ;;   (s-prepend " -> " return-type))
                  (when (s-present? kind)
                    (format " [%s]" kind))))))))

(defun company-tabnine--kind-to-type (kind)
  (pcase kind
    (1 "Text")
    (2 "Method")
    (3 "Function")
    (4 "Constructor")
    (5 "Field")
    (6 "Variable")
    (7 "Class")
    (8 "Interface")
    (9 "Module")
    (10 "Property" )
    (11 "Unit" )
    (12 "Value" )
    (13 "Enum")
    (14 "Keyword" )
    (15 "Snippet")
    (16 "Color")
    (17 "File")
    (18 "Reference")
    (19 "Folder")
    (20 "EnumMember")
    (21 "Constant")
    (22 "Struct")
    (23 "Event")
    (24 "Operator")
    (25 "TypeParameter")))

(defun company-tabnine--make-candidates (results)
  "Accept the cache list and return candidates."
  (mapcar (lambda (result)
            (propertize
             (plist-get result :candidate)
             'annotation (concat (plist-get result :detail) " " (plist-get result :type))
             'new_suffix (plist-get result :new_suffix)
             'old_suffix (plist-get result :old_suffix)
             'meta (plist-get result :meta)
             'user_message (plist-get result :user_message))) results))

(defun company-tabnine--candidates (prefix)
  "Candidates-command handler for the company backend for PREFIX.

Return completion candidates.  Must be called after `company-tabnine-query'."
  (company-tabnine--make-candidates company-tabnine--response-cache))

(defun company-tabnine--meta (candidate)
  "Return meta information for CANDIDATE.  Currently used to display user messages."
  (when company-tabnine-show-user-message
    (let ((messages (get-text-property 0 'user_message candidate)))
      (when messages
        (s-join " " messages)))))

(defun company-tabnine--post-completion (candidate)
  "Replace old suffix with new suffix for CANDIDATE."
  (setq company-tabnine--response-cache nil)
  (when company-tabnine-auto-balance
    (let ((old_suffix (get-text-property 0 'old_suffix candidate))
          (new_suffix (get-text-property 0 'new_suffix candidate)))
      (delete-region (point)
                     (min (+ (point) (length old_suffix))
                          (point-max)))
      (when (stringp new_suffix)
        (save-excursion
          (insert new_suffix))))))

;;
;; Interactive functions
;;

(defun company-tabnine-kill-process ()
  "Kill TabNine process."
  (interactive)
  (when company-tabnine--process
    (let ((process company-tabnine--process))
      (setq company-tabnine--process nil) ; this happens first so sentinel don't catch the kill
      (delete-process process)))
  ;; hook remove
  (dolist (hook company-tabnine--hooks-alist)
    (remove-hook (car hook) (cdr hook))))

(defun company-tabnine-restart-server ()
  "Start/Restart TabNine server."
  (interactive)
  (company-tabnine-start-process))

(defun company-tabnine-install-binary ()
  "Install TabNine binary into `company-tabnine-binaries-folder'."
  (interactive)
  (let ((version-tempfile (concat
                           (file-name-as-directory
                            company-tabnine-binaries-folder)
                           company-tabnine--version-tempfile))
        (target (company-tabnine--get-target))
        (exe (company-tabnine--get-exe))
        (binaries-dir company-tabnine-binaries-folder))
    (message version-tempfile)
    (message "Getting current version...")
    (make-directory (file-name-directory version-tempfile) t)
    (url-copy-file "https://update.tabnine.com/bundles/version" version-tempfile t)
    (let ((version (s-trim (with-temp-buffer (insert-file-contents version-tempfile) (buffer-string)))))
      (when (= (length version) 0)
        (error "TabNine installation failed.  Please try again"))
      (message "Current version is %s" version)
      (let* ((url (concat "https://update.tabnine.com/bundles/" version "/" target "/TabNine.zip"))
             (version-directory (file-name-as-directory
                                 (concat
                                  (file-name-as-directory
                                   (concat (file-name-as-directory binaries-dir) version)))))
             (target-directory (file-name-as-directory (concat version-directory target) ))
             (bundle-path (concat version-directory (format "%s.zip" target)))
             (target-path (concat target-directory exe)))
        (message "Installing at %s. Downloading %s ..." target-path url)
        (make-directory target-directory t)
        (url-copy-file url bundle-path t)
        (condition-case ex
            (let ((default-directory target-directory))
              (if (or (eq system-type 'ms-dos)
                      (eq system-type 'windows-nt)
                      (eq system-type 'cygwin))
                  (shell-command (format "tar -xf %s" (expand-file-name bundle-path)))
                (shell-command (format "unzip -o %s -d %s"
                                       (expand-file-name bundle-path)
                                       (expand-file-name target-directory)))))
          ('error
           (error "Unable to unzip automatically. Please go to [%s] and unzip the content of [%s] into [%s/]."
                  (expand-file-name version-directory)
                  (file-name-nondirectory bundle-path)
                  (file-name-sans-extension (file-name-nondirectory bundle-path)))))
        (mapc (lambda (filename)
                (set-file-modes (concat target-directory filename) (string-to-number "744" 8)))
              (--remove (member it '("." "..")) (directory-files target-directory)))
        (delete-file bundle-path)
        (delete-file version-tempfile)
        (message "TabNine installation complete.")))))

(defun company-tabnine-call-other-backends ()
  "Invoke company completion but disable TabNine once, passing query to other backends in `company-backends'.

This is actually obsolete, since `company-other-backend' does the same."
  (interactive)
  (company-tabnine-with-disabled
   (company-abort)
   (company-auto-begin)))

;;;###autoload
(defun company-tabnine (command &optional arg &rest ignored)
  "`company-mode' backend for TabNine.

See documentation of `company-backends' for details."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-tabnine))
    (prefix (company-tabnine--prefix))
    (candidates (company-tabnine--candidates arg))
    ;; TODO: should we use async or not?
    ;; '(:async . (lambda (callback)
    ;;              (funcall callback (company-tabnine--candidates) arg))))
    (meta (company-tabnine--meta arg))
    (annotation (company-tabnine--annotation arg))
    (post-completion (company-tabnine--post-completion arg))
    (no-cache t)
    (sorted t)))

;;
;; Advices
;;

(defun company-tabnine--continue-advice (func &rest args)
  "Advice for `company--continue'."
  (let ((company-tabnine--calling-continue t))
    (apply func args)))

(advice-add #'company--continue :around #'company-tabnine--continue-advice)

(defun company-tabnine--insert-candidate-advice (func &rest args)
  "Advice for `company--insert-candidate'."
  (if company-tabnine-auto-balance
      (let ((smartparens-mode nil))
        (apply func args))
    (apply func args)))

;; `smartparens' will add an advice on `company--insert-candidate' in order to
;; add closing parenthesis.
;; If TabNine takes care of parentheses, we disable smartparens temporarily.
(eval-after-load 'smartparens
  '(advice-add #'company--insert-candidate
               :around #'company-tabnine--insert-candidate-advice))

;;
;; Hooks
;;


(provide 'company-tabnine)

;;; company-tabnine.el ends here
