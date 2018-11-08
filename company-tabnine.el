;;; company-tabnine.el --- A company-mode backend for TabNine, the all-language autocompleter: https://tabnine.com/
;;
;; Copyright (c) 2018 Tommy Xiang
;;
;; Author: Tommy Xiang <tommyx058@gmail.com>
;; Keywords: convenience
;; Version: 0.0.1
;; URL: https://github.com/TommyX12/company-tabnine/
;; Package-Requires: ((emacs "24.3") (company "0.9.3") (cl-lib "0.5") json unicode-escape s)
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
;; Usage:
;; 
;; `company-tabnine' should work out of the box.
;; See M-x customize-group RET company-tabnine RET for customizations.
;;

;;; Code:

;;
;; Dependencies
;;

(require 'cl-lib)
(require 'company)
(require 'json)
(require 's)
(require 'unicode-escape)

;;
;; Constants
;;

(defconst company-tabnine--process-name "company-tabnine--process")
(defconst company-tabnine--buffer-name "*company-tabnine-log*")
(defconst company-tabnine--hooks-alist nil)
(defconst company-tabnine--protocol-version "0.11.1")

;;
;; Macros
;;

;;
;; Customization
;;

(defgroup company-tabnine nil
  "Options for company-tabnine."
  :prefix "company-tabnine-")

(defcustom company-tabnine-max-num-results 10
  "Maximum number of results to show."
  :group 'company-tabnine
  :type 'integer)

(defcustom company-tabnine-context-radius 2000
  "The number of chars before and after point to send for completion.
For example, setting this to 2000 will send 4000 chars in total per query.
It is not recommended to change this."
  :group 'company-tabnine
  :type 'integer)

(defcustom company-tabnine-max-restart-count 10
  "Maximum number of times TabNine can consecutively restart due to errors or updates.
Any successful completion will reset the consecutive count."
  :group 'company-tabnine
  :type 'integer)

(defcustom company-tabnine-wait 0.25
  "Number of seconds to wait for TabNine to respond."
  :group 'company-tabnine
  :type 'float)

(defcustom company-tabnine-always-trigger t
  "Whether to overload company's minimum prefix length to trigger on as many keystrokes as possible.
Default is t (strongly recommended)."
  :group 'company-tabnine
  :type 'boolean)

;;
;; Faces
;;

;;
;; Variables
;;

(defvar company-tabnine-binaries-folder
  (expand-file-name
   "binaries"
   (file-name-directory load-file-name))
  "Path to TabNine binaries folder.")

(defvar company-tabnine-executable-args nil
  "Arguments passed to TabNine.")

(defvar company-tabnine--process nil
  "TabNine server process.")

(defvar company-tabnine--restart-count 0
  "Number of times TabNine server has restarted abnormally.
Resets every time successful completion is returned.")

(defvar company-tabnine--result nil
  "Temporarily stored TabNine server responses.")

;;
;; Major mode definition
;;

;;
;; Global methods
;;

(defun company-tabnine--error-no-binaries ()
  "Signal error for when TabNine binary is not found."
  (error "No TabNine binaries found.  Run fetch-binaries.sh to download binaries"))

(defun company-tabnine--executable-path ()
  "Find and return the path of the latest TabNine binary for the current system."
  (if (file-directory-p company-tabnine-binaries-folder)
      (let* (children version architecture os file-name)

        ;; get latest version
        (setq children
              (cl-remove-if-not
               (lambda (child)
                 (file-directory-p (concat (file-name-as-directory
                                            company-tabnine-binaries-folder)
                                           child)))
               (directory-files company-tabnine-binaries-folder)))
        (setq children
              (mapcar
               (lambda (child)
                 (let ((vers (s-split "\\." child t)))
                   (if (= (length vers) 3)
                       (cons (mapcar 'string-to-number vers)
                             child) ; ((major minor patch) . original-name)
                     nil)))
               children))
        (setq children
              (cl-remove-if
               (lambda (child)
                 (null child))
               children))
        (setq children
              (sort
               children
               (lambda (child1 child2)
                 (letrec ((comp (lambda (ver1 ver2)
                                  (cond
                                   ((null ver1) ; which means (null ver2)
                                    t)
                                   ((< (car ver1) (car ver2))
                                    t)
                                   ((= (car ver1) (car ver2))
                                    (comp (cdr ver1) (cdr ver2)))))))
                   (funcall comp (car child1) (car child2))))))
        (setq version (cdr (car children)))
        (when (null version)
          (company-tabnine--error-no-binaries))

        ;; get system architecture
        (setq architecture
              (cond
               ((string= (s-left 6 system-configuration) "x86_64")
                "x86_64")
               (t
                "i686")))

        ;; get system type
        (setq os
              (cond
               ((or (eq system-type 'ms-dos)
                    (eq system-type 'windows-nt)
                    (eq system-type 'cygwin))
                "pc-windows-gnu")
               ((or (eq system-type 'darwin))
                "apple-darwin")
               (t
                "unknown-linux-gnu")))

        ;; get file name
        (setq file-name
              (cond
               ((or (eq system-type 'ms-dos)
                    (eq system-type 'windows-nt)
                    (eq system-type 'cygwin))
                "TabNine.exe")
               (t
                "TabNine")))

        ;; get final executable
        (let ((executable
               (expand-file-name
                (concat version "/"
                        architecture "-" os "/"
                        file-name)
                company-tabnine-binaries-folder)))
          (if (and (file-exists-p executable)
                   (file-regular-p executable))
              executable
            (company-tabnine--error-no-binaries))))

    (company-tabnine--error-no-binaries)))

(defun company-tabnine-start-process ()
	"Start TabNine process."
  (message "starting")
	(company-tabnine-kill-process)
	(let ((process-connection-type nil))
		(setq company-tabnine--process
					(make-process
					 :name company-tabnine--process-name
					 :command (cons
										 (company-tabnine--executable-path)
										 company-tabnine-executable-args)
					 :coding 'no-conversion
					 :connection-type 'pipe
					 :filter #'company-tabnine--process-filter
           :sentinel #'company-tabnine--process-sentinel
           :noquery t)))
	; hook setup
	(dolist (hook company-tabnine--hooks-alist)
		(add-hook (car hook) (cdr hook))))

(defun company-tabnine-kill-process ()
	"Kill TabNine process."
	(when company-tabnine--process
    (let ((process company-tabnine--process))
      (setq company-tabnine--process nil) ; this happens first so sentinel don't catch the kill
		  (delete-process process)))
  ; hook remove
	(dolist (hook company-tabnine--hooks-alist)
		(remove-hook (car hook) (cdr hook))))

(defun company-tabnine-send-request (request)
	"Send REQUEST to TabNine server.  REQUEST needs to be JSON-serializable object."
	(when (null company-tabnine--process)
    (company-tabnine-start-process))
	(when company-tabnine--process
    (let ((json-null nil)
				  (json-encoding-pretty-print nil)
				  (encoded (concat (unicode-escape* (json-encode-plist request)) "\n")))
      (setq company-tabnine--result nil)
		  (process-send-string company-tabnine--process encoded)
      (accept-process-output company-tabnine--process company-tabnine-wait))))

(defun company-tabnine-query ()
	"Query TabNine server for auto-complete."
	(let* ((point-min 1)
         (point-max (1+ (buffer-size)))
         (before-point
          (max point-min (- (point) company-tabnine-context-radius)))
         (after-point
          (min point-max (+ (point) company-tabnine-context-radius))))

    (company-tabnine-send-request
	   (list
		  :version company-tabnine--protocol-version :request
      (list :Autocomplete
            (list
             :before (buffer-substring-no-properties before-point (point))
             :after (buffer-substring-no-properties (point) after-point)
             :filename (or (buffer-file-name) nil)
             :region_includes_beginning (if (= before-point point-min)
                                            t json-false)
             :region_includes_end (if (= before-point point-min)
                                      t json-false)
             :max_num_results company-tabnine-max-num-results))))))

(defun company-tabnine--decode (msg)
  "Decode TabNine server response MSG, and return the decoded object."
  (let ((json-array-type 'list))
    (json-read-from-string msg)))

(defun company-tabnine--process-sentinel (process event)
  "Sentinel for TabNine server process."
  (when (and company-tabnine--process
             (memq (process-status process) '(exit signal)))

    (if (>= company-tabnine--restart-count
            company-tabnine-max-restart-count)
        (setq company-tabnine--process nil)

      (message "TabNine process restarted.")
      (company-tabnine-start-process)
      (setq company-tabnine--restart-count
            (1+ company-tabnine--restart-count)))))

(defun company-tabnine--process-filter (process output)
  "Filter for TabNine server process."
	(setq output (s-split "\n" output t))
	(setq company-tabnine--result
        (company-tabnine--decode (car (last output)))))

(defun company-tabnine--prefix ()
  "Return completion prefix.  Must be called after `company-tabnine-query'."
  (if (null company-tabnine--result)
      nil
    (alist-get 'suffix_to_substitute company-tabnine--result)))

(defun company-tabnine--candidates ()
  "Return completion candidates.  Must be called after `company-tabnine-query'."
  (if (null company-tabnine--result)
      nil
    (let ((results (alist-get 'results company-tabnine--result)))
      (setq results
            (mapcar
             (lambda (entry)
               (let ((result (alist-get 'result entry))
                     (suffix (alist-get 'prefix_to_substitute entry)))
                 (substring result 0 (- (length result) (length suffix)))))
             results))
      (when (> (length results) 0)
        (setq company-tabnine--restart-count 0))
      results)))

(defun company-tabnine--meta (candidate)
  "Return meta information for CANDIDATE.  Currently used to display promotional messages."
  (if (null company-tabnine--result)
      nil
    (when-let ((messages (alist-get 'promotional_message company-tabnine--result)))
      (s-join " " messages))))

;;
;; Interactive functions
;;

(defun company-tabnine-restart-server ()
  "Start/Restart TabNine server."
  (company-tabnine-start-process))

(defun company-tabnine (command &optional arg &rest ignored)
  "`company-mode' backend for TabNine.  See documentation of `company-backends' for details."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-tabnine))
    (prefix
     (company-tabnine-query)
     (if company-tabnine-always-trigger
         (cons (company-tabnine--prefix) t)
       (company-tabnine--prefix)))
    (candidates
     '(:async . (lambda (callback)
                  (funcall callback (company-tabnine--candidates)))))
    (meta
     (company-tabnine--meta arg))

		(no-cache t)
		(sorted t)))

;;
;; Advices
;;

;;
;; Hooks
;;



(provide 'company-tabnine)
;;; company-tabline.el ends here
