;;; tags-smoothie.el --- number of helper functions definition for emacs to produce tags assistants in c/c++ projects 

;; Copyright © 2015  Maxim Musolov <mmusolov@gmail.com>

;; Author: Maxim Musolov <mmusolov@gmail.com>
;; URL: https://github.com/rhexo/tags-smoothie
;; Keywords: tags, completion, assistant
;; Version: 0.0.0
;; Package-Requires: 

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; 
;; This mode implements a number of helper functions that organize :
;; 1. traversal of the tree of includes declarations in c/c++ files
;; 2. get list of files that coresponds to includes of tree traversal algorithm.
;;    Using a variable to define system include locations. 
;;    Using this variable and relative definitions of #include directive to existance check
;; 3. Unify entries of the list and return this as result
;; 4. generate etags file using project path as argument of this function
;;
;;; Code:


;; (defvar tags-smoothie-project-dir (nil)
;;   "TAGS file dir as project dir")

;; (defun tags-smoothie-project-traversal (string)
;;   "Traversal sources tree. Parse project sources and gets it`'s includes as list."
;;   (setq tags-smoothie-project-dir 'string))


;; (defun tags-smoothie-parse-file (string)
;;   "Get list of #include declarations in file."
;;   (""))

;; (defun tags-smoothie-get-project-files ()
;;   "Retrieve c/c++ project files."
;;   )

;; ;;; Customization
;; (defgroup tags-smoothie nil
;;   "etags generation algorithm."
;;   :group 'tools)

;; (defcustom tags-smoothie-include-path "/usr/include/"
;;   "external and system include paths that uses in c/c++ project files."
;;   :group 'tags-smoothie
;;   :type '(repeat string))

(require 'dash)

;; helper function (print)
(defun print-elements-recursively (list)
  "Print each element of LIST on a line of its own. Uses recursion."
  (when list                     ; do-again-test
    (print (car list))           ; body
    (print-elements-recursively  ; recursive call
     (cdr list))))               ; next-step-expression

;; set path to parsed file (helper definition)
(setq src-file-path  "/usr/home/rhexo/ydisk/proj/app-lib/MPL/mpl__views_and_iterator_adapters_1/include/view_example.hpp")


;; List on files to tags
(defvar tags-smoothie-include-list)
;; initialize
(setq tags-smoothie-include-list nil)

;; Read file as list of lines
(defun tags-smoothie-read-lines (filePath)
  "Return list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

;; Get #include argument
(defun tags-smoothie-source-get-path (line)
  "Get #include content from file-buffer. nil if line is not a directive"
  (if (string-match "#include[ \t]+" line)
      (let ((result ""))
        (setq result 
              (replace-regexp-in-string "[<>\"]" "" ;; remove <|>|" symbols from include argument 
                                        (replace-regexp-in-string "#include[ \t]+" "" line))) ;; remove #include prefix
        result)))

;; complete list entries with system include paths. Before this declare variable : include-search-path
(setq tags-smoothie-search-path
      '("/usr/include/"                    ;; system/include support
        "/usr/local/include/"              ;; local/include support
        "/usr/include/c++/v1/"))           ;; c++ support

;; get completed path for include entrie
(defun tags-smoothie-source-get-complete-path (line)
  "Get completed file path for #include argument."
  (let ((include-path "")
        (file-path ""))
    ;; Get argument of include definition
    (setq include-path (tags-smoothie-source-get-path line))    
    (if (> (length include-path) 0)
        ;; argument is not nil. Try to complete file path
        (dolist (item tags-smoothie-search-path file-path)
          (if (file-exists-p (concat item include-path))
              ;; file existence check -> true
              (setq file-path (concat item include-path)))
          ))
    ;; return as result
    file-path))

;; get list of sorce include files.
(defun tags-smoothie-source-get-complete-list (src-file-path)
  "Get list of source includes represented as full path strings."
  (let ((lines-list (tags-smoothie-read-lines src-file-path))
        (result-list nil)
        (item nil))    
    (while lines-list
      (setq item (tags-smoothie-source-get-complete-path (car lines-list)))
      (if (not (eq item ""))
          (push item result-list))
      (setq lines-list (cdr lines-list)))
    result-list))

;; parse sources lines. Stay only substrings of #include derictive
(message "list: %s" (tags-smoothie-source-get-complete-list src-file-path))
(message "lines: %s" (tags-smoothie-read-lines src-file-path))

;; get directories
(defun tags-smoothie-fs-get-dir-list (path exclude-dir)
  "Get list of subdirecotries"
  (let ((dir-list nil))
    ;; get only non hiden directories and files
    (dolist (item (directory-files path t "^[a-zA-Z_].*$") dir-list)
      ;; stays only directories
      (if (string= (car (file-attributes item)) "t") 
          ;; check in exclude list
          (if (eq (-contains? exclude-dir item) nil)
              (push item dir-list))))
    ;; return as result
    dir-list))

(setq test-exclude-list '("/usr/home/rhexo/ydisk/proj/app-lib/MPL/mpl__views_and_iterator_adapters_1/bin"
                          "/usr/home/rhexo/ydisk/proj/app-lib/MPL/mpl__views_and_iterator_adapters_1/build"))

(setq test-project-dir "/usr/home/rhexo/ydisk/proj/app-lib/MPL/mpl__views_and_iterator_adapters_1")

(print-elements-recursively (tags-smoothie-fs-get-dir-list test-project-dir test-exclude-list))
;; OUtput
;; "/usr/home/rhexo/ydisk/proj/app-lib/MPL/mpl__views_and_iterator_adapters_1/include"

;; (print-elements-recursively (tags-smoothie-fs-get-dir-list test-project-dir nil))
;; Output
;; "/usr/home/rhexo/ydisk/proj/app-lib/MPL/mpl__views_and_iterator_adapters_1/include"
;; "/usr/home/rhexo/ydisk/proj/app-lib/MPL/mpl__views_and_iterator_adapters_1/build"
;; "/usr/home/rhexo/ydisk/proj/app-lib/MPL/mpl__views_and_iterator_adapters_1/bin"

(setq tags-smoothie-cpp-files-regexp "\\(\\.cxx$\\)\\|\\(\\.cpp$\\)\\|\\(\\.cc$\\)\\|\\(\\.hpp$\\)\\|\\(\\.hh$\\)\\|\\(\\.hxx$\\)\\|\\(\\.h$\\)")

(setq tags-smoothie-cpp-dir-to-exclude '("/build" "/bin"))

;; get sources
(defun tags-smoothie-fs-get-sources-list (path filter)
  "Get list of sources"
  (let ((sources-list nil))
    ;; get only non hiden directories and files
    (dolist (item (directory-files path t filter) sources-list)
      ;; stays only files
      (if (string= (car (file-attributes item)) "nil") 
          (push item sources-list)))
    ;; return as result
    sources-list))

(print-elements-recursively (tags-smoothie-fs-get-sources-list test-project-dir tags-smoothie-cpp-files-regexp))

;; (tags-smoothie-fs-get-dir-list test-project-dir (mapcar (lambda (elem) (concat test-project-dir elem)) tags-smoothie-cpp-dir-to-exclude))
;; (mapcar (lambda (elem) (concat test-project-dir elem)) tags-smoothie-cpp-dir-to-exclude)

;; get list of project sources
(defun tags-smoothie-fs-get-project-sources (path exclude-dir)
  "Building list from project sources. Using recursion for that."
  (let (
        ;; init 
        (dir-list (tags-smoothie-fs-get-dir-list path (mapcar (lambda (elem) (concat path elem)) tags-smoothie-cpp-dir-to-exclude)))
        (src-list (tags-smoothie-fs-get-sources-list path tags-smoothie-cpp-files-regexp)))
    ;; loop dir-list and add src-list to glob-sources-list by using recursion 
    (while dir-list
      ;; create new list
      (setq src-list (append src-list (tags-smoothie-fs-get-project-sources (car dir-list) exclude-dir)))
      (setq dir-list (cdr dir-list)))
    ;; return as result
    src-list
    ))

(setq tags-smoothie-include-list nil)
(tags-smoothie-fs-get-project-sources test-project-dir tags-smoothie-cpp-dir-to-exclude)

;; process step
(defun tags-smoothie-process-step (src-list proc-list)
  "Algorithm:
1. loop at sources list. 
2. parse include directive
3. get list of directive parameters and complete it
4. exclude processed entries
5. run recursion step "
  (let ((current-list nil)) 
    (if (not (null src-list))
        (progn
          (while src-list
            (if (eq (-contains? proc-list (car src-list)) nil)
                (progn
                  ;; entry dosen`t process early
                  (push (car src-list) proc-list)
                  ;; parse include directives
                  (let ((parse-list (tags-smoothie-source-get-complete-list (car src-list)))
                        (new-list nil)
                        (result-list nil))
                    ;; exclude processed entries
                    (while parse-list
                      (if (not (-contains? proc-list (car parse-list)))
                          (push (car parse-list) new-list))
                      (setq parse-list (cdr parse-list)))
                    ;; call recursion step
                    (if (not (null new-list))
                        (progn 
                          (setq result-list (tags-smoothie-process-step new-list proc-list))
                          (if (not (null result-list))
                              (while result-list
                                (if (not (-contains? proc-list (car result-list)))
                                    (push (car result-list) proc-list))                                    
                                (setq result-list (cdr result-list))))))
                    (setq current-list proc-list))))
            (setq src-list (cdr src-list)))))
    ;; return as result
    current-list))

;; fill tags-smoothie-list
(defun tags-smoothie-get-list (dev-dir)
  "Parse project sources and build list of system includes that using in project. Includes represents as full path strings"
  (let ((proj-list (tags-smoothie-fs-get-project-sources dev-dir tags-smoothie-cpp-dir-to-exclude))
        (include-list nil)
        (proc-list nil))
    (setq include-list (tags-smoothie-process-step proj-list proc-list))
    ;; return as result
    include-list))

;;(print-elements-recursively (tags-smoothie-get-list test-project-dir))
(message "result: %s" (tags-smoothie-get-list test-project-dir))




