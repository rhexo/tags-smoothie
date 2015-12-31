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
(defvar tags-smoothie-list)
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

;; get list of competed paths of include entries
(defun tags-smoothie-source-get-complete-path (line)
  "Get completed file path of #include argument."
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

;; (include-source-get-complete-path "#include    <boost/mpl/contains.hpp>")


;; parse sources lines. Stays only substrings of #include derictive
(mapcar 'tags-smoothie-source-get-complete-path (tags-smoothie-read-lines src-file-path))




