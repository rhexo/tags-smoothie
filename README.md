# tags-smoothie
Self recursion tags generation agorithm based on exctags

# requirments
 - FreeBSD 10.2
 - projectile (see https://github.com/bbatsov/projectile.git)

# configuration
Need to be added in .emacs

```elisp
(setq projectile-tags-command "/usr/local/bin/exctags -Re -f \"%s\" --language-force=c++ %s %s")

(add-to-list 'load-path "/home/rhexo/.emacs.d/tags-smoothie")
(require 'tags-smoothie)

;; sets search directories for system includes (see defaults in tags-smoothie.el)
(setq tags-smoothie-search-path
      '("/usr/include/"                    ;; system/include support
        "/usr/local/include/"              ;; local/include support
        "/usr/include/c++/v1/"))           ;; c++ support

;; project directories that will be excluded from overview (see defaults in tags-smoothie.el)
(setq tags-smoothie-cpp-dir-to-exclude '("/build" "/bin"))

;; redefinition of projectile regeneration tags procedure
(defun projectile-regenerate-tags ()
  "Regenerate the project's [e|g]tags. Override function"
  (interactive)
  (let* ((project-root (projectile-project-root))
         (tags-exclude (projectile-tags-exclude-patterns))
         (default-directory project-root)
         (tags-file (expand-file-name projectile-tags-file-name))
         (tags-includes (tags-smoothie-get-files-stream project-root))
         (command (format projectile-tags-command tags-file tags-includes tags-exclude))
         shell-output exit-code)
    (with-temp-buffer
      (setq exit-code
            (call-process-shell-command command nil (current-buffer))
            shell-output (projectile-trim-string
                          (buffer-substring (point-min) (point-max)))))
    (unless (zerop exit-code)
      (error shell-output))
    (visit-tags-table tags-file)))
```
--language-force=c++ needed for compiling includes like string or map, i.e. includes without file extension

If you are using tags-smoothie for non c/c++ projects you have to omitted this option.

