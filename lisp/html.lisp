; Functions for writing html

(defun html-tag (stream tag attrs &optional contents)
  "Emits the specified tag to the given stream"
  (format stream "<~A~{~A=\"~A\"~^ ~}~A>"
	  tag
	  attrs
	  (if contents "" "/"))
  (when contents
    (format stream "~A</~A>"
	    contents
	    tag)))
